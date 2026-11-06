#!/usr/bin/env python3
"""Exercise the real library against a private fixture-only Hyprland IPC server."""
import copy
import json
import os
import resource
from pathlib import Path
import shutil
import socket
import subprocess
import sys
import tempfile
import threading
import time

resource.setrlimit(resource.RLIMIT_CORE, (0, 0))
library, gir, output_root, mode = sys.argv[1:]
runtime = Path(tempfile.mkdtemp(prefix="ipc-", dir=output_root))
os.environ["XDG_RUNTIME_DIR"] = str(runtime)
os.environ["HYPRLAND_INSTANCE_SIGNATURE"] = "fixture"
os.environ["GI_TYPELIB_PATH"] = str(runtime)
os.environ["G_DEBUG"] = "fatal-warnings"
os.environ["PYTHONDONTWRITEBYTECODE"] = "1"
subprocess.run(["g-ir-compiler", "--shared-library=" + library, "--output=" +
                str(runtime / "AstalHyprland-0.1.typelib"), gir], check=True)
legacy = mode == "legacy"

def workspace(address, name=None, number=None, special=False):
    obj = dict(name=name or address, monitor="fixture-monitor", monitorID=0,
               windows=1, hasfullscreen=False, lastwindow="0x1001")
    if not legacy:
        obj.update(address=address, type="special" if special else "normal")
    if number is not None:
        obj["id"] = number
    return obj

one = workspace("1", number=1)
two = workspace("2", number=2)
alpha = workspace("special:alpha", number=-98 if legacy else None, special=True)
beta = workspace("special:beta", number=-97 if legacy else None, special=True)
# Sources: Hyprland v0.56.0 WorkspaceQueryCore.cpp (named IDs <= -1337),
# v0.57.1 RegularWorkspace.cpp / Resolver.cpp (raw named address, name: selector).
named = workspace("chat", name="chat", number=-1337 if legacy else None)

def ref(ws):
    return {k: ws[k] for k in ("id", "address", "type", "name") if k in ws}

def client(addr, ws):
    return dict(address=addr, mapped=True, hidden=False, floating=False,
                **{"class": "fixture", "title": "fixture", "initialClass": "fixture",
                   "initialTitle": "fixture"}, pid=1, xwayland=False, pinned=False,
                swallowing="", focusHistoryID=0, at=[0, 0], size=[100, 100],
                fullscreen=0, fullscreenClient=0, workspace=ref(ws), monitor=0)

monitor = dict(id=0, name="fixture-monitor", description="fixture", make="fixture",
               model="fixture", serial="fixture", width=800, height=600,
               refreshRate=60.0, x=0, y=0, scale=1.0, transform=0, focused=True,
               dpmsStatus=True, vrr=False, activelyTearing=False, disabled=False,
               currentFormat="fixture", reserved=[0, 0, 0, 0], availableModes=[],
               activeWorkspace=ref(one), specialWorkspace=ref(alpha))
state = {"j/workspaces": [one, two, alpha, beta, named], "j/monitors": [monitor],
         "j/clients": [client("0x1001", one), client("0x1002", alpha), client("0x1003", beta)],
         "j/activeworkspace": ref(one), "j/activewindow": {"address": "0x1001"}}
if mode == "empty":
    state["j/workspaces"] = []
    state["j/clients"] = []
    state["j/activeworkspace"] = {}
    state["j/activewindow"] = {}
    monitor["activeWorkspace"] = {}
    monitor["specialWorkspace"] = {}

lock = threading.Lock()
connected = threading.Event()
event_conn = None
server_errors = []
commands = []
expected_dispatch = []
hold_workspaces = None
raw_active_error = False
requests = []
servers = []

def command_connection(conn):
    global hold_workspaces, raw_active_error
    try:
        request = conn.recv(65536).decode()
        with lock:
            requests.append(request.split(" ")[0])
            if request.startswith("dispatch "):
                commands.append(request)
                expected = expected_dispatch.pop(0) if expected_dispatch else None
                if request != expected:
                    server_errors.append("unexpected dispatch: " + request)
                response = "ok" if request == expected else "invalid selector"
            else:
                response = json.dumps(copy.deepcopy(state.get(request, {})))
            if request == "j/activeworkspace" and raw_active_error:
                response = "unsafe state"
                raw_active_error = False
            gate = hold_workspaces if request == "j/workspaces" else None
            if gate is not None: hold_workspaces = None
        if gate is not None:
            gate[0].set()
            if not gate[1].wait(4): raise ValueError("held response timed out")
        conn.sendall(response.encode())
    except (OSError, ValueError) as error:
        server_errors.append(type(error).__name__)
    finally:
        conn.close()

def listen(name):
    global event_conn
    path = runtime / "hypr" / "fixture" / ("." + name + ".sock")
    path.parent.mkdir(parents=True, exist_ok=True)
    server = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    server.bind(str(path)); server.listen(20); servers.append(server)
    while True:
        try:
            conn, _ = server.accept()
        except OSError:
            return
        if name == "socket2":
            event_conn = conn
            connected.set()
        else:
            threading.Thread(target=command_connection, args=(conn,), daemon=True).start()

for name in ("socket", "socket2"):
    threading.Thread(target=listen, args=(name,), daemon=True).start()
while len(servers) != 2:
    time.sleep(0.002)

try:
    if mode == "gjs":
        js = Path(__file__).with_name("introspection.js")
        subprocess.run(["gjs", "-m", str(js)], check=True, timeout=15)
        assert not server_errors, server_errors
        print("GJS typelib and workspace associations: PASS")
        sys.exit(0)

    import gi
    gi.require_version("AstalHyprland", "0.1")
    from gi.repository import AstalHyprland, GLib
    hypr = AstalHyprland.Hyprland.get_default()
    assert hypr is not None and connected.wait(2)
    context = GLib.MainContext.default()
    if mode == "empty":
        assert not hypr.props.workspaces and not hypr.props.clients
        assert hypr.props.focused_workspace is None and hypr.props.focused_client is None
        assert hypr.get_monitor(0).props.active_workspace is None
        assert hypr.get_monitor(0).props.special_workspace is None
        print("empty session: null focus and workspace references: PASS")
        sys.exit(0)

    def wait_for(predicate, message):
        deadline = time.monotonic() + 4
        while time.monotonic() < deadline:
            while context.pending():
                context.iteration(False)
            if predicate():
                return
            time.sleep(0.002)
        raise AssertionError(message)

    def event(name, payload=""):
        event_conn.sendall((name + ">>" + payload + "\n").encode())

    def sync_workspaces():
        done = []
        hypr.sync_workspaces(lambda obj, result: (obj.sync_workspaces_finish(result), done.append(True)))
        wait_for(lambda: done, "workspace sync timed out")

    def sync_clients():
        done = []
        hypr.sync_clients(lambda obj, result: (obj.sync_clients_finish(result), done.append(True)))
        wait_for(lambda: done, "client sync timed out")

    def sync_monitors():
        done = []
        hypr.sync_monitors(lambda obj, result: (obj.sync_monitors_finish(result), done.append(True)))
        wait_for(lambda: done, "monitor sync timed out")

    if mode == "recovery":
        # Hyprland v0.57.1 Commands.cpp activeWorkspaceRequest returns this
        # non-JSON reply when there is no focused monitor. Only this expected
        # diagnostic is consumed; other GLib warnings remain fatal.
        seen = []
        hypr.connect("event", lambda _, kind, payload: seen.append(kind))
        GLib.test_expect_message(None, GLib.LogLevelFlags.LEVEL_CRITICAL, "*Parse error*")
        with lock: raw_active_error = True
        event("focusedmon", "fixture-monitor,1")
        event("recovery-marker")
        wait_for(lambda: "recovery-marker" in seen, "reader stopped after recoverable IPC error")
        GLib.test_assert_expected_messages_internal("", __file__, 0, "reader_recovery")
        with lock:
            state["j/activeworkspace"] = ref(two)
            monitor["activeWorkspace"] = ref(two)
        event("workspacev2", "2,2")
        wait_for(lambda: hypr.props.focused_workspace == hypr.get_workspace(2), "valid state update lost after error")
        assert not server_errors, server_errors
        print("recoverable IPC parse error -> next event and real focus update: PASS")
        sys.exit(0)

    address_a = "-98" if legacy else "special:alpha"
    address_b = "-97" if legacy else "special:beta"
    address_named = "-1337" if legacy else "chat"
    a = hypr.get_workspace_by_address(address_a)
    b = hypr.get_workspace_by_address(address_b)
    normal = hypr.get_workspace(1)
    assert normal is not None and normal.props.id == 1
    assert a is not None and b is not None and a != b
    assert len({ws.props.id for ws in hypr.props.workspaces}) == 5
    assert a.props.id != 0 and b.props.id != 0 and a.props.id != b.props.id
    assert hypr.get_workspace(a.props.id) == a
    assert hypr.get_workspace_by_address(address_named) != normal
    assert hypr.get_client("0x1002").props.workspace == a
    assert hypr.get_client("0x1003").props.workspace == b
    assert hypr.get_monitor(0).props.special_workspace == a
    assert hypr.props.focused_workspace == normal
    assert a.props.clients == [hypr.get_client("0x1002")]
    print(mode + ": numeric, multiple special, named-number identity, initial links: PASS")

    # The integer API lookup contract for workspaces 1..10 must remain intact, including empty slots.
    for number in range(1, 11):
        ws = hypr.get_workspace(number)
        assert (ws is not None) == (number in (1, 2))
        if ws is not None:
            assert ws.props.id == number

    # Display-name changes must retain object identity and existing references.
    named["name"] = "renamed-chat-display"
    sync_workspaces()
    assert hypr.get_workspace_by_address(address_a) == a
    assert hypr.get_workspace_by_address(address_named).props.name == "renamed-chat-display"
    assert hypr.get_client("0x1002").props.workspace == a

    # Missing, null, scalar and malformed IDs must never warn or alias workspace zero.
    malformed = [dict(one, id=None), dict(one, id="1"), dict(one, id={}),
                 dict(one, id=[]), dict(one, id=True), dict(one, id=1.5),
                 dict(one, id=2 ** 40), dict(one, id=2), {"name": "unresolved"}, None]
    with lock:
        state["j/workspaces"].extend(malformed)
    sync_workspaces()
    assert len(hypr.props.workspaces) == 5
    assert hypr.get_workspace(0) is None
    with lock:
        state["j/workspaces"] = state["j/workspaces"][:5]
        state["j/clients"][1]["workspace"] = {"name": "unresolved"}
        state["j/monitors"][0]["specialWorkspace"] = None
    sync_clients(); sync_monitors()
    assert hypr.get_client("0x1002").props.workspace is None
    assert hypr.get_monitor(0).props.special_workspace is None
    with lock:
        state["j/clients"][1]["workspace"] = ref(alpha)
        state["j/monitors"][0]["specialWorkspace"] = ref(alpha)
    sync_clients(); sync_monitors()
    assert hypr.get_client("0x1002").props.workspace == a
    print(mode + ": rename, malformed/null/unresolved references: PASS")

    # Numeric focus remains valid with either old numeric or new address event payloads.
    with lock:
        state["j/activeworkspace"] = ref(two)
        state["j/monitors"][0]["activeWorkspace"] = ref(two)
    event("workspacev2", "2,renamed-display")
    wait_for(lambda: hypr.props.focused_workspace == hypr.get_workspace(2), "focus did not update")
    assert hypr.get_monitor(0).props.active_workspace == hypr.get_workspace(2)

    # Client move notifications must report the actual canonical object, never int.parse(address).
    moved = []
    hypr.connect("client-moved", lambda obj, client_, ws: moved.append(ws))
    with lock:
        state["j/clients"][0]["workspace"] = ref(beta)
    event("movewindowv2", "0x1001," + address_b + ",fixture")
    wait_for(lambda: moved, "client moved signal missing")
    assert moved[-1] == b and hypr.get_client("0x1001").props.workspace == b
    assert hypr.get_client("0x1001") in b.props.clients
    assert hypr.get_client("0x1001") not in normal.props.clients

    # Same-length membership changes still emit clients notifications.
    membership = []
    a.connect("notify::clients", lambda *_: membership.append(True))
    with lock:
        state["j/clients"][0]["workspace"] = ref(alpha)
        state["j/clients"][1]["workspace"] = ref(beta)
    sync_clients()
    assert membership and a.props.clients == [hypr.get_client("0x1001")]
    print(mode + ": focus, moved signals and client membership: PASS")

    added = []; removed = []
    hypr.connect("workspace-added", lambda obj, ws: added.append(ws))
    hypr.connect("workspace-removed", lambda obj, id_: removed.append(id_))
    gamma = workspace("special:gamma", number=-95 if legacy else None, special=True)
    with lock:
        state["j/workspaces"].append(gamma)
        state["j/clients"][0]["workspace"] = ref(gamma)
    # A move may be observed before the create event for its target workspace.
    event("movewindowv2", "1001," + ("-95" if legacy else "special:gamma") + ",fixture")
    wait_for(lambda: len(added) == 1 and len(moved) == 2, "new move target not resolved")
    g = added[0]
    assert moved[-1] == g and hypr.get_client("0x1001").props.workspace == g
    event("createworkspacev2", ("-95,fixture" if legacy else "special:gamma,fixture"))
    assert g.props.id not in (a.props.id, b.props.id, 0)
    assert hypr.get_workspace(g.props.id) == g
    # Duplicate v1/v2 lifecycle events must not duplicate objects or signals.
    event("createworkspace", "special:gamma")
    sync_workspaces()
    assert len(added) == 1
    with lock:
        state["j/workspaces"].remove(gamma)
    event("destroyworkspacev2", ("-95,fixture" if legacy else "special:gamma,fixture"))
    wait_for(lambda: removed, "workspace removal missing")
    assert removed == [g.props.id] and hypr.get_workspace(g.props.id) is None
    assert g.props.address == ("-95" if legacy else "special:gamma")  # retained ref remains valid
    assert hypr.get_workspace_by_address("-95" if legacy else "special:gamma") is None
    event("destroyworkspace", "special:gamma")
    sync_workspaces()
    assert removed == [g.props.id]

    # A newly opened client can also introduce a previously unknown workspace.
    delta = workspace("special:delta", number=-94 if legacy else None, special=True)
    with lock:
        state["j/workspaces"].append(delta)
        state["j/clients"].append(client("0x1004", delta))
    event("openwindow", "1004,fixture")
    wait_for(lambda: hypr.get_client("0x1004") is not None and
             hypr.get_client("0x1004").props.workspace is not None, "new client target unresolved")
    d = hypr.get_workspace_by_address("-94" if legacy else "special:delta")
    assert hypr.get_client("0x1004").props.workspace == d
    assert hypr.get_client("0x1004") in d.props.clients
    with lock:
        state["j/clients"].pop()
    event("closewindow", "1004")
    wait_for(lambda: hypr.get_client("0x1004") is None, "closed client still registered")

    # R-01: the command socket is already at the new lifetime while the
    # entire destroy/create history waits on socket2 (v0.56/v0.57 formats).
    barrier = []
    hypr.connect("event", lambda _, kind, payload: barrier.append(True) if kind == "reviewbarrier" else None)
    event("reviewbarrier")
    wait_for(lambda: barrier, "previous events not drained")
    lifecycle_events = []
    hypr.connect("event", lambda _, kind, payload: lifecycle_events.append(kind) if kind in ("createworkspace", "createworkspacev2", "destroyworkspace", "destroyworkspacev2") and ("alpha" in payload or payload.startswith(address_a + ",")) else None)
    with lock: state["j/clients"][0]["workspace"] = ref(alpha)
    # Hold a pre-destroy public sync response until after the event queue drains.
    obsolete = workspace("obsolete", name="obsolete", number=-1400 if legacy else None)
    received = threading.Event(); release = threading.Event(); stale_done = []
    with lock:
        state["j/workspaces"].append(obsolete)
        hold_workspaces = (received, release)
    hypr.sync_workspaces(lambda obj, result: (obj.sync_workspaces_finish(result), stale_done.append(True)))
    wait_for(received.is_set, "old snapshot not captured")
    with lock: state["j/workspaces"].remove(obsolete)
    old_a = a; old_handle = a.props.id
    old_removed = []
    a.connect("removed", lambda *_: old_removed.append(True))
    added.clear(); removed.clear()
    for kind in ("destroyworkspace", "destroyworkspacev2", "createworkspace", "createworkspacev2"):
        payload = address_a + ",special:alpha" if kind.endswith("v2") else "special:alpha"
        event(kind, payload)
    wait_for(lambda: len(lifecycle_events) == 4, "queued lifecycle stalled")
    release.set()
    wait_for(lambda: stale_done, "old sync did not complete")
    assert hypr.get_workspace_by_address("-1400" if legacy else "obsolete") is None
    a = hypr.get_workspace_by_address(address_a)
    assert a != old_a and old_removed == [True]
    assert removed == [old_handle] and added == [a]
    assert old_a.props.address == address_a
    assert legacy or a.props.id != old_handle
    assert hypr.get_client("0x1001").props.workspace == a
    sync_workspaces(); sync_workspaces()
    assert removed == [old_handle] and added == [a]

    # R-03: GIR, typelib and actual GObject flags agree on read-only address.
    import xml.etree.ElementTree as ET
    from gi.repository import GObject
    from gi import _gi
    ns = {"gi": "http://www.gtk.org/introspection/core/1.0"}
    prop = ET.parse(gir).find('.//gi:class[@name="Workspace"]/gi:property[@name="address"]', ns)
    assert prop.get("writable", "0") == "0"
    assert not (a.find_property("address").flags & GObject.ParamFlags.WRITABLE)
    repo = _gi.Repository.get_default()
    info = repo.find_by_name("AstalHyprland", "Workspace")
    props = info.get_properties()
    assert not next(p for p in props if p.get_name()=="address").get_flags() & GObject.ParamFlags.WRITABLE

    # R-02: independently specified expected selectors, including all helpers.
    cases = [(normal, "1"), (a, "special:alpha"),
             (hypr.get_workspace_by_address(address_named), "name:" + ("renamed-chat-display" if legacy else "chat"))]
    for i, word in enumerate(("empty", "next", "prev", "previous", "ordinary")):
        obj = workspace(word, name=word, number=-1338-i if legacy else None)
        with lock: state["j/workspaces"].append(obj)
        sync_workspaces()
        cases.append((hypr.get_workspace_by_address(str(-1338-i) if legacy else word), "name:"+word))
    expected_commands = []
    for target, selector in cases:
        for method, expected in ((lambda: target.focus(), "dispatch workspace "+selector),
                (lambda: target.move_to(hypr.get_monitor(0)), "dispatch moveworkspacetomonitor "+selector+" 0"),
                (lambda: hypr.get_client("0x1001").move_to(target), "dispatch movetoworkspacesilent "+selector+",address:0x1001")):
            before = len(commands)
            expected_dispatch.append(expected)
            method()
            wait_for(lambda: len(commands)>before, "dispatch missing")
            assert commands[-1] == expected, (commands[-1], expected)
            expected_commands.append(expected)
    print(mode + ": R01 queued lifecycle, R02 selectors/all helpers, R03 metadata: PASS")
    # IPC methods must send the canonical selector, never synthetic handles.
    expected_dispatch.append("dispatch workspace special:alpha")
    before = len(commands)
    a.focus()
    wait_for(lambda: len(commands) > before, "dispatch missing")
    assert commands[-1] == "dispatch workspace special:alpha"
    assert not server_errors, server_errors
    loaded = [line.split()[-1] for line in Path("/proc/self/maps").read_text().splitlines()
              if "libastal-hyprland.so" in line]
    assert loaded and all(Path(path).resolve() == Path(library).resolve() for path in loaded)
    print(mode + ": lifecycle, no duplicate signals, canonical dispatch, actual local ELF: PASS")
finally:
    if event_conn:
        event_conn.close()
    for server in servers:
        server.close()
    shutil.rmtree(runtime)
