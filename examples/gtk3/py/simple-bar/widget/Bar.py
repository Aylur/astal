import math
from gi.repository import (
    AstalIO,
    Astal,
    Gtk,
    Gdk,
    GLib,
    GObject,
    AstalBattery as Battery,
    AstalWp as Wp,
    AstalNetwork as Network,
    AstalTray as Tray,
    AstalMpris as Mpris,
    AstalHyprland as Hyprland,
)

SYNC = GObject.BindingFlags.SYNC_CREATE


class Workspaces(Gtk.Box):
    def __init__(self) -> None:
        super().__init__()
        Astal.widget_set_class_names(self, ["Workspaces"])
        hypr = Hyprland.get_default()
        hypr.connect("notify::workspaces", self.sync)
        hypr.connect("notify::focused-workspace", self.sync)
        self.sync()

    def sync(self, *_):
        hypr = Hyprland.get_default()
        for child in self.get_children():
            child.destroy()

        for ws in hypr.get_workspaces():
            if not (ws.get_id() >= -99 and ws.get_id() <= -2): # filter out special workspaces
                self.add(self.button(ws))

    def button(self, ws):
        hypr = Hyprland.get_default()
        btn = Gtk.Button(visible=True)
        btn.add(Gtk.Label(visible=True, label=ws.get_id()))

        if hypr.get_focused_workspace() == ws:
            Astal.widget_set_class_names(btn, ["focused"])

        btn.connect("clicked", lambda *_: ws.focus())
        return btn


class FocusedClient(Gtk.Label):
    def __init__(self) -> None:
        super().__init__()
        Astal.widget_set_class_names(self, ["Focused"])
        Hyprland.get_default().connect("notify::focused-client", self.sync)
        self.sync()

    def sync(self, *_):
        client = Hyprland.get_default().get_focused_client()
        if client is None:
            return self.set_label("")

        client.bind_property("title", self, "label", SYNC)


class Media(Gtk.Box):
    def __init__(self) -> None:
        super().__init__()
        self.players = {}
        mpris = Mpris.get_default()
        Astal.widget_set_class_names(self, ["Media"])
        mpris.connect("notify::players", self.sync)
        self.sync()

    def sync(self):
        mpris = Mpris.get_default()
        for child in self.get_children():
            child.destroy()

        if len(mpris.get_players()) == 0:
            self.add(Gtk.Label(visible=True, label="Nothing Playing"))
            return

        player = mpris.get_players()[0]
        label = Gtk.Label(visible=True)
        cover = Gtk.Box(valign=Gtk.Align.CENTER)
        Astal.widget_set_class_names(cover, ["Cover"])

        self.add(cover)
        self.add(label)

        player.bind_property(
            "metadata",
            label,
            "label",
            SYNC,
            lambda *_: f"{player.get_artist()} - {player.get_title()}",
        )

        def on_cover_art(*_):
            Astal.widget_set_css(
                cover, f"background-image: url('{player.get_cover_art()}')"
            )

        id = player.connect("notify::cover-art", on_cover_art)
        cover.connect("destroy", lambda _: player.disconnect(id))
        on_cover_art()


class SysTray(Gtk.Box):
    def __init__(self) -> None:
        super().__init__()
        Astal.widget_set_class_names(self, ["SysTray"])
        self.items = {}
        tray = Tray.get_default()
        tray.connect("item_added", self.add_item)
        tray.connect("item_removed", self.remove_item)

    def add_item(self, _: Tray.Tray, id: str):
        if id in self.items:
            return

        item = Tray.get_default().get_item(id)
        btn = Gtk.MenuButton(use_popover=False, visible=True)
        icon = Astal.Icon(visible=True)

        item.bind_property("tooltip-markup", btn, "tooltip-markup", SYNC)
        item.bind_property("gicon", icon, "gicon", SYNC)
        item.bind_property("menu-model", btn, "menu-model", SYNC)
        btn.insert_action_group("dbusmenu", item.get_action_group())

        def on_action_group(*args):
            btn.insert_action_group("dbusmenu", item.get_action_group())

        item.connect("notify::action-group", on_action_group)

        btn.add(icon)
        self.add(btn)
        self.items[id] = btn

    def remove_item(self, _: Tray.Tray, id: str):
        if id in self.items:
            del self.items[id]


class Wifi(Astal.Icon):
    def __init__(self) -> None:
        super().__init__()
        Astal.widget_set_class_names(self, ["Wifi"])
        wifi = Network.get_default().get_wifi()
        if wifi:
            wifi.bind_property("ssid", self, "tooltip-text", SYNC)
            wifi.bind_property("icon-name", self, "icon", SYNC)


class AudioSlider(Gtk.Box):
    def __init__(self) -> None:
        super().__init__()
        Astal.widget_set_class_names(self, ["AudioSlider"])
        Astal.widget_set_css(self, "min-width: 140px")

        icon = Astal.Icon()
        slider = Astal.Slider(hexpand=True)

        self.add(icon)
        self.add(slider)

        speaker = Wp.get_default().get_audio().get_default_speaker()
        speaker.bind_property("volume-icon", icon, "icon", SYNC)
        speaker.bind_property("volume", slider, "value", SYNC)
        slider.connect("dragged", lambda *_: speaker.set_volume(slider.get_value()))


class BatteryLevel(Gtk.Box):
    def __init__(self) -> None:
        super().__init__()
        Astal.widget_set_class_names(self, ["Battery"])

        icon = Astal.Icon()
        label = Astal.Label()

        self.add(icon)
        self.add(label)

        bat = Battery.get_default()
        bat.bind_property("is-present", self, "visible", SYNC)
        bat.bind_property("battery-icon-name", icon, "icon", SYNC)
        bat.bind_property(
            "percentage",
            label,
            "label",
            SYNC,
            lambda _, value: f"{math.floor(value * 100)}%",
        )


class Time(Astal.Label):
    def __init__(self, format="%H:%M - %A %e."):
        super().__init__()
        self.format = format
        self.interval = AstalIO.Time.interval(1000, self.sync)
        self.connect("destroy", self.interval.cancel)
        Astal.widget_set_class_names(self, ["Time"])

    def sync(self):
        self.set_label(GLib.DateTime.new_now_local().format(self.format))


class Left(Gtk.Box):
    def __init__(self) -> None:
        super().__init__(hexpand=True, halign=Gtk.Align.START)
        self.add(Workspaces())
        self.add(FocusedClient())


class Center(Gtk.Box):
    def __init__(self) -> None:
        super().__init__()
        self.add(Media())


class Right(Gtk.Box):
    def __init__(self) -> None:
        super().__init__(hexpand=True, halign=Gtk.Align.END)
        self.add(SysTray())
        self.add(Wifi())
        self.add(AudioSlider())
        self.add(BatteryLevel())
        self.add(Time())


class Bar(Astal.Window):
    def __init__(self, monitor: Gdk.Monitor):
        super().__init__(
            anchor=Astal.WindowAnchor.LEFT
            | Astal.WindowAnchor.RIGHT
            | Astal.WindowAnchor.TOP,
            gdkmonitor=monitor,
            exclusivity=Astal.Exclusivity.EXCLUSIVE,
        )

        Astal.widget_set_class_names(self, ["Bar"])

        self.add(
            Astal.CenterBox(
                start_widget=Left(),
                center_widget=Center(),
                end_widget=Right(),
            )
        )

        self.show_all()
