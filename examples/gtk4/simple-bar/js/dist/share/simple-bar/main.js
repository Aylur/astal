#!/nix/store/jasd05x715hihimvzhpha86mdz6jykqn-gjs-1.84.2/bin/gjs -m

import { exit, programArgs } from "system"
import Gio from "gi://Gio"
import GLib from "gi://GLib"

// makes sure `LD_PRELOAD` does not leak into subprocesses
GLib.setenv("LD_PRELOAD", "", true)
Gio.Resource.load("/home/demeter/Projects/astal/examples/gtk4/simple-bar/js/dist/share/simple-bar/data.gresource")._register()

const module = await import("resource:///index.js")
exit(await module.default.main(programArgs))
