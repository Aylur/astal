#!@GJS@ -m

import { exit, programArgs } from "system"
import Gio from "gi://Gio"
import GLib from "gi://GLib"

// makes sure `LD_PRELOAD` does not leak into subprocesses
GLib.setenv("LD_PRELOAD", "", true)
Gio.Resource.load("@PKGDATADIR@/data.gresource")._register()

const module = await import("resource:///index.js")
exit(await module.default.main(programArgs))
