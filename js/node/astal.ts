import gi from "node-gtk"
import Time from "../src/time.js"
import Process from "../src/process.js"
import * as variable from "../src/variable.js"
const Astal = gi.require("Astal", "0.1")

const { interval, timeout, idle } = Time(Astal.Time)
const { subprocess, exec, execAsync } = Process({
    defaultOut: console.log,
    defaultErr: console.error,
    exec: Astal.Process.exec,
    execv: Astal.Process.execv,
    execAsync: Astal.Process.execAsync,
    execAsyncv: Astal.Process.execAsyncv,
    subprocess: Astal.Process.subprocess,
    subprocessv: Astal.Process.subprocessv,
})

variable.config.defaultErrHandler = console.log
variable.config.execAsync = execAsync
variable.config.subprocess = subprocess
variable.config.interval = interval
variable.config.VariableBase = Astal.VariableBase
Object.freeze(variable.config)

export { subprocess, exec, execAsync }
export { interval, timeout, idle }
export { bind } from "../src/binding.js"
export { Variable } from "../src/variable.js"
export * as Widget from "./widgets.js"
export { App } from "./application.js"

// for convinience
export const GLib = gi.require("GLib", "2.0")
export const Gtk = gi.require("Gtk", "3.0")
export const Gio = gi.require("Gio", "2.0")
export const GObject = gi.require("GObject", "2.0")
export { Astal, gi }
