import Astal from "gi://Astal"
import Time from "../src/time.js"
import Process from "../src/process.js"
import * as variable from "../src/variable.js"

const { interval, timeout, idle } = Time(Astal.Time)
const { subprocess, exec, execAsync } = Process({
    defaultOut: print,
    defaultErr: console.error,
    exec: Astal.Process.exec,
    execv: Astal.Process.execv,
    execAsync: Astal.Process.exec_async,
    execAsyncv: Astal.Process.exec_asyncv,
    subprocess: Astal.Process.subprocess,
    subprocessv: Astal.Process.subprocessv,
})

variable.config.defaultErrHandler = print
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
export { default as GLib } from "gi://GLib?version=2.0"
export { default as Gtk } from "gi://Gtk?version=3.0"
export { default as Gio } from "gi://Gio?version=2.0"
export { default as GObject } from "gi://GObject?version=2.0"
export { default as Astal } from "gi://Astal?version=0.1"
