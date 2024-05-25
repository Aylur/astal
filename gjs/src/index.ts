import Gtk from "gi://Gtk?version=3.0"
import Astal from "gi://Astal?version=0.1"
import GLib from "gi://GLib?version=2.0"
import GObject from "gi://GObject?version=2.0"
import Gio from "gi://Gio?version=2.0"

export { subprocess, exec, execAsync } from "./process.js"
export { interval, timeout, idle } from "./time.js"
export { bind } from "./binding.js"
export { Variable } from "./variable.js"
export * as Widget from "./widgets.js"
export { App } from "./application.js"

// for convinience
export { Astal, Gtk, GLib, GObject, Gio }

// gjs crashes if a widget is constructed before Gtk.init
Gtk.init(null)
