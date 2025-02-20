import Astal from "gi://Astal?version=4.0"
import Gtk from "gi://Gtk?version=4.0"
import Gdk from "gi://Gdk?version=4.0"
import Gsk from "gi://Gsk?version=4.0"
import astalify, { type ConstructProps } from "./astalify.js"

export { Astal, Gtk, Gdk, Gsk }
export { default as App } from "./app.js"
export { astalify, ConstructProps }
export * as Widget from "./widget.js"
export { hook } from "../_astal"
