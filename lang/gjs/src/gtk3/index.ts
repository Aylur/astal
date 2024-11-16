import Astal from "gi://Astal?version=3.0"
import Gtk from "gi://Gtk?version=3.0"
import Gdk from "gi://Gdk?version=3.0"
import astalify, { type ConstructProps, type BindableProps } from "./astalify.js"

export { Astal, Gtk, Gdk }
export { default as App } from "./app.js"
export { astalify, ConstructProps, BindableProps }
export * as Widget from "./widget.js"
