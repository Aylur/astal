import Astal from "gi://Astal?version=4.0"
import Gtk from "gi://Gtk?version=4.0"
import Gdk from "gi://Gdk?version=4.0"
import astalify, { type ConstructProps, type BindableChild } from "./astalify.js"

export { Astal, Gtk, Gdk }
export { default as App } from "./app.js"
export { astalify, ConstructProps, BindableChild }
export * as Widget from "./widget.js"
