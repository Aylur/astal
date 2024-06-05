import { Gtk } from "./src/imports.js"

export * from "./src/imports.js"
export * from "./src/process.js"
export * from "./src/time.js"
export { bind, default as Binding } from "./src/binding.js"
export { Variable } from "./src/variable.js"
export * as Widget from "./src/widgets.js"
export { App } from "./src/application.js"

// gjs crashes if a widget is constructed before Gtk.init
Gtk.init(null)
