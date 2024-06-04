local lgi = require("lgi")
local Astal = lgi.require("Astal", "0.1")
local Gtk = lgi.require("Gtk", "3.0")
local Gdk = lgi.require("Gdk", "3.0")
local GObject = lgi.require("GObject", "2.0")
local Widget = require("astal.widget")
local Variable = require("astal.variable")
local Binding = require("astal.binding")
local App = require("astal.application")
local Process = require("astal.process")
local Time = require("astal.time")

return {
    App = App,
    Variable = Variable,
    Widget = Widget,
    bind = Binding.new,
    interval = Time.interval,
    timeout = Time.timeout,
    idle = Time.idle,
    subprocess = Process.subprocess,
    exec = Process.exec,
    exec_async = Process.exec_async,

    Astal = Astal,
    Gtk = Gtk,
    Gdk = Gdk,
    GObject = GObject,
    GLib = lgi.require("GLib", "2.0"),
    Gio = lgi.require("Gio", "2.0"),
    require = lgi.require,
}
