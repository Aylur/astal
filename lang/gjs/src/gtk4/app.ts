import Gtk from "gi://Gtk?version=4.0"
import Astal from "gi://Astal?version=4.0"
import { mkApp } from "../_app"

Gtk.init()

export default mkApp(Astal.Application)
