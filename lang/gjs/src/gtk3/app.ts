import Gtk from "gi://Gtk?version=3.0"
import { mkApp } from "../_app"
import Astal from "gi://Astal?version=3.0"

Gtk.init(null)

export default await mkApp<Astal.Application>("3")
