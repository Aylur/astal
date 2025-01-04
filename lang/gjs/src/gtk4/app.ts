import Gtk from "gi://Gtk?version=4.0"
import { mkApp } from "../_app"
import Astal from "gi://Astal?version=4.0"

Gtk.init()

// users might want to use Adwaita in which case it has to be initialized
// it might be common pitfall to forget it because `App` is not `Adw.Application`
await import("gi://Adw?version=1")
    .then(({ default: Adw }) => Adw.init())
    .catch(() => void 0)

export default await mkApp<Astal.Application>("4")
