import GObject from "gi://GObject"
import Gio from "gi://Gio"
import GLib from "gi://GLib"
import Gtk from "gi://Gtk?version=4.0"
import Gdk from "gi://Gdk?version=4.0"
import Astal from "gi://Astal?version=4.0"
import Bar from "./bar/Bar"

export default class App extends Astal.Application {
    static {
        GObject.registerClass(this)
    }

    static instance: App
    private bar!: Bar

    constructor() {
        super({
            applicationId: "my.awesome.simple-bar",
            flags: Gio.ApplicationFlags.HANDLES_COMMAND_LINE,
        })
    }

    private initCss() {
        const provider = new Gtk.CssProvider()
        provider.load_from_resource("/style.css")

        Gtk.StyleContext.add_provider_for_display(
            Gdk.Display.get_default()!,
            provider,
            Gtk.STYLE_PROVIDER_PRIORITY_USER,
        )
    }

    vfunc_command_line(command_line: Gio.ApplicationCommandLine): number {
        const argv = command_line.get_arguments()

        if (command_line.isRemote) {
            // app is already running we can print to remote
            command_line.print_literal("hello from the main instance\n")

            // for example, we could toggle the visibility of the bar
            if (argv.length >= 2 && argv[0] == "toggle" && argv[1] == "bar") {
                this.bar.visible = !this.bar.visible
            }

            // exit remote process
            command_line.done()
        } else {
            // main instance, initialize stuff here
            this.initCss()
            this.bar = new Bar()
            this.add_window(this.bar)
        }

        return 0
    }

    // entry point of our app
    static async main(argv: string[]): Promise<number> {
        App.instance = new App()
        GLib.set_prgname("simple-bar")
        return App.instance.runAsync(argv)
    }
}
