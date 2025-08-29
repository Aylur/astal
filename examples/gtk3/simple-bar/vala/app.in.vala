class App : Gtk.Application {
    static App instance;

    private Bar bar;

    private void init_css() {
        var provider = new Gtk.CssProvider();
        provider.load_from_data("""@STYLE@""");

        Gtk.StyleContext.add_provider_for_screen(
            Gdk.Screen.get_default(),
            provider,
            Gtk.STYLE_PROVIDER_PRIORITY_USER
        );
    }

    // this is the method that will be invoked on `app.run()`
    // this is where everything should be initialized and instantiated
    public override int command_line(ApplicationCommandLine command_line) {
        var argv = command_line.get_arguments();

        if (command_line.is_remote) {
            // app is already running we can print to remote
            command_line.print_literal("hello from the main instance\n");

            // for example, we could toggle the visibility of the bar
            if (argv.length >= 3 && argv[1] == "toggle" && argv[2] == "bar") {
                bar.visible = !bar.visible;
            }
        } else {
            // main instance, initialize stuff here
            init_css();
            add_window((bar = new Bar()));
        }

        return 0;
    }

    private App() {
        application_id = "my.awesome.simple-bar";
        flags = ApplicationFlags.HANDLES_COMMAND_LINE;
    }

    // entry point of our app
    static int main(string[] argv) {
        App.instance = new App();
        Environment.set_prgname("simple-bar");
        return App.instance.run(argv);
    }
}
