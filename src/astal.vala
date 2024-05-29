namespace Astal {
public class Application : Gtk.Application {
    private List<Gtk.CssProvider> css_providers;
    private SocketService service;
    private string socket;

    public new string application_id {
        get { return base.application_id; }
        set { base.application_id = value; }
    }

    private string _instance_name;
    public string instance_name {
        get { return _instance_name; }
        set {
            application_id = "io.Astal." + value;
            _instance_name = value;
        }
    }

    public List<Gtk.Window> windows {
        get { return get_windows(); }
    }

    public Gtk.Settings settings {
        get { return Gtk.Settings.get_default(); }
    }

    public Gdk.Screen screen {
        get { return Gdk.Screen.get_default(); }
    }

    public string gtk_theme {
        owned get { return settings.gtk_theme_name; }
        set { settings.gtk_theme_name = value; }
    }

    public string icon_theme {
        owned get { return settings.gtk_icon_theme_name; }
        set { settings.gtk_icon_theme_name = value; }
    }

    public string cursor_theme {
        owned get { return settings.gtk_cursor_theme_name; }
        set { settings.gtk_cursor_theme_name = value; }
    }

    public void reset_css() {
        foreach(var provider in css_providers) {
            Gtk.StyleContext.remove_provider_for_screen(screen, provider);
            css_providers.remove_all(provider);
        }
    }

    public void inspector() {
        Gtk.Window.set_interactive_debugging(true);
    }

    public Gtk.Window? get_window(string name) {
        foreach(var win in windows) {
            if (win.name == name)
                return win;
        }

        critical("no window with name \"%s\"".printf(name));
        return null;
    }

    public void apply_css(string style, bool reset = false) {
        var provider = new Gtk.CssProvider();

        if (reset)
            reset_css();

        try {
            if (FileUtils.test(style, FileTest.EXISTS))
                provider.load_from_path(style);
            else
                provider.load_from_data(style);
        } catch (Error err) {
            critical(err.message);
        }

        Gtk.StyleContext.add_provider_for_screen(
            screen, provider, Gtk.STYLE_PROVIDER_PRIORITY_USER);

        css_providers.append(provider);
    }

    private async void _socket_request(SocketConnection conn) {
        string message = yield read_sock(conn);
        request(message.strip(), conn);
    }

    public virtual void request(string msg, SocketConnection conn) {
        write_sock.begin(conn, "missing response implementation on ".concat(application_id));
    }

    /**
     * should be called before `run()`
     * the return value indicates if instance is already running
     */
    public bool acquire_socket() {
        socket = GLib.Environment.get_user_runtime_dir().concat(
            "/",
            instance_name,
            ".sock");

        if (FileUtils.test(socket, GLib.FileTest.EXISTS)) {
            info("socket %s exists", socket);
            return false;
        }

        try {
            service = new SocketService();
            service.add_address(
                new UnixSocketAddress(socket),
                SocketType.STREAM,
                SocketProtocol.DEFAULT,
                null,
                null);

            service.incoming.connect((conn) => {
                _socket_request.begin(conn);
                return false;
            });

            info("socket acquired: %s\n", socket);
            return true;
        } catch (Error err) {
            critical("could not acquire socket %s\n", application_id);
            critical(err.message);
            return false;
        }
    }

    construct {
        if (instance_name == null)
            instance_name = "astal";

        shutdown.connect(() => {
            if (FileUtils.test(socket, GLib.FileTest.EXISTS)){
                try {
                    File.new_for_path(socket).delete(null);
                } catch (Error err) {
                    warning(err.message);
                }
            }
        });

        SourceFunc close = () => { quit(); };
        Unix.signal_add(1, close, Priority.HIGH);
        Unix.signal_add(2, close, Priority.HIGH);
        Unix.signal_add(15, close, Priority.HIGH);
    }
}

public async string read_sock(SocketConnection conn) {
    try {
        var stream = new DataInputStream(conn.input_stream);
        return yield stream.read_upto_async("\x04", -1, Priority.DEFAULT, null, null);
    } catch (Error err) {
        critical(err.message);
        return err.message;
    }
}

public async void write_sock(SocketConnection conn, string response) {
    try {
        yield conn.output_stream.write_async(
            response.concat("\x04").data,
            Priority.DEFAULT);
    } catch (Error err) {
        critical(err.message);
    }
}
}
