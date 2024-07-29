namespace Astal {
[DBus (name="io.Astal.Application")]
public class Application : Gtk.Application {
    private List<Gtk.CssProvider> css_providers = new List<Gtk.CssProvider>();
    private SocketService service;
    private DBusConnection conn;
    private string _instance_name;

    public string socket_path { get; private set; }

    [DBus (visible=false)]
    public string instance_name {
        get { return _instance_name; }
        set {
            application_id = "io.Astal." + value;
            _instance_name = value;
        }
    }

    [DBus (visible=false)]
    public List<Gtk.Window> windows {
        get { return get_windows(); }
    }

    [DBus (visible=false)]
    public Gtk.Settings settings {
        get { return Gtk.Settings.get_default(); }
    }

    [DBus (visible=false)]
    public Gdk.Screen screen {
        get { return Gdk.Screen.get_default(); }
    }

    [DBus (visible=false)]
    public string gtk_theme {
        owned get { return settings.gtk_theme_name; }
        set { settings.gtk_theme_name = value; }
    }

    [DBus (visible=false)]
    public string icon_theme {
        owned get { return settings.gtk_icon_theme_name; }
        set { settings.gtk_icon_theme_name = value; }
    }

    [DBus (visible=false)]
    public string cursor_theme {
        owned get { return settings.gtk_cursor_theme_name; }
        set { settings.gtk_cursor_theme_name = value; }
    }

    [DBus (visible=false)]
    public void reset_css() {
        foreach(var provider in css_providers) {
            Gtk.StyleContext.remove_provider_for_screen(screen, provider);
        }
        css_providers = new List<Gtk.CssProvider>();
    }

    public void inspector() throws DBusError, IOError {
        Gtk.Window.set_interactive_debugging(true);
    }

    [DBus (visible=false)]
    public Gtk.Window? get_window(string name) {
        foreach(var win in windows) {
            if (win.name == name)
                return win;
        }

        critical("no window with name \"%s\"".printf(name));
        return null;
    }

    public void toggle_window(string window) throws DBusError, IOError {
        var win = get_window(window);
        if (win != null) {
            win.visible = !win.visible;
        } else {
            throw new IOError.FAILED("window not found");
        }
    }

    [DBus (visible=false)]
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

    [DBus (visible=false)]
    public void add_icons(string? path) {
        if (path != null)
            Gtk.IconTheme.get_default().prepend_search_path(path);
    }

    private async void _socket_request(SocketConnection conn) {
        string message = yield read_sock(conn);
        request(message != null ? message.strip() : "", conn);
    }

    [DBus (visible=false)]
    public virtual void request(string msg, SocketConnection conn) {
        write_sock.begin(conn, @"missing response implementation on $application_id");
    }

    /**
     * should be called before `run()`
     * the return value indicates if instance is already running
     */
    [DBus (visible=false)]
    public bool acquire_socket() {
        var rundir = GLib.Environment.get_user_runtime_dir();
        socket_path = @"$rundir/$instance_name.sock";

        if (FileUtils.test(socket_path, GLib.FileTest.EXISTS)) {
            info("socket %s exists", socket_path);
            return false;
        }

        try {
            service = new SocketService();
            service.add_address(
                new UnixSocketAddress(socket_path),
                SocketType.STREAM,
                SocketProtocol.DEFAULT,
                null,
                null);

            service.incoming.connect((conn) => {
                _socket_request.begin(conn, (_, res) => _socket_request.end(res));
                return false;
            });

            Bus.own_name(
                BusType.SESSION,
                "io.Astal." + instance_name,
                BusNameOwnerFlags.NONE,
                (conn) => {
                    try {
                    this.conn = conn;
                        conn.register_object("/io/Astal/Application", this);
                    } catch (Error err) {
                        critical(err.message);
                    }
                },
                () => {},
                () => {});

            info("socket acquired: %s\n", socket_path);
            return true;
        } catch (Error err) {
            critical("could not acquire socket %s\n", application_id);
            critical(err.message);
            return false;
        }
    }

    public string message(string? msg) throws DBusError, IOError {
        var rundir = GLib.Environment.get_user_runtime_dir();
        var socket_path = @"$rundir/$instance_name.sock";
        var client = new SocketClient();

        if (msg == null)
            msg = "";

        try {
            var conn = client.connect(new UnixSocketAddress(socket_path), null);
            conn.output_stream.write(msg.concat("\x04").data);

            var stream = new DataInputStream(conn.input_stream);
            return stream.read_upto("\x04", -1, null, null);
        } catch (Error err) {
            printerr(err.message);
            return "";
        }
    }

    construct {
        if (instance_name == null)
            instance_name = "astal";

        shutdown.connect(() => {
            if (FileUtils.test(socket_path, GLib.FileTest.EXISTS)){
                try {
                    File.new_for_path(socket_path).delete(null);
                } catch (Error err) {
                    warning(err.message);
                }
            }
        });

        Unix.signal_add(1, () => { try { quit(); } catch(Error err) {} }, Priority.HIGH);
        Unix.signal_add(2, () => { try { quit(); } catch(Error err) {} }, Priority.HIGH);
        Unix.signal_add(15, () => { try { quit(); } catch(Error err) {} }, Priority.HIGH);
    }

    public new void quit() throws DBusError, IOError {
        base.quit();
    }

    public static List<string> get_instances() {
        var list = new List<string>();
        var prefix = "io.Astal.";

        try {
            DBusImpl dbus = Bus.get_proxy_sync(
                BusType.SESSION,
                "org.freedesktop.DBus",
                "/org/freedesktop/DBus"
            );

            foreach (var busname in dbus.list_names()) {
                if (busname.has_prefix(prefix))
                    list.append(busname.replace(prefix, ""));
            }
        } catch (Error err) {
            critical(err.message);
        }

        return list;
    }

    public static void quit_instance(string instance) {
        try {
            IApplication proxy = Bus.get_proxy_sync(
                BusType.SESSION,
                "io.Astal." + instance,
                "/io/Astal/Application"
            );

            proxy.quit();
        } catch (Error err) {
            critical(err.message);
        }
    }

    public static void open_inspector(string instance) {
        try {
            IApplication proxy = Bus.get_proxy_sync(
                BusType.SESSION,
                "io.Astal." + instance,
                "/io/Astal/Application"
            );

            proxy.inspector();
        } catch (Error err) {
            critical(err.message);
        }
    }

    public static void toggle_window_by_name(string instance, string window) {
        try {
            IApplication proxy = Bus.get_proxy_sync(
                BusType.SESSION,
                "io.Astal." + instance,
                "/io/Astal/Application"
            );

            proxy.toggle_window(window);
        } catch (Error err) {
            critical(err.message);
        }
    }

    public static string send_message(string instance_name, string msg) {
        var rundir = GLib.Environment.get_user_runtime_dir();
        var socket_path = @"$rundir/$instance_name.sock";
        var client = new SocketClient();

        try {
            var conn = client.connect(new UnixSocketAddress(socket_path), null);
            conn.output_stream.write(msg.concat("\x04").data);

            var stream = new DataInputStream(conn.input_stream);
            return stream.read_upto("\x04", -1, null, null);
        } catch (Error err) {
            printerr(err.message);
            return "";
        }
    }
}

[DBus (name="org.freedesktop.DBus")]
private interface DBusImpl : DBusProxy {
    public abstract string[] list_names() throws GLib.Error;
}

[DBus (name="io.Astal.Application")]
private interface IApplication : DBusProxy {
    public abstract void quit() throws GLib.Error;
    public abstract void inspector() throws GLib.Error;
    public abstract void toggle_window(string window) throws GLib.Error;
    public abstract string message(string window) throws GLib.Error;
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
