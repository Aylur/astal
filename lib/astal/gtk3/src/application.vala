[DBus (name="io.Astal.Application")]
public class Astal.Application : Gtk.Application, AstalIO.Application {
    private List<Gtk.CssProvider> css_providers = new List<Gtk.CssProvider>();
    private SocketService service;
    private DBusConnection conn;
    private string _instance_name = "astal";
    private string socket_path { get; private set; }

    /**
     * Emitted when a new monitor is added to [class@Gdk.Display].
     */
    [DBus (visible=false)]
    public signal void monitor_added(Gdk.Monitor monitor);

    /**
     * Emitted when a monitor is disconnected from [class@Gdk.Display].
     */
    [DBus (visible=false)]
    public signal void monitor_removed(Gdk.Monitor monitor);

    /**
     * Emitted when a window that has been added using
     * [method@Gtk.Application.add_window] changes its visibility .
     */
    [DBus (visible=false)]
    public signal void window_toggled(Gtk.Window window);

    /**
     * Get all monitors from [class@Gdk.Display].
     */
    [DBus (visible=false)]
    public List<weak Gdk.Monitor> monitors {
        owned get {
            var display = Gdk.Display.get_default();
            var list = new List<weak Gdk.Monitor>();
            for (var i = 0; i <= display.get_n_monitors(); ++i) {
                var mon = display.get_monitor(i);
                if (mon != null) {
                    list.append(mon);
                }
            }
            return list;
        }
    }

    /**
     * A unique instance name.
     *
     * This is the identifier used by the AstalIO package and the CLI.
     */
    [DBus (visible=false)]
    public string instance_name {
        owned get { return _instance_name; }
        construct set {
            _instance_name = value != null ? value : "astal";
            application_id = @"io.Astal.$_instance_name";
        }
    }

    /**
     * Windows that has been added to this app using [method@Gtk.Application.add_window].
     */
    [DBus (visible=false)]
    public List<Gtk.Window> windows {
        get { return get_windows(); }
    }

    private Gtk.Settings settings {
        get { return Gtk.Settings.get_default(); }
    }

    private Gdk.Screen screen {
        get { return Gdk.Screen.get_default(); }
    }

    /**
     * Shortcut for [property@Gtk.Settings:gtk_theme_name]
     */
    [DBus (visible=false)]
    public string gtk_theme {
        owned get { return settings.gtk_theme_name; }
        set { settings.gtk_theme_name = value; }
    }

    /**
     * Shortcut for [property@Gtk.Settings:gtk_icon_theme_name]
     */
    [DBus (visible=false)]
    public string icon_theme {
        owned get { return settings.gtk_icon_theme_name; }
        set { settings.gtk_icon_theme_name = value; }
    }

    /**
     * Shortcut for [property@Gtk.Settings:gtk_cursor_theme_name]
     */
    [DBus (visible=false)]
    public string cursor_theme {
        owned get { return settings.gtk_cursor_theme_name; }
        set { settings.gtk_cursor_theme_name = value; }
    }

    /**
     * Remove all [class@Gtk.StyleContext] providers.
     */
    [DBus (visible=false)]
    public void reset_css() {
        foreach(var provider in css_providers) {
            Gtk.StyleContext.remove_provider_for_screen(screen, provider);
        }
        css_providers = new List<Gtk.CssProvider>();
    }

    /**
     * Shortcut for [func@Gtk.Window.set_interactive_debugging].
     */
    public void inspector() throws DBusError, IOError {
        Gtk.Window.set_interactive_debugging(true);
    }

    /**
     * Get a window by its [property@Gtk.Widget:name] that has been added to this app
     * using [method@Gtk.Application.add_window].
     */
    [DBus (visible=false)]
    public Gtk.Window? get_window(string name) {
        foreach(var win in windows) {
            if (win.name == name)
                return win;
        }

        critical("no window with name \"%s\"".printf(name));
        return null;
    }

    /**
     * Toggle the visibility of a window by its [property@Gtk.Widget:name]
     * that has been added to this app using [method@Gtk.Application.add_window].
     */
    public void toggle_window(string window) throws Error {
        var win = get_window(window);
        if (win != null) {
            win.visible = !win.visible;
        } else {
            throw new IOError.FAILED("window not found");
        }
    }

    /**
     * Add a new [class@Gtk.StyleContext] provider.
     *
     * @param style Css string or a path to a css file.
     */
    [DBus (visible=false)]
    public void apply_css(string style, bool reset = false) {
        var provider = new Gtk.CssProvider();

        if (reset)
            reset_css();

        try {
            if (FileUtils.test(style, FileTest.EXISTS)) {
                provider.load_from_path(style);
            } else if (style.has_prefix("resource://")) {
                provider.load_from_resource(style.replace("resource://", ""));
            } else {
                provider.load_from_data(style);
            }
        } catch (Error err) {
            critical(err.message);
        }

        Gtk.StyleContext.add_provider_for_screen(
            screen, provider, Gtk.STYLE_PROVIDER_PRIORITY_USER);

        css_providers.append(provider);
    }

    /**
     * Shortcut for [method@Gtk.IconTheme.prepend_search_path].
     */
    [DBus (visible=false)]
    public void add_icons(string? path) {
        if (path != null) {
            Gtk.IconTheme.get_default().prepend_search_path(path);
        }
    }

    /**
     * Handler for an incoming request.
     *
     * @param request Body of the request
     * @param conn The connection which expects the response.
     */
    [DBus (visible=false)]
    public virtual void request(string request, SocketConnection conn) {
        AstalIO.write_sock.begin(conn, @"missing response implementation on $application_id");
    }

    /**
     * Attempt to acquire the astal socket for this app identified by its [property@AstalIO.Application:instance_name].
     * If the socket is in use by another app with the same name an [error@AstalIO.AppError.NAME_OCCUPIED] is thrown.
     */
    [DBus (visible=false)]
    public void acquire_socket() throws Error {
        string path;
        service = AstalIO.acquire_socket(this, out path);
        socket_path = path;

        Bus.own_name(
            BusType.SESSION,
            application_id,
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
            () => {}
        );
    }

    /**
     * Quit and stop the socket if it was acquired.
     */
    public new void quit() throws DBusError, IOError {
        if (service != null) {
            service.stop();
            service.close();
        }

        base.quit();
    }

    construct {
        activate.connect(() => {
            var display = Gdk.Display.get_default();
            display.monitor_added.connect((mon) => {
                monitor_added(mon);
                notify_property("monitors");
            });
            display.monitor_removed.connect((mon) => {
                monitor_removed(mon);
                notify_property("monitors");
            });
        });

        window_added.connect((window) => {
            ulong id1, id2;
            id1 = window.notify["visible"].connect(() => window_toggled(window));
            id2 = window_removed.connect((removed) => {
                if (removed == window) {
                    window.disconnect(id1);
                    this.disconnect(id2);
                }
            });
        });

        shutdown.connect(() => { try { quit(); } catch(Error err) {} });
        Unix.signal_add(1, () => { try { quit(); } catch(Error err) {} }, Priority.HIGH);
        Unix.signal_add(2, () => { try { quit(); } catch(Error err) {} }, Priority.HIGH);
        Unix.signal_add(15, () => { try { quit(); } catch(Error err) {} }, Priority.HIGH);
    }
}
