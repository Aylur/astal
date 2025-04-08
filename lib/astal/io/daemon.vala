[DBus (name="io.Astal.Application")]
public class AstalIO.Daemon : GLib.Application, AstalIO.Application {
    private SocketService service;
    private DBusConnection conn;
    private string _instance_name = "astal";
    private string socket_path { get; private set; }

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

    public void inspector() throws DBusError, IOError {
        throw new DBusError.FAILED("Daemon does not implement inspector");
    }

    public void toggle_window(string window) throws DBusError, IOError {
        throw new DBusError.FAILED("Daemon does not implement toggle_window");
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
        hold();

        shutdown.connect(() => { try { quit(); } catch(Error err) {} });
        Unix.signal_add(1, () => { try { quit(); } catch(Error err) {} }, Priority.HIGH);
        Unix.signal_add(2, () => { try { quit(); } catch(Error err) {} }, Priority.HIGH);
        Unix.signal_add(15, () => { try { quit(); } catch(Error err) {} }, Priority.HIGH);
    }
}
