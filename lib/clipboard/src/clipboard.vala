namespace AstalClipboard {
public static unowned Clipboard get_default() {
    return Clipboard.get_default();
}

public class Clipboard : Object {
    private static Clipboard? _instance;

    public static unowned Clipboard get_default() {
        if (_instance == null) {
            _instance = new Clipboard();
        }
        return _instance;
    }

    private Backend clip;

    public signal void selected(DataOffer offer);

    public List<weak DataOffer> history {
        get {
            return this.clip.history;
        }
    }

    private void acquire_bus(DBusConnection conn) {}

    private void name_acquired(DBusConnection conn) {
        var daemon = new Daemon(conn);
        if (clip != null) {
            foreach (var item in clip.history) {
                daemon.add_to_history(item);
            }
        }
        clip = daemon;
        clip.selected.connect((offer) => this.selected(offer));
        clip.notify["history"].connect(() => this.notify_property("history"));
        active();
    }

    private void name_lost() {
        clip = new Proxy();
        clip.notify["history"].connect(() => this.notify_property("history"));
        clip.selected.connect((offer) => this.selected(offer));
        active();
    }
    internal signal void active();

    construct {
        MainLoop? loop = null;

        if (!MainContext.default().is_owner()) {
            loop = new MainLoop();
        }

        bool done = false;
        Bus.own_name(
            BusType.SESSION,
            "io.astal.clipboard",
            BusNameOwnerFlags.NONE,
            acquire_bus,
            name_acquired,
            name_lost
        );

        active.connect(() => {
            done = true;
            if ((loop != null) && loop.is_running()) {
                loop.quit();
            }
        });

        if (loop != null) {
            loop.run();
        } else {
            while (!done) {
                MainContext.default().iteration(false);
            }
        }
    }
}
}
