namespace AstalMpris {
    /**
     * Gets the default singleton Mpris instance.
     */
    public Mpris get_default() {
        return Mpris.get_default();
    }
}

/**
 * Object that monitors dbus for players to appear and disappear.
 */
public class AstalMpris.Mpris : Object {
    internal static string PREFIX = "org.mpris.MediaPlayer2.";

    private static Mpris instance;
    private DBusImpl proxy;

    /**
     * Gets the default singleton Mpris instance.
     */
    public static Mpris get_default() {
        if (instance == null)
            instance = new Mpris();

        return instance;
    }

    private HashTable<string, Player> _players =
        new HashTable<string, Player> (str_hash, str_equal);

    /**
     * List of currently available players.
     */
    public List<weak Player> players { owned get { return _players.get_values(); } }

    /**
     * Emitted when a new mpris Player appears.
     */
    public signal void player_added(Player player);

    /**
     * Emitted when a Player disappears.
     */
    public signal void player_closed(Player player);

    construct {
        try {
            proxy = Bus.get_proxy_sync(
                BusType.SESSION,
                "org.freedesktop.DBus",
                "/org/freedesktop/DBus"
            );

            foreach (var busname in proxy.list_names()) {
                if (busname.has_prefix(Mpris.PREFIX))
                    add_player(busname);
            }

            proxy.name_owner_changed.connect((name, old_owner, new_owner) => {
                if (!name.has_prefix(Mpris.PREFIX))
                    return;

                if (new_owner != "" && old_owner == "")
                    add_player(name);
            });
        } catch (Error error) {
            critical(error.message);
        }
    }

    private void add_player(string busname) {
        var p = new Player(busname);
        _players.set(busname, p);

        ulong id = 0;
        id = p.closed.connect(() => {
            if (id > 0) p.disconnect(id);
            player_closed(p);
            _players.remove(busname);
            notify_property("players");
        });

        player_added(p);
        notify_property("players");
    }
}
