namespace AstalMpris {
public Mpris get_default() {
    return Mpris.get_default();
}

public class Mpris : Object {
    internal static string PREFIX = "org.mpris.MediaPlayer2.";

    private static Mpris instance;
    public static Mpris get_default() {
        if (instance == null)
            instance = new Mpris();

        return instance;
    }

    private DBusImpl proxy;

    private HashTable<string, Player> _players =
        new HashTable<string, Player> (str_hash, str_equal);

    public List<weak Player> players { owned get { return _players.get_values(); } }
    public bool poll_position { get; set; }

    public signal void player_added (Player player);
    public signal void player_closed (Player player);

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

        p.closed.connect(() => {
            player_closed(p);
            _players.remove(busname);
            notify_property("players");
        });

        player_added(p);
        notify_property("players");
    }
}
}
