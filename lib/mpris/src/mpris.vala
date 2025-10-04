namespace AstalMpris {
/**
 * Gets the default singleton Mpris instance.
 */
public Mpris get_default() {
    return Mpris.get_default();
}
}

/**
 * Manager object that monitors the session DBus for Mpris players to appear and disappear.
 */
public class AstalMpris.Mpris : Object, ListModel {
    private static Mpris instance;
    private BusProxy proxy;

    /**
     * Gets the default singleton Mpris instance.
     */
    public static Mpris get_default() {
        if (instance == null) instance = new Mpris();
        return instance;
    }

    private HashTable<string, Player> player_table =
        new HashTable<string, Player>(str_hash, str_equal);

    private List<weak Player> player_list = new List<weak Player>();

    /**
     * List of currently available players.
     */
    public List<weak Player> players {
        get { return player_list; }
    }

    /**
     * Emitted when a new mpris Player appears.
     */
    public signal void player_added(Player player);

    /**
     * Emitted when a Player disappears.
     */
    public signal void player_closed(Player player);

    construct {
        init.begin((_, res) => {
            try {
                init.end(res);
            } catch (Error error) {
                critical(error.message);
            }
        });
    }

    private async void init() throws Error {
        if (proxy != null) return;

        proxy = yield BusProxy.new();

        foreach (var busname in yield proxy.list_names()) {
            if (busname.has_prefix(MediaPlayerProxy.PREFIX)) add_player(busname);
        }

        proxy.name_owner_changed.connect((name, old_owner, new_owner) => {
            if (!name.has_prefix(MediaPlayerProxy.PREFIX)) return;
            if ((new_owner != "") && (old_owner == "")) add_player(name);
        });
    }

    private void add_player(string busname) {
        var p = new Player(busname);
        player_table.set(busname, p);

        ulong id = 0;
        id = p.notify["available"].connect(() => {
            if (p.available) {
                player_list = player_table.get_values().copy();
                notify_property("players");
                player_added(p);
                items_changed(player_table.size(), 0, 1);
            } else {
                if (id > 0) p.disconnect(id);
                player_table.remove(busname);
                player_list = player_table.get_values().copy();
                notify_property("players");
                player_closed(p);

                var pos = 0;
                foreach (var player in player_list) {
                    if (player == p) {
                        items_changed(pos, 1, 0);
                        break;
                    }
                    pos = +1;
                }
            }
        });
    }

    public Object? get_item(uint position) {
        // realistically there will be at max 3-4 players at a time
        // so iterating shuold be fine? if not we should use a table lookup instead
        var i = 0;
        foreach (var player in player_list) {
            if (i++ == position) {
                return player;
            }
        }

        return null;
    }

    public GLib.Type get_item_type() {
        return typeof(Player);
    }

    public uint get_n_items() {
        return player_table.size();
    }
}
