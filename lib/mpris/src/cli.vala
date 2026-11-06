using AstalMpris;
using Quarrel;

abstract class MprisCommand : Command {
    static SpecialFlag help;
    static StringArrayOpt player_names;
    static Flag pretty_print;

    public abstract async int execute() throws Error;

    protected static int err(string msg) {
        printerr(@"\x1b[1;31merror:\x1b[0m $msg\n");
        return 1;
    }

    static async string[] list_bus_names() throws Error {
        string[] names = {};
        BusProxy proxy = yield BusProxy.new();

        foreach (var name in yield proxy.list_names()) {
            if (name.has_prefix(MediaPlayerProxy.PREFIX)) {
                names += name;
            }
        }

        return names;
    }

    static async List<Player> load_players() throws Error {
        var players = new List<Player>();

        if (player_names.value.length > 0) {
            foreach (var name in player_names.value) {
                var busname = name.has_prefix(MediaPlayerProxy.PREFIX)
                    ? name
                    : MediaPlayerProxy.PREFIX + name;

                players.append(yield Player.new_async(busname));
            }
        } else {
            foreach (var name in yield list_bus_names()) {
                players.append(yield Player.new_async(name));
            }
        }

        return players;
    }

    static Json.Node to_json(Player p) {
        var uris = new Json.Builder().begin_array();
        foreach (var uri in p.supported_uri_schemes) {
            uris.add_string_value(uri);
        }
        uris.end_array();

        var mimes = new Json.Builder().begin_array();
        foreach (var mime in p.supported_mime_types) {
            mimes.add_string_value(mime);
        }
        mimes.end_array();

        return new Json.Builder()
            .begin_object()
            .set_member_name("bus_name")
            .add_string_value(p.bus_name)
            .set_member_name("identity")
            .add_string_value(p.identity)
            .set_member_name("entry")
            .add_string_value(p.entry)
            .set_member_name("can_quit")
            .add_boolean_value(p.can_quit)
            .set_member_name("fullscreen")
            .add_boolean_value(p.fullscreen)
            .set_member_name("can_set_fullscreen")
            .add_boolean_value(p.can_set_fullscreen)
            .set_member_name("can_raise")
            .add_boolean_value(p.can_raise)
            .set_member_name("supported_uri_schemes")
            .add_value(uris.get_root())
            .set_member_name("supported_mime_types")
            .add_value(mimes.get_root())
            .set_member_name("loop_status")
            .add_string_value(p.loop_status.to_string())
            .set_member_name("shuffle_status")
            .add_string_value(p.shuffle_status.to_string())
            .set_member_name("rate")
            .add_double_value(p.rate)
            .set_member_name("volume")
            .add_double_value(p.volume)
            .set_member_name("position")
            .add_double_value(p.position)
            .set_member_name("playback_status")
            .add_string_value(p.playback_status.to_string())
            .set_member_name("minimum_rate")
            .add_double_value(p.minimum_rate)
            .set_member_name("maximum_rate")
            .add_double_value(p.maximum_rate)
            .set_member_name("can_go_next")
            .add_boolean_value(p.can_go_next)
            .set_member_name("can_go_previous")
            .add_boolean_value(p.can_go_previous)
            .set_member_name("can_play")
            .add_boolean_value(p.can_play)
            .set_member_name("can_pause")
            .add_boolean_value(p.can_pause)
            .set_member_name("can_seek")
            .add_boolean_value(p.can_seek)
            .set_member_name("can_control")
            .add_boolean_value(p.can_control)
            .set_member_name("cover_art")
            .add_string_value(p.cover_art)
            .set_member_name("metadata")
            .add_value(Json.gvariant_serialize(p.metadata))
            .end_object()
            .get_root();
    }

    static void print_players(List<weak Player> players) {
        var json = new Json.Builder().begin_array();

        foreach (var p in players) {
            json.add_value(to_json(p));
        }

        stdout.printf("%s\n", Json.to_string(json.end_array().get_root(), pretty_print.enabled));
        stdout.flush();
    }

    class ListPlayers : MprisCommand {
        Flag json;

        public ListPlayers() {
            name = "list";
            about("List available players");
            opt(json = new Flag("json", 'j', "Print list as JSON"));
            opt(pretty_print);
        }

        public override async int execute() throws Error {
            if (json.enabled) {
                var list = new Json.Builder().begin_array();

                foreach (var name in yield list_bus_names()) {
                    list.add_string_value(name.replace(MediaPlayerProxy.PREFIX, ""));
                }

                print("%s\n", Json.to_string(list.end_array().get_root(), false));
            } else {
                foreach (var name in yield list_bus_names()) {
                    print("%s\n", name.replace(MediaPlayerProxy.PREFIX, ""));
                }
            }

            return 0;
        }
    }

    class Watch : MprisCommand {
        Flag once;

        public Watch() {
            name = "watch";
            about("Print players as JSON");
            opt(once = new Flag("once", 'o', "Print once and exit"));
            opt(pretty_print);
            opt(player_names);
        }

        bool should_print(Player player) {
            if (player_names.value.length == 0) return true;

            foreach (var name in player_names.value) {
                var busname = name.has_prefix(MediaPlayerProxy.PREFIX)
                    ? name
                    : MediaPlayerProxy.PREFIX + name;

                if (player.bus_name == busname) return true;
            }

            return false;
        }

        List<weak Player> filtered_players(Mpris mpris) {
            var players = new List<weak Player>();

            foreach (var player in mpris.players) {
                if (should_print(player)) {
                    players.append(player);
                }
            }

            return players;
        }

        void start_watcher() {
            var mpris = Mpris.get_default();

            mpris.player_added.connect((player) => {
                if (!should_print(player)) return;

                print_players(filtered_players(mpris));
                player.notify.connect(() => {
                    if (should_print(player)) {
                        print_players(filtered_players(mpris));
                    }
                });
            });

            mpris.player_closed.connect((player) => {
                if (should_print(player)) {
                    print_players(filtered_players(mpris));
                }
            });

            new MainLoop(null, false).run();
        }

        async void print_info() throws Error {
            var players = yield load_players();
            var available_players = new List<weak Player>();
            foreach (var player in players) {
                if (player.available) {
                    available_players.append(player);
                }
            }
            print_players(available_players.copy());
        }

        public override async int execute() throws Error {
            if (once.enabled) {
                yield print_info();
            } else {
                start_watcher();
            }
            return 0;
        }
    }

    class PlayerAction : MprisCommand {
        public delegate void ActionFunc(Player player);

        ActionFunc action;

        public PlayerAction(string name, string description, owned ActionFunc action) {
            this.name = name;
            about(description);
            this.action = (owned)action;
        }

        public override async int execute() throws Error {
            var players = yield load_players();

            foreach (var player in players) {
                action(player);
            }

            return 0;
        }
    }

    class Position : MprisCommand {
        public Position() {
            name = "position";
            about("Set player position");
            required_arg("OFFSET", "Position in seconds, percentage, or delta with + or - suffix");
            example("astal-mpris position 10%");
            example("astal-mpris position 10%-");
            example("astal-mpris position 10%+");
            example("astal-mpris position 60");
            example("astal-mpris position 30+");
            example("astal-mpris position 30-");
        }

        static int do_position(Player player, string arg) {
            if (arg.has_suffix("%")) {
                player.position = player.length * (double.parse(arg.slice(0, -1)) / 100);
            } else if (arg.has_suffix("-")) {
                player.position += double.parse(arg.slice(0, -1)) * -1;
            } else if (arg.has_suffix("+")) {
                player.position += double.parse(arg.slice(0, -1));
            } else {
                player.position = double.parse(arg);
            }

            return 0;
        }

        public override async int execute() throws Error {
            var players = yield load_players();

            foreach (var player in players) {
                if (do_position(player, args[0]) != 0) return 1;
            }

            return 0;
        }
    }

    class Volume : MprisCommand {
        public Volume() {
            name = "volume";
            about("Set player volume");
            required_arg("LEVEL", "Volume level, percentage, or delta with + or - suffix");
            example("astal-mpris volume 10%");
            example("astal-mpris volume 10%-");
            example("astal-mpris volume 10%+");
            example("astal-mpris volume 60");
            example("astal-mpris volume 30+");
            example("astal-mpris volume 30-");
        }

        static int do_volume(Player player, string arg) {
            if (arg.has_suffix("%")) {
                player.volume = double.parse(arg.slice(0, -1)) / 100;
            } else if (arg.has_suffix("-")) {
                player.volume += (double.parse(arg.slice(0, -1)) * -1) / 100;
            } else if (arg.has_suffix("+")) {
                player.volume += double.parse(arg.slice(0, -1)) / 100;
            } else {
                player.volume = double.parse(arg);
            }

            return 0;
        }

        public override async int execute() throws Error {
            var players = yield load_players();

            foreach (var player in players) {
                if (do_volume(player, args[0]) != 0) return 1;
            }

            return 0;
        }
    }

    class LoopStatus : MprisCommand {
        public LoopStatus() {
            name = "loop";
            about("Set or cycle player loop status");
            arg("STATUS", "One of: 'None', 'Track', 'Playlist'. Omit to cycle.");
        }

        static int do_loop(Player player, string? arg) {
            if (arg == null) {
                player.loop();
                return 0;
            }

            switch (arg) {
                case "None":
                    player.loop_status = Loop.NONE;
                    break;
                case "Track":
                    player.loop_status = Loop.TRACK;
                    break;
                case "Playlist":
                    player.loop_status = Loop.PLAYLIST;
                    break;
                default:
                    return err(@"unknown loop status \"$arg\"");
            }

            return 0;
        }

        public override async int execute() throws Error {
            var players = yield load_players();
            var status = (args.length > 0) ? args[0] : null;

            foreach (var player in players) {
                if (do_loop(player, status) != 0) return 1;
            }

            return 0;
        }
    }

    class ShuffleStatus : MprisCommand {
        public ShuffleStatus() {
            name = "shuffle";
            about("Set or toggle player shuffle status");
            arg("STATUS", "One of: 'On', 'Off', 'Toggle'. Omit to toggle.");
        }

        static int do_shuffle(Player player, string? arg) {
            if (arg == null) {
                player.shuffle();
                return 0;
            }

            switch (arg) {
                case "On":
                    player.shuffle_status = Shuffle.ON;
                    break;
                case "Off":
                    player.shuffle_status = Shuffle.OFF;
                    break;
                case "Toggle":
                    player.shuffle();
                    break;
                default:
                    return err(@"unknown shuffle status \"$arg\"");
            }

            return 0;
        }

        public override async int execute() throws Error {
            var players = yield load_players();
            var status = (args.length > 0) ? args[0] : null;

            foreach (var player in players) {
                if (do_shuffle(player, status) != 0) return 1;
            }

            return 0;
        }
    }

    class Open : MprisCommand {
        public Open() {
            name = "open";
            about("Open a URI in the player");
            required_arg("URI", "URI to open");
        }

        public override async int execute() throws Error {
            var players = yield load_players();

            foreach (var player in players) {
                player.open_uri(args[0]);
            }

            return 0;
        }
    }

    class CLI : MprisCommand {
        SpecialFlag version;

        void cmd(Command cmd) {
            subcommand(cmd.opt(help));
        }

        public CLI() {
            name = "astal-mpris";
            about("Control MPRIS media players");

            pretty_print = new Flag("pretty", 'p', "Pretty print JSON");
            opt(help = new SpecialFlag("help", 'h', "Print help"));
            opt(version = new SpecialFlag("version", 'v', "Print version"));
            opt(player_names = new StringArrayOpt("name", 'n', "Operate on given players") {
                name = "NAME",
            });

            cmd(new ListPlayers());
            cmd(new Watch());
            cmd(new PlayerAction("play", "Play track", (player) => {
                player.play();
            }));
            cmd(new PlayerAction("pause", "Pause track", (player) => {
                player.pause();
            }));
            cmd(new PlayerAction("play-pause", "Play if paused, pause if playing", (player) => {
                player.play_pause();
            }));
            cmd(new PlayerAction("stop", "Stop player", (player) => {
                player.stop();
            }));
            cmd(new PlayerAction("next", "Play next track", (player) => {
                player.next();
            }));
            cmd(new PlayerAction("previous", "Play previous track", (player) => {
                player.previous();
            }));
            cmd(new PlayerAction("quit", "Quit player", (player) => {
                player.quit();
            }));
            cmd(new PlayerAction("raise", "Ask compositor to raise the player", (player) => {
                player.raise();
            }));
            cmd(new Position());
            cmd(new Volume());
            cmd(new LoopStatus());
            cmd(new ShuffleStatus());
            cmd(new Open());
        }

        public override async int execute() throws Error {
            if (version.enabled) {
                print("%s\n", VERSION);
                return 0;
            }

            printerr("%s\n", Quarrel.help(this));
            return 1;
        }
    }

    static async int main(string[] argv) {
        try {
            var cmd = new CLI().parse(argv) as MprisCommand;

            if (help.enabled) {
                print("%s\n", Quarrel.help(cmd));
                return 0;
            }

            return yield cmd.execute();
        } catch (ParseError parse_error) {
            return err(parse_error.message);
        } catch (Error error) {
            return err(error.message);
        }
    }
}
