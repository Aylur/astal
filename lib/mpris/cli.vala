namespace AstalMpris {
static bool help;
static bool version;
static bool list;
static bool raw;
[CCode (array_length = false, array_null_terminated = true)]
static string[] players;

const OptionEntry[] options = {
    { "version", 'v', OptionFlags.NONE, OptionArg.NONE, ref version, null, null },
    { "help", 'h', OptionFlags.NONE, OptionArg.NONE, ref help, null, null },
    { "player", 'p', OptionFlags.NONE, OptionArg.STRING_ARRAY, ref players, null, null },
    { "list", 'l', OptionFlags.NONE, OptionArg.NONE, ref list, null, null },
    { "raw", 'r', OptionFlags.NONE, OptionArg.NONE, ref raw, null, null },
    { null },
};

int main(string[] argv) {
    try {
        var opts = new OptionContext();
        opts.add_main_entries(options, null);
        opts.set_help_enabled(false);
        opts.set_ignore_unknown_options(false);
        opts.parse(ref argv);
    } catch (OptionError err) {
        printerr(err.message);
        return 1;
    }

    if (help) {
        print("Usage:\n");
        print("    %s [flags] [command]\n\n", argv[0]);
        print("Flags:\n");
        print("    -h, --help                   Print this help and exit\n");
        print("    -v, --version                Print version number and exit\n");
        print("    -l, --list                   List available players\n");
        print("    -p, --player                 Operate on given player\n");
        print("    -r, --raw                    Print single line json info\n");
        print("\nCommands:\n");
        print("    info                         Print info about player\n");
        print("    monitor                      Monitor changes\n");
        print("    play                         Play track\n");
        print("    pause                        Pause track\n");
        print("    play-pause                   Play if paused, Pause if playing\n");
        print("    stop                         Stop player\n");
        print("    next                         Play next track\n");
        print("    previous                     Play previous track\n");
        print("    quit                         Quit player\n");
        print("    raise                        Ask compositor to raise the player\n");
        print("    position [OFFSET][+/-/%]     Set position of player\n");
        print("    volume [LEVEL][+/-/%]        Set volume of player\n");
        print("    loop [STATUS]                One of: \"None\", \"Track\", \"Playlist\"\n");
        print("    shuffle [STATUS]             One of: \"On\", \"Off\", \"Toggle\"\n");
        return 0;
    }

    if (version) {
        print(VERSION);
        return 0;
    }

    var mpris = new Mpris();
    var mpris_players = new List<Player>();

    if (list) {
        foreach (var p in mpris.players)
            print("%s\n", p.bus_name.replace(Mpris.PREFIX, ""));

        return 0;
    }

    if (players.length > 0) {
        foreach (var name in players)
            mpris_players.append(new Player(name));
    } else {
        foreach (var p in mpris.players)
            mpris_players.append(p);
    }

    var cmd = argv[1];
    var arg = argv[2];

    switch (cmd) {
        case "monitor":
            return do_monitor(mpris);

        case "info":
            print_players(mpris_players.copy());
            break;

        case "play":
            foreach (var player in mpris_players)
                player.play();
            break;

        case "pause":
            foreach (var player in mpris_players)
                player.pause();
            break;

        case "play-pause":
            foreach (var player in mpris_players)
                player.play_pause();
            break;

        case "stop":
            foreach (var player in mpris_players)
                player.stop();
            break;

        case "next":
            foreach (var player in mpris_players)
                player.next();
            break;

        case "previous":
            foreach (var player in mpris_players)
                player.previous();
            break;

        case "raise":
            foreach (var player in mpris_players)
                player.raise();
            break;

        case "quit":
            foreach (var player in mpris_players)
                player.quit();
            break;

        case "position":
            foreach (var player in mpris_players) {
                if (do_position(player, arg) != 0)
                    return 1;
            }
            break;

        case "volume":
            foreach (var player in mpris_players) {
                if (do_volume(player, arg) != 0)
                    return 1;
            }
            break;

        case "loop":
            foreach (var player in mpris_players) {
                if (do_loop(player, arg) != 0)
                    return 1;
            }
            break;

        case "shuffle":
            foreach (var player in mpris_players) {
                if (do_shuffle(player, arg) != 0)
                    return 1;
            }
            break;

        case "open":
            if (arg == null) {
                stderr.printf("missing open arg");
                return 1;
            }

            foreach (var player in mpris_players)
                player.open_uri(arg);
            break;

        default:
            if (cmd == null)
                stderr.printf("missing command\n");
            else
                stderr.printf(@"unknown command \"$cmd\"\n");
            return 1;
    }

    return 0;
}

Json.Node to_json(Player p) {
    var uris = new Json.Builder().begin_array();
    foreach (var uri in p.supported_uri_schemes)
        uris.add_string_value(uri);

    uris.end_array();

    return new Json.Builder().begin_object()
        .set_member_name("bus_name").add_string_value(p.bus_name)
        .set_member_name("available").add_boolean_value(p.available)
        .set_member_name("identity").add_string_value(p.identity)
        .set_member_name("entry").add_string_value(p.entry)
        .set_member_name("supported_uri_schemes").add_value(uris.get_root())
        .set_member_name("loop_status").add_string_value(p.loop_status.to_string())
        .set_member_name("shuffle_status").add_string_value(p.shuffle_status.to_string())
        .set_member_name("rate").add_double_value(p.rate)
        .set_member_name("volume").add_double_value(p.volume)
        .set_member_name("position").add_double_value(p.position)
        .set_member_name("cover_art").add_string_value(p.cover_art)
        .set_member_name("metadata").add_value(Json.gvariant_serialize(
            p.metadata != null ? p.metadata : new HashTable<string, Variant>(str_hash, str_equal)))
        .end_object()
        .get_root();
}

void print_players(List<weak Player> players) {
    var json = new Json.Builder().begin_array();

    foreach (var p in players)
        json.add_value(to_json(p));

    stdout.printf("%s\n", Json.to_string(json.end_array().get_root(), !raw));
    stdout.flush();
}

int do_monitor(Mpris mpris) {
    print_players(mpris.players);
    foreach (var player in mpris.players) {
        player.notify.connect(() => print_players(mpris.players));
    }

    mpris.player_added.connect((player) => {
        player.notify.connect(() => print_players(mpris.players));
    });

    mpris.player_closed.connect(() => {
        print_players(mpris.players);
    });

    new MainLoop(null, false).run();
    return 0;
}

int do_position(Player player, string? arg) {
    if (arg == null) {
        stderr.printf("missing position argument\n");
        return 1;
    }

    else if (arg.has_suffix("%")) {
        var percent = double.parse(arg.slice(0, -1)) / 100;
        player.position = player.length * percent;
    }

    else if (arg.has_suffix("-")) {
        player.position += double.parse(arg.slice(0, -1)) * -1;
    }

    else if (arg.has_suffix("+")) {
        player.position += double.parse(arg.slice(0, -1));
    }

    else {
        player.position = double.parse(arg);
    }

    return 0;
}

int do_volume(Player player, string? arg) {
    if (arg == null) {
        stderr.printf("missing volume argument\n");
        return 1;
    }

    else if (arg.has_suffix("%")) {
        player.volume = double.parse(arg.slice(0, -1)) / 100;
    }

    else if (arg.has_suffix("-")) {
        player.volume += (double.parse(arg.slice(0, -1)) * -1) / 100;
    }

    else if (arg.has_suffix("+")) {
        player.volume += double.parse(arg.slice(0, -1)) / 100;
    }

    else {
        player.volume = double.parse(arg);
    }

    return 0;
}

int do_loop(Player player, string? arg) {
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
            stderr.printf(@"unknown shuffle status \"$arg\"");
            return 1;
    }

    return 0;
}

int do_shuffle(Player player, string? arg) {
    if (arg == null) {
        player.shuffle();
        return 1;
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
            stderr.printf(@"unknown shuffle status \"$arg\"");
            return 1;
    }

    return 0;
}
}
