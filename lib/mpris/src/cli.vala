using AstalMpris;

static bool help;
static bool version;
static bool list;
static bool raw;
[CCode(array_length = false, array_null_terminated = true)]
static string[] player_names;

static List<weak Player> players;

const OptionEntry[] options = {
    { "version", 'v', OptionFlags.NONE, OptionArg.NONE, ref version, null, null },
    { "help", 'h', OptionFlags.NONE, OptionArg.NONE, ref help, null, null },
    { "player", 'p', OptionFlags.NONE, OptionArg.STRING_ARRAY, ref player_names, null, null },
    { "list", 'l', OptionFlags.NONE, OptionArg.NONE, ref list, null, null },
    { "raw", 'r', OptionFlags.NONE, OptionArg.NONE, ref raw, null, null },
    { null },
};

async int main(string[] argv) {
    try {
        var opts = new OptionContext();
        opts.add_main_entries(options, null);
        opts.set_help_enabled(false);
        opts.set_ignore_unknown_options(false);
        opts.parse(ref argv);

        if (help) {
            stdout.printf("Usage:\n");
            stdout.printf("    %s [flags] [command]\n\n", argv[0]);
            stdout.printf("Flags:\n");
            stdout.printf("    -h, --help                   Print this help and exit\n");
            stdout.printf("    -v, --version                Print version number and exit\n");
            stdout.printf("    -l, --list                   List available players and exit\n");
            stdout.printf("    -p, --player                 Operate on given players\n");
            stdout.printf("    -r, --raw                    Print single line json info\n");
            stdout.printf("\nCommands:\n");
            stdout.printf("    info                         Print info\n");
            stdout.printf("    monitor                      Monitor changes\n");
            stdout.printf("    play                         Play track\n");
            stdout.printf("    pause                        Pause track\n");
            stdout.printf("    play-pause                   Play if paused, Pause if playing\n");
            stdout.printf("    stop                         Stop player\n");
            stdout.printf("    next                         Play next track\n");
            stdout.printf("    previous                     Play previous track\n");
            stdout.printf("    quit                         Quit player\n");
            stdout.printf("    raise                        Ask compositor to raise the player\n");
            stdout.printf("    position [OFFSET][+/-/%]     Set position\n");
            stdout.printf("    volume [LEVEL][+/-/%]        Set volume\n");
            stdout.printf("    loop [STATUS]                One of: \"None\", \"Track\", \"Playlist\"\n");
            stdout.printf("    shuffle [STATUS]             One of: \"On\", \"Off\", \"Toggle\"\n");
            stdout.flush();
            return 0;
        }

        if (version) {
            print(VERSION);
            return 0;
        }

        BusProxy proxy = yield BusProxy.new();
        var names = new GenericArray<string>();
        foreach (var name in yield proxy.list_names()) {
            if (name.has_prefix(MediaPlayerProxy.PREFIX)) {
                names.add(name);
            }
        }

        if (list) {
            foreach (var name in names) {
                print("%s\n", name.replace(MediaPlayerProxy.PREFIX, ""));
            }

            return 0;
        }

        players = new List<weak Player>();
        if (player_names.length > 0) {
            foreach (var name in player_names) {
                players.append(yield new Player.async(name));
            }
        } else {
            foreach (var name in names) {
                players.append(yield new Player.async(name));
            }
        }

        var cmd = argv[1];
        var arg = argv[2];

        switch (cmd) {
            case "monitor":
                return yield do_monitor();

            case "info":
                var list = new List<weak Player>();
                foreach (var player in players) {
                    list.append(player);
                }
                print_players(list);
                break;

            case "play":
                foreach (var player in players) {
                    player.play();
                }
                break;

            case "pause":
                foreach (var player in players) {
                    player.pause();
                }
                break;

            case "play-pause":
                foreach (var player in players) {
                    player.play_pause();
                }
                break;

            case "stop":
                foreach (var player in players) {
                    player.stop();
                }
                break;

            case "next":
                foreach (var player in players) {
                    player.next();
                }
                break;

            case "previous":
                foreach (var player in players) {
                    player.previous();
                }
                break;

            case "raise":
                foreach (var player in players) {
                    player.raise();
                }
                break;

            case "quit":
                foreach (var player in players) {
                    player.quit();
                }
                break;

            case "position":
                foreach (var player in players) {
                    if (do_position(player, arg) != 0) return 1;
                }
                break;

            case "volume":
                foreach (var player in players) {
                    if (do_volume(player, arg) != 0) return 1;
                }
                break;

            case "loop":
                foreach (var player in players) {
                    if (do_loop(player, arg) != 0) return 1;
                }
                break;

            case "shuffle":
                foreach (var player in players) {
                    if (do_shuffle(player, arg) != 0) return 1;
                }
                break;

            case "open":
                if (arg == null) {
                    printerr("missing open arg");
                    return 1;
                }

                foreach (var player in players) {
                    player.open_uri(arg);
                }
                break;

            default:
                if (cmd == null) {
                    printerr("missing command\n");
                } else {
                    printerr(@"unknown command \"$cmd\"\n");
                }
                return 1;
        }

        return 0;
    } catch (Error error) {
        printerr(error.message);
        return 1;
    }
}

Json.Node to_json(Player p) {
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

    return new Json.Builder().begin_object()
        .set_member_name("bus_name").add_string_value(p.bus_name)
        .set_member_name("identity").add_string_value(p.identity)
        .set_member_name("entry").add_string_value(p.entry)
        .set_member_name("can_quit").add_boolean_value(p.can_quit)
        .set_member_name("fullscreen").add_boolean_value(p.fullscreen)
        .set_member_name("can_set_fullscreen").add_boolean_value(p.can_set_fullscreen)
        .set_member_name("can_raise").add_boolean_value(p.can_raise)
        .set_member_name("supported_uri_schemes").add_value(uris.get_root())
        .set_member_name("supported_mime_types").add_value(mimes.get_root())
        .set_member_name("loop_status").add_string_value(p.loop_status.to_string())
        .set_member_name("shuffle_status").add_string_value(p.shuffle_status.to_string())
        .set_member_name("rate").add_double_value(p.rate)
        .set_member_name("volume").add_double_value(p.volume)
        .set_member_name("position").add_double_value(p.position)
        .set_member_name("playback_status").add_string_value(p.playback_status.to_string())
        .set_member_name("minimum_rate").add_double_value(p.minimum_rate)
        .set_member_name("maximum_rate").add_double_value(p.maximum_rate)
        .set_member_name("can_go_next").add_boolean_value(p.can_go_next)
        .set_member_name("can_go_previous").add_boolean_value(p.can_go_previous)
        .set_member_name("can_play").add_boolean_value(p.can_play)
        .set_member_name("can_pause").add_boolean_value(p.can_pause)
        .set_member_name("can_seek").add_boolean_value(p.can_seek)
        .set_member_name("can_control").add_boolean_value(p.can_control)
        .set_member_name("cover_art").add_string_value(p.cover_art)
        .set_member_name("metadata").add_value(Json.gvariant_serialize(p.metadata))
        .end_object()
        .get_root();
}

void print_players(List<weak Player> players) {
    var json = new Json.Builder().begin_array();

    foreach (var p in players) {
        json.add_value(to_json(p));
    }

    stdout.printf("%s\n", Json.to_string(json.end_array().get_root(), !raw));
    stdout.flush();
}

async int do_monitor() throws Error {
    var mpris = Mpris.get_default();

    mpris.player_added.connect((player) => {
        print_players(mpris.players);
        player.notify.connect(() => {
            print_players(mpris.players);
        });
    });

    mpris.player_closed.connect(() => {
        print_players(mpris.players);
    });

    new MainLoop(null, false).run();
    return 0;
}

int do_position(Player player, string? arg) {
    if (arg == null) {
        printerr("missing position argument\n");
        return 1;
    } else if (arg.has_suffix("%")) {
        var percent = double.parse(arg.slice(0, -1)) / 100;
        player.position = player.length * percent;
    } else if (arg.has_suffix("-")) {
        player.position += double.parse(arg.slice(0, -1)) * -1;
    } else if (arg.has_suffix("+")) {
        player.position += double.parse(arg.slice(0, -1));
    } else {
        player.position = double.parse(arg);
    }

    return 0;
}

int do_volume(Player player, string? arg) {
    if (arg == null) {
        printerr("missing volume argument\n");
        return 1;
    } else if (arg.has_suffix("%")) {
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
            printerr(@"unknown loop status \"$arg\"");
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
            printerr(@"unknown shuffle status \"$arg\"");
            return 1;
    }

    return 0;
}
