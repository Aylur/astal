using AstalNotifd;
using Quarrel;

abstract class NotifdCommand : Command {
    static SpecialFlag help;

    public abstract async int execute();

    protected static int err(string msg) {
        printerr(@"\x1b[1;31merror:\x1b[0m $msg\n");
        return 1;
    }

    class Notify : NotifdCommand {
        class UrgencyLevel : Opt {
            public Urgency urgency { get; set; default = Urgency.NORMAL; }

            construct {
                parse.connect((value) => {
                    switch (value) {
                        case "low": urgency = Urgency.LOW; return null;
                        case "normal": urgency = Urgency.NORMAL; return null;
                        case "critical": urgency = Urgency.CRITICAL; return null;
                        default: return "Level must be one of: 'low', 'normal', 'critical'";
                    }
                });
            }

            public UrgencyLevel(string long, char short, string description) {
                Object(long: long, short: short, description: description);
                name = "LEVEL";
            }
        }

        class Hints : Opt {
            public HashTable<string, Variant> hints = new HashTable<string, Variant>(str_hash, str_equal);

            construct {
                parse.connect((value) => {
                    var parts = value.split(":", 3);
                    if (parts.length != 3) {
                        return "Hints should be in TYPE:NAME:VALUE format";
                    }
                    var vtype = parts[0];
                    var name = parts[1];
                    try {
                        switch (vtype) {
                            case "boolean": {
                                switch (parts[2]) {
                                    case "true":
                                        hints.set(name, new Variant.boolean(true));
                                        break;
                                    case "false":
                                        hints.set(name, new Variant.boolean(false));
                                        break;
                                    default: return "Invalid boolean value. Must be 'true' or 'false'";
                                }
                                return null;
                            }
                            case "int": {
                                hints.set(name, new Variant.int32(int.parse(parts[2])));
                                return null;
                            }
                            case "double": {
                                hints.set(name, new Variant.double(double.parse(parts[2])));
                                return null;
                            }
                            case "string": {
                                hints.set(name, new Variant.string(parts[2]));
                                return null;
                            }
                            case "variant": {
                                hints.set(name, Variant.parse(VariantType.VARIANT, parts[2]));
                                return null;
                            }
                            default: return @"Invalid hint type '$vtype'. Must be one of: boolean, int, double, string, variant";
                        }
                    } catch (Error error) {
                        return error.message;
                    }
                });
            }

            public Hints(string long, char short, string description) {
                Object(long: long, short: short, description: description);
                name = "TYPE:NAME:VALUE";
            }
        }

        class Actions : Opt {
            public AstalNotifd.Action[] actions = {};

            construct {
                parse.connect((value) => {
                    var id_text = value.split("=", 2);
                    var id = (id_text.length == 2) ? id_text[0] : actions.length.to_string();
                    var text = (id_text.length == 2) ? id_text[1] : id_text[0];
                    actions += new AstalNotifd.Action(id, text);
                });
            }

            public Actions(string long, char short, string description) {
                Object(long: long, short: short, description: description);
                name = "[ID=]Text";
            }
        }

        [DBus(name = "org.freedesktop.Notifications")]
        interface OrgFreedesktopNotifications : Object {
            public abstract void close_notification(uint id) throws DBusError, IOError;

            public static void close(uint id) {
                try {
                    OrgFreedesktopNotifications proxy = Bus.get_proxy_sync(
                        BusType.SESSION,
                        "org.freedesktop.Notifications",
                        "/org/freedesktop/Notifications"
                    );

                    proxy.close_notification(id);
                } catch (Error error) {
                    printerr("%s\n", error.message);
                }
            }
        }

        Flag wait;
        Flag print_id;
        IntOpt replace_id;
        IntOpt expire_time;
        StringOpt app_name;
        StringOpt app_icon;
        UrgencyLevel urgency;
        Actions actions;
        FileOpt image;
        Flag action_icons;
        StringOpt category;
        StringOpt desktop_entry;
        Flag resident;
        FileOpt sound_file;
        StringOpt sound_name;
        Flag suppress_sound;
        Flag transient;
        Hints hints;

        public Notify() {
            name = "notify";
            about("Send a notification");
            required_arg("SUMMARY", "The summary of the notification.");
            arg("BODY", "The body of the notification.");
            opt(wait = new Flag("wait", 'w', "Wait for the notification to be closed before exiting."));
            opt(print_id = new Flag("print-id", 'p', "Print the notification ID."));
            opt(replace_id = new IntOpt("replace-id", 'r', "ID of the notification to replace.") {
                value = -1
            });
            opt(app_name = new StringOpt("app-name", 'n', "App name for the notification.") {
                value = "astal-notifd"
            });
            opt(app_icon = new StringOpt("app-icon", 'i', "Named icon or filename for the notification."));
            opt(urgency = new UrgencyLevel("urgency", 'u', "The urgency level (low, normal, critical)."));
            opt(expire_time = new IntOpt("expire-time", 'e', "Timeout in milliseconds at which to expire the notification.") {
                value = -1
            });
            opt(actions = new Actions("action", 'a', "Action to append to the notification. Implies --wait."));
            opt(image = new FileOpt("image", 'I', "Image to be displayed for the notification."));
            opt(action_icons = new Flag("action-icons", 'A', "Indicate that action IDs should be interpreted as named icons."));
            opt(category = new StringOpt("category", 'c', "The category of the notification."));
            opt(desktop_entry = new StringOpt("desktop-entry", 'd', "Name of the desktop filename representing the notification."));
            opt(resident = new Flag("resident", 'R', "Indicate that the notification is kept alive after action invocation."));
            opt(sound_file = new FileOpt("sound", 's', "Sound file to play when the notification pops up."));
            opt(sound_name = new StringOpt("sound-name", 'S', "Named sound to play when the notification pops up."));
            opt(suppress_sound = new Flag("suppress-sound", '\0', "Suppress playing any sound."));
            opt(transient = new Flag("transient", 't', "Indicate that the notification should be excluded from persistence."));
            opt(hints = new Hints("hint", 'H', "Valid types are boolean, int, double, string, and variant."));

            example("astal-notifd notify \"Hello World\" \"Lorem ipsum dolor sit amet, consectetur adipiscing elit.\"");
            example("astal-notifd notify \"Extra hints\" -H \"string:custom-hint:<{'hello':'there'}>\"");
            example("astal-notifd notify \"With Actions\" -a \"id1=My custom Action\"");
            example("astal-notifd notify \"Custom Variant hints\" -H \"variant:custom-hint:<{'hello':'there'}>\"");
        }

        public override async int execute() {
            var exit_code = 0;
            var loop = new MainLoop(null, false);

            var notification = new AstalNotifd.Notification() {
                id = replace_id.value,
                summary = args[0],
                expire_timeout = expire_time.value,
                app_name = app_name.value,
                urgency = urgency.urgency,
            };

            if (args.length > 1) {
                notification.body = args[1];
            }

            if (app_icon.value != null) {
                notification.app_icon = app_icon.value;
            }

            if (image.value != null) {
                var path = image.value.get_path();
                if (path != null) {
                    notification.image = path;
                }
            }

            if (action_icons.enabled) {
                notification.action_icons = true;
            }

            if (category.value != null) {
                notification.category = category.value;
            }

            if (desktop_entry.value != null) {
                notification.category = desktop_entry.value;
            }

            if (resident.enabled) {
                notification.resident = true;
            }

            if (sound_file.value != null) {
                var path = sound_file.value.get_path();
                if (path != null) {
                    notification.sound_file = path;
                }
            }

            if (sound_name.value != null) {
                notification.sound_name = sound_name.value;
            }

            if (suppress_sound.enabled) {
                notification.suppress_sound = true;
            }

            if (transient.enabled) {
                notification.transient = true;
            }

            // notify-send uses int64
            notification.set_hint("sender-pid", new Variant.int64(Posix.getpid()));

            hints.hints.foreach((name, value) => {
                notification.set_hint(name, value);
            });

            foreach (var action in actions.actions) {
                notification.add_action(action);
            }

            notification.invoked.connect((id) => {
                print("%s\n", id);
                loop.quit();
            });

            notification.resolved.connect(() => {
                loop.quit();
            });

            try {
                yield send_notification(notification);
            } catch (Error error) {
                return err(error.message);
            }

            if (print_id.enabled) {
                print(@"$(notification.id)\n");
            }

            if (wait.enabled || (actions.actions.length > 0)) {
                Unix.signal_add(Posix.Signal.HUP, () => {
                    OrgFreedesktopNotifications.close(notification.id);
                    return Source.REMOVE;
                });

                Unix.signal_add(Posix.Signal.INT, () => {
                    OrgFreedesktopNotifications.close(notification.id);
                    return Source.REMOVE;
                });

                Unix.signal_add(Posix.Signal.TERM, () => {
                    OrgFreedesktopNotifications.close(notification.id);
                    return Source.REMOVE;
                });

                loop.run();
            }

            return exit_code;
        }
    }

    class GetNotification : NotifdCommand {
        Flag pretty;

        public GetNotification() {
            name = "get";
            about("Print a notification by its ID");
            opt(pretty = new Flag("pretty", 'p', "Pretty print JSON"));
            required_arg("ID", "Notification ID");
        }

        public override async int execute() {
            var notifd = Notifd.get_default();
            var id = uint.parse(args[0]);
            var notification = notifd.get_notification(id);

            if (notification != null) {
                var node = Json.gvariant_serialize(notification.serialize());
                var json = Json.to_string(node, pretty.enabled);
                print("%s\n", json);
            } else {
                return err(@"notification '$id' does not exist");
            }

            return 0;
        }
    }

    class DismissNotification : NotifdCommand {
        public DismissNotification() {
            name = "dismiss";
            about("Dismiss a notification by its ID");
            required_arg("ID", "Notification ID");
        }

        public override async int execute() {
            var notifd = Notifd.get_default();
            var id = uint.parse(args[0]);
            var n = notifd.get_notification(id);

            if (n != null) {
                n.dismiss();
                notifd.daemon?.flush_state();
            } else {
                return err(@"notification '$id' does not exist");
            }

            return 0;
        }
    }

    class InvokeNotification : NotifdCommand {
        public InvokeNotification() {
            name = "invoke";
            about("Invoke a notification action");
            required_arg("ID", "Notification ID");
            required_arg("ACTION", "Action ID");
        }

        public override async int execute() {
            var notifd = Notifd.get_default();
            var id = uint.parse(args[0]);
            var action = args[1];

            var n = notifd.get_notification(id);
            if (n != null) {
                n.invoke(action);
            } else {
                return err(@"notification '$id' does not exist");
            }

            return 0;
        }
    }

    class ListNotifications : NotifdCommand {
        Flag pretty;

        public ListNotifications() {
            name = "list";
            about("Print all notifications");
            opt(pretty = new Flag("pretty", 'p', "Pretty print JSON"));
        }

        public override async int execute() {
            var settings = new Settings("io.astal.notifd");
            var notifications = settings.get_value("notifications");
            var node = Json.gvariant_serialize(notifications);
            var json = Json.to_string(node, pretty.enabled);
            print("%s\n", json);
            return 0;
        }
    }

    class NotificationDaemon : NotifdCommand {
        Flag events;

        public NotificationDaemon() {
            name = "daemon";
            about("Start the notifd daemon or a proxy");
            opt(events = new Flag("events", 'e', "Print notified and resolved events."));
        }

        private void print_event(string event, string payload) {
            stdout.printf(@"{\"event\":\"$event\",\"payload\":$payload}\n");
            stdout.flush();
        }

        public override async int execute() {
            var notifd = Notifd.get_default();
            var loop = new MainLoop();

            if (events.enabled) {
                notifd.notified.connect((id) => {
                    var n = notifd.get_notification(id).serialize();
                    var payload = Json.gvariant_serialize_data(n, null);
                    print_event("notified", payload);
                });

                notifd.resolved.connect((id, reason) => {
                    print_event("resolved", @"{\"id\":$id,\"reason\":\"$reason\"}");
                });
            } else {
                notifd.notified.connect((id) => {
                    var n = notifd.get_notification(id).serialize();
                    stdout.printf("%s\n", Json.gvariant_serialize_data(n, null));
                    stdout.flush();
                });
            }

            Unix.signal_add(Posix.Signal.HUP, () => {
                loop.quit();
                return Source.REMOVE;
            });

            Unix.signal_add(Posix.Signal.INT, () => {
                loop.quit();
                return Source.REMOVE;
            });

            Unix.signal_add(Posix.Signal.TERM, () => {
                loop.quit();
                return Source.REMOVE;
            });

            loop.run();
            return 0;
        }
    }

    class ToggleDND : NotifdCommand {
        Flag enable;
        Flag disable;
        Flag toggle;

        public ToggleDND() {
            name = "dnd";
            about("Toggle 'do not disturb' state");
            opt(enable = new Flag("enable", 'e', "Enable 'do not disturb'"));
            opt(disable = new Flag("disable", 'd', "Disable 'do not disturb'"));
            opt(toggle = new Flag("toggle", 't', "Toggle 'do not disturb'"));
        }

        public override async int execute() {
            if (enable.enabled && disable.enabled && toggle.enabled) {
                return err("Flags are mutually exclusive");
            }

            if (enable.enabled) {
                Notifd.settings.set_boolean("dont-disturb", true); return 0;
            }

            if (disable.enabled) {
                Notifd.settings.set_boolean("dont-disturb", false);
                return 0;
            }

            var dnd = Notifd.settings.get_boolean("dont-disturb");

            if (toggle.enabled) {
                Notifd.settings.set_boolean("dont-disturb", !dnd);
                return 0;
            }

            print("%s\n", dnd ? "enabled" : "disabled");
            return 0;
        }
    }

    class CLI : NotifdCommand {
        SpecialFlag version;

        public CLI() {
            name = "astal-notifd";
            about("Notifd CLI");
            opt(help = new SpecialFlag("help", 'h', "Print help"));
            opt(version = new SpecialFlag("version", 'v', "Print version"));
            subcommand(new Notify().opt(help));
            subcommand(new GetNotification().opt(help));
            subcommand(new DismissNotification().opt(help));
            subcommand(new InvokeNotification().opt(help));
            subcommand(new ListNotifications().opt(help));
            subcommand(new NotificationDaemon().opt(help));
            subcommand(new ToggleDND().opt(help));
        }

        public override async int execute() {
            if (version.enabled) {
                print("%s\n", VERSION);
                return 0;
            }

            printerr("%s\n", Quarrel.help(this));
            return 1;
        }
    }

    static async int main(string[] argv) {
        Notifd.settings = new Settings("io.astal.notifd");

        try {
            var cmd = new CLI().parse(argv) as NotifdCommand;

            if (help.enabled) {
                print("%s\n", Quarrel.help(cmd));
                return 0;
            }

            return yield cmd.execute();
        } catch (ParseError parse_error) {
            return err(parse_error.message);
        } finally {
            Settings.sync();
        }
    }
}
