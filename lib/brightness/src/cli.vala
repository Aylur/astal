using AstalBrightness;
using Quarrel;

abstract class BrightnessCommand : Command {
    static SpecialFlag help;

    public abstract async int execute();

    protected static int err(string msg) {
        printerr(@"\x1b[1;31merror:\x1b[0m $msg\n");
        return 1;
    }

    static Json.Node device_to_json(Device device) {
        return new Json.Builder()
            .begin_object()
            .set_member_name("name")
            .add_string_value(device.name)
            .set_member_name("subsystem")
            .add_string_value(device.subsystem.to_string())
            .set_member_name("brightness")
            .add_int_value(device.real_brightness)
            .set_member_name("max_brightness")
            .add_int_value(device.max_brightness)
            .end_object()
            .get_root();
    }

    class ListDevices : BrightnessCommand {
        Flag pretty;

        public ListDevices() {
            name = "list";
            about("Print a list of devices and their current states in JSON format");
            opt(pretty = new Flag("pretty", 'p', "Pretty print JSON"));
        }

        public override async int execute() {
            var brightness = Brightness.get_default();
            var devices = new Json.Builder().begin_array();

            foreach (var backlight in brightness.backlights.devices) {
                devices.add_value(device_to_json(backlight));
            }

            foreach (var led in brightness.leds.devices) {
                devices.add_value(device_to_json(led));
            }

            print("%s\n", Json.to_string(devices.end_array().get_root(), pretty.enabled));
            return 0;
        }
    }

    class SubsystemOpt : Opt {
        public Subsystem value { get; set; default = Subsystem.BACKLIGHT; }

        construct {
            name = "SUBSYSTEM";
            long = "subsystem";
            short = 's';
            description = "One of: 'leds' or 'backlight'. Default: 'backlight'";

            parse.connect((str) => {
                switch (str) {
                    case "leds":
                        value = Subsystem.LEDS;
                        break;
                    case "backlight":
                        value = Subsystem.BACKLIGHT;
                        break;
                    default:
                        return "Subsystem must be one of: 'leds' or 'backlight'";
                }
            });
        }
    }

    class SetDevice : BrightnessCommand {
        SubsystemOpt subsystem;
        StringOpt device_name;

        struct Operation {
            double value;
            bool is_percentage;
            bool is_positive_delta;
            bool is_negative_delta;
        }

        bool parse_operation(string input, out Operation result) {
            result = Operation();
            var s = input;

            result.is_positive_delta = s.has_prefix("+");
            if (result.is_positive_delta) {
                s = s.substring(1);
            }

            result.is_negative_delta = s.has_suffix("-");
            if (result.is_negative_delta) {
                s = s.substring(0, s.length - 1);
            }

            result.is_percentage = s.has_suffix("%");
            if (result.is_percentage) {
                s = s.substring(0, s.length - 1);
            }

            if (s == "") return false;
            result.value = double.parse(s);

            return true;
        }

        public SetDevice() {
            name = "set";
            about("Set the brightness of a device");
            opt(subsystem = new SubsystemOpt());
            opt(device_name = new StringOpt("name", 'n', "Name of the device") {
                name = "NAME",
            });
            required_arg("VALUE", "Specific brightness value or delta");
            example("astal-brightness set 155");
            example("astal-brightness set 10%");
            example("astal-brightness set +20%");
            example("astal-brightness set 20%-");
        }

        public override async int execute() {
            try {
                Operation operation;
                Device device;

                if (device_name.value != null) {
                    var name = device_name.value;
                    var system = subsystem.value;

                    if (!FileUtils.test(@"/sys/class/$system/$name", FileTest.EXISTS)) {
                        return err("No such device exists");
                    }

                    device = new BrightnessDevice(system, name);
                } else if (subsystem.value == Subsystem.BACKLIGHT) {
                    device = Brightness.get_default().screen;
                } else {
                    device = Brightness.get_default().keyboard;
                }

                if (!parse_operation(args[0], out operation)) {
                    return err("Invalid brightness value");
                }

                if (operation.is_percentage) {
                    var value = (operation.value > 0) ? operation.value / 100 : 0;
                    if (operation.is_negative_delta) {
                        device.brightness -= (float)(device.brightness * value);
                    } else if (operation.is_positive_delta) {
                        device.brightness += (float)(device.brightness * value);
                    } else {
                        device.brightness = (float)value;
                    }
                } else {
                    if (operation.is_negative_delta) {
                        device.real_brightness -= (uint)operation.value;
                    } else if (operation.is_positive_delta) {
                        device.real_brightness += (uint)operation.value;
                    } else {
                        device.real_brightness = (uint)(operation.value);
                    }
                }
            } catch (Error error) {
                return err(error.message);
            }

            return 0;
        }
    }

    class GetDevice : BrightnessCommand {
        Flag pretty;
        SubsystemOpt subsystem;

        public GetDevice() {
            name = "get";
            about("Get the current state of a device");
            required_arg("NAME", "Name of the device");
            opt(subsystem = new SubsystemOpt());
            opt(pretty = new Flag("pretty", 'p', "Pretty print JSON"));
        }

        public override async int execute() {
            var system = subsystem.value;
            var name = args[0];

            if (!FileUtils.test(@"/sys/class/$system/$name", FileTest.EXISTS)) {
                return err("No such device exists");
            }

            try {
                var device = new BrightnessDevice(system, name);
                print("%s\n", Json.to_string(device_to_json(device), pretty.enabled));
            } catch (Error error) {
                return err(error.message);
            }

            return 0;
        }
    }

    class Monitor : BrightnessCommand {
        StringArrayOpt filter;

        public Monitor() {
            name = "monitor";
            about("Monitor for brightness changes");
            opt(filter = new StringArrayOpt("filter", 'f', "Device filter") {
                name = "SUBSYSTEM/NAME",
            });
            example("astal-brightness monitor -f backlight/amdgpu_bl0 -f leds/dell::kbd_backlight");
        }

        bool should_print(Device device) {
            if (filter.value.length == 0) return true;

            foreach (var f in filter.value) {
                var parts = f.split("/", 2);
                var subsystem = (parts.length == 2) ? parts[0] : "backlight";
                var name = (parts.length == 2) ? parts[1] : f;

                if ((device.subsystem.to_string() == subsystem) && (device.name == name)) {
                    return true;
                }
            }

            return false;
        }

        public override async int execute() {
            var brightness = Brightness.get_default();
            var loop = new MainLoop();

            brightness.brightness_changed.connect((device) => {
                if (should_print(device)) {
                    stdout.printf("%s\n", Json.to_string(device_to_json(device), false));
                    stdout.flush();
                }
            });

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

    class CLI : BrightnessCommand {
        SpecialFlag version;

        public CLI() {
            name = "astal-brightness";
            about("Read and control device brightness");
            opt(help = new SpecialFlag("help", 'h', "Print help"));
            opt(version = new SpecialFlag("version", 'v', "Print version"));
            subcommand(new ListDevices().opt(help));
            subcommand(new SetDevice().opt(help));
            subcommand(new GetDevice().opt(help));
            subcommand(new Monitor().opt(help));
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
        try {
            var cmd = new CLI().parse(argv) as BrightnessCommand;

            if (help.enabled) {
                print("%s\n", Quarrel.help(cmd));
                return 0;
            }

            return yield cmd.execute();
        } catch (ParseError parse_error) {
            return err(parse_error.message);
        }
    }
}
