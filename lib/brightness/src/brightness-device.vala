class AstalBrightness.BrightnessDevice : Object, Device {
    FileMonitor monitor;
    uint prev_real_brightness;

    public Subsystem subsystem { get; }
    public string name { get; }

    public float brightness {
        get {
            if (max_brightness == 0) {
                critical("max_brightness == 0");
                return 0;
            }

            return (float)real_brightness / (float)max_brightness;
        }
        set {
            real_brightness = (uint)Math.roundf(max_brightness * value);
        }
    }

    public uint real_brightness {
        get {
            return uint.parse(read("brightness") ?? "0");
        }
        set {
            if (max_brightness == 0) {
                critical("max_brightness == 0");
                return;
            }

            write_brightness(value > max_brightness ? max_brightness : value < 0 ? 0 : value);
        }
    }

    public uint max_brightness {
        get {
            return uint.parse(read("max_brightness") ?? "0");
        }
    }

    internal BrightnessDevice(Subsystem subsystem, string name) throws IOError {
        this._subsystem = subsystem;
        this._name = name;

        monitor = File.new_for_path(@"/sys/class/$subsystem/$name/brightness").monitor_file(
            FileMonitorFlags.NONE,
            null
        );

        prev_real_brightness = real_brightness;

        monitor.changed.connect(() => {
            if (prev_real_brightness != real_brightness) {
                prev_real_brightness = real_brightness;
                notify_property("real-brightness");
                notify_property("brightness");
            }
        });
    }

    ~BrightnessDevice() {
        if (monitor != null) {
            monitor.cancel();
        }
    }

#if UDEV
    private void write_brightness(uint value) {
        try {
            FileUtils.set_contents(@"/sys/class/$subsystem/$name/brightness", @"$value");
        } catch (FileError error) {
            critical(error.message);
        }
    }
#else
    private void write_brightness(uint value) {
        try {
            var conn = Bus.get_sync(BusType.SYSTEM);

            conn.call_sync(
                "org.freedesktop.login1",
                "/org/freedesktop/login1/session/auto",
                "org.freedesktop.login1.Session",
                "SetBrightness",
                new Variant("(ssu)", @"$subsystem", name, value),
                new VariantType("()"),
                DBusCallFlags.NONE,
                -1
            );
        } catch (Error error) {
            critical(error.message);
        }
    }
#endif
}
