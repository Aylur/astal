namespace AstalBrightness {
/**
 * Get the singleton Brightness instance.
 */
    public Brightness get_default() {
        return Brightness.get_default();
    }
}

/**
 * Manager object that exposes collections of `LEDS` and `BACKLIGHT` devices.
 * It also exposes proxy objects for the screen and keyboard devices guessed to be the main devices.
 */
public class AstalBrightness.Brightness : Object {
    private Brightness() {}
    private static Brightness? instance;

    /**
     * Get the singleton Brightness instance.
     */
    public static Brightness get_default() {
        if (instance == null)instance = new Brightness();
        return instance;
    }

    DeviceProxy _screen = new DeviceProxy();
    DeviceProxy _keyboard = new DeviceProxy();

    DeviceList _backlights = new DeviceList(Subsystem.BACKLIGHT);
    DeviceList _leds = new DeviceList(Subsystem.LEDS);

    /**
     * A proxy `BACKLIGHT` device object for the device that is guessed as the "main" screen.
     */
    public Device screen {
        get { return _screen; }
    }

    /**
     * A proxy `LEDS` device object for the device that is guessed as the "main" keyboard.
     */
    public Device keyboard {
        get { return _keyboard; }
    }

    /**
     * Collection of `BACKLIGHT` devices.
     */
    public DeviceList backlights {
        get { return _backlights; }
    }

    /**
     * Collection of `LEDS` devices.
     */
    public DeviceList leds {
        get { return _leds; }
    }

    /**
     * Emitted when any device in [property@AstalBrightness.Brightness:backlights] or [property@AstalBrightness.Brightness:leds] changes.
     */
    public signal void brightness_changed(Device device);

    void connect_list(DeviceList list) {
        foreach (var device in list.devices) {
            device.notify["brightness"].connect(() => {
                brightness_changed(device);
            });
        }

        list.device_appeared.connect((device) => {
            device.notify["brightness"].connect(() => {
                brightness_changed(device);
            });
        });
    }

    int score_screen(Device device) {
        if (device is DummyDevice)return 0;

        var score = 0;
        var type = device.read("type");
        var name = device.name;

        if (type == "raw") {
            score += 100;
        } else if (type == "platform") {
            score += 50;
        } else if (type == "firmware") {
            score += 25;
        }

        if (Regex.match_simple("intel|amdgpu|radeon|nvidia", name, RegexCompileFlags.CASELESS)) {
            score += 10;
        }

        if (Regex.match_simple("acpi_video|video|firmware", name, RegexCompileFlags.CASELESS)) {
            score -= 10;
        }

        return score;
    }

    int score_keyboard(Device device) {
        if (device is DummyDevice)return 0;

        var score = 0;
        var name = device.name;

        if (name == "kbd_backlight" || name.has_suffix("::kbd_backlight")) {
            score += 100;
        }

        if (Regex.match_simple("platform|asus|dell|thinkpad|apple|smc|tpacpi", name, RegexCompileFlags.CASELESS)) {
            score += 10;
        }

        if (Regex.match_simple("input|scrolllock|capslock|numlock", name, RegexCompileFlags.CASELESS)) {
            score -= 100;
        }

        return score;
    }

    void init_screen_proxy() {
        foreach (var device in _backlights.devices) {
            if (score_screen(device) > score_screen(_screen.proxied)) {
                _screen.proxied = device;
            }
        }

        _backlights.device_appeared.connect((device) => {
            if (score_screen(device) > score_screen(_screen.proxied)) {
                _screen.proxied = device;
            }
        });

        _backlights.device_removed.connect((device) => {
            if (_screen.proxied != device)return;

            _screen.proxied = new DummyDevice();
            foreach (var d in _backlights.devices) {
                if (d != device && score_screen(d) > score_screen(_screen.proxied)) {
                    _screen.proxied = d;
                }
            }
        });
    }

    void init_keyboard_proxy() {
        foreach (var device in _leds.devices) {
            if (score_keyboard(device) > score_keyboard(_keyboard.proxied)) {
                _keyboard.proxied = device;
            }
        }

        _leds.device_appeared.connect((device) => {
            if (score_keyboard(device) > score_keyboard(_keyboard.proxied)) {
                _keyboard.proxied = device;
            }
        });

        _leds.device_removed.connect((device) => {
            if (_keyboard.proxied != device)return;

            _keyboard.proxied = new DummyDevice();
            foreach (var d in _leds.devices) {
                if (d != device && score_keyboard(d) > score_keyboard(_keyboard.proxied)) {
                    _keyboard.proxied = d;
                }
            }
        });
    }

    construct {
        connect_list(_backlights);
        connect_list(_leds);
        init_screen_proxy();
        init_keyboard_proxy();
    }
}
