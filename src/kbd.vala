namespace AstalBattery {
public KbdBacklight get_keyboard() {
    return KbdBacklight.get_default();
}

public class KbdBacklight : Object {
    private IUPowerKdbBacklight proxy;

    private static KbdBacklight instance;
    public static KbdBacklight? get_default() {
        if (instance != null)
            return instance;

        try {
            instance = new KbdBacklight();
            return instance;
        } catch (Error error) {
            critical(error.message);
            return null;
        }
    }

    public int brightness {
        get {
            try {
                return proxy.get_brightness();
            } catch (Error error) {
                critical(error.message);
                return 0;
            }
        }
        set {
            try {
                proxy.set_brightness(value);
            } catch (Error error) {
                critical(error.message);
            }
        }
    }

    public int max_brightness {
        get {
            try {
                return proxy.get_max_brightness();
            } catch (Error error) {
                critical(error.message);
                return 0;
            }
        }
    }

    public KbdBacklight() throws Error {
        proxy = Bus.get_proxy_sync(
            BusType.SYSTEM,
            "org.freedesktop.UPower",
            "/org/freedesktop/UPower/KbdBacklight"
        );

        proxy.brightness_changed.connect(() => {
            notify_property("brightness");
        });
    }
}
}
