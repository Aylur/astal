namespace AstalBattery {
public Device get_default() {
    return Device.get_default();
}

public class Device : Object {
    private static Device display_device;
    public static Device? get_default() {
        if (display_device != null)
            return display_device;

        try {
            display_device = new Device(
                    "/org/freedesktop/UPower/devices/DisplayDevice");

            return display_device;
        } catch (Error error) {
            critical(error.message);
        }
        return null;
    }

    private Properties props;
    private IUPowerDevice proxy;

    public DeviceType device_type {
        get {
            Value value = Value(Type.UINT);
            proxy.get_property("name", ref value);
            return value.get_uint();
        }
    }

    public string native_path { owned get { return proxy.native_path; } }
    public string vendor { owned get { return proxy.vendor; } }
    public string model { owned get { return proxy.model; } }
    public string serial { owned get { return proxy.serial; } }
    public uint64 update_time { get { return proxy.update_time; } }
    public bool power_supply { get { return proxy.power_supply; } }
    public bool has_history { get { return proxy.has_history; } }
    public bool has_statistics { get { return proxy.has_statistics; } }
    public bool online { get { return proxy.online; } }
    public double energy { get { return proxy.energy; } }
    public double energy_empty { get { return proxy.energy_empty; } }
    public double energy_full { get { return proxy.energy_full; } }
    public double energy_full_design { get { return proxy.energy_full_design; } }
    public double energy_rate { get { return proxy.energy_rate; } }
    public double voltage { get { return proxy.voltage; } }
    public int charge_cycles { get { return proxy.charge_cycles; } }
    public double luminosity { get { return proxy.luminosity; } }
    public int64 time_to_empty { get { return proxy.time_to_empty; } }
    public int64 time_to_full { get { return proxy.time_to_full; }}
    public double percentage { get { return proxy.percentage / 100; } }
    public double temperature { get { return proxy.temperature; } }
    public bool is_present { get { return proxy.is_present; } }
    public DeviceState state { get { return proxy.state; } }
    public bool is_rechargable { get { return proxy.is_rechargable; } }
    public double capacitiy { get { return proxy.capacitiy; } }
    public DeviceTechnology technology { get { return proxy.technology; } }
    public WarningLevel warning_level { get { return proxy.warning_level; } }
    public BatteryLevel battery_level { get { return proxy.battery_level; } }
    public string icon_name { owned get { return proxy.icon_name; } }

    public Device(string path) throws Error {
        proxy = Bus.get_proxy_sync(BusType.SYSTEM, "org.freedesktop.UPower", path);
        props = Bus.get_proxy_sync(BusType.SYSTEM, "org.freedesktop.UPower", path);

        props.properties_changed.connect((iface, vardict) => {
            foreach (var key in vardict.get_keys()) {
                var prop = pascal_to_kebab_case(key);
                if (get_class().find_property(prop) != null)
                    notify_property(prop);
            }
        });
    }
}

public enum DeviceType {
    UNKNOWN = 0,
    LINE_POWER,
    BATTERY,
    UPS,
    MONITOR,
    MOUSE,
    KEYBOARD,
    PDA,
    PHONE,
    MEDIA_PLAYER,
    TABLET,
    COMPUTER,
    GAMING_INPUT,
    PEN,
    TOUCHPAD,
    MODEM,
    NETWORK,
    HEADSET,
    SPEAKERS,
    HEADPHONES,
    VIDEO,
    OTHER_AUDIO,
    REMOVE_CONTROL,
    PRINTER,
    SCANNER,
    CAMERA,
    WEARABLE,
    TOY,
    BLUETOOTH_GENERIC,
}

public enum DeviceState {
    UNKNOWN = 0,
    CHARGING,
    DISCHARGING,
    EMPTY,
    FULLY_CHARGED,
    PENDING_CHARGE,
    PENDING_DISCHARGE,
}

public enum DeviceTechnology {
    UNKNOWN = 0,
    LITHIUM_ION,
    LITHIUM_POLYMER,
    LITHIUM_IRON_PHOSPHATE,
    LEAD_ACID,
    NICKEL_CADMIUM,
    NICKEL_METAL_HYDRIDE,
}

public enum WarningLevel {
    UNKNOWN = 0,
    NONE,
    DISCHARGING,
    LOW,
    CRITICIAL,
    ACTION,
}

public enum BatteryLevel {
    UNKNOWN = 0,
    NONE,
    LOW,
    CRITICIAL,
    NORMAL,
    HIGH,
    FULL,
}
}
