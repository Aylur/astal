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
            display_device = new Device("/org/freedesktop/UPower/devices/DisplayDevice");

            return display_device;
        } catch (Error error) {
            critical(error.message);
        }
        return null;
    }

    private IUPowerDevice proxy;

    public Device(string path) throws Error {
        proxy = Bus.get_proxy_sync(BusType.SYSTEM, "org.freedesktop.UPower", path);
        proxy.g_properties_changed.connect(sync);
        sync();
    }

    public Type device_type { get; private set; }
    public string native_path { owned get; private set; }
    public string vendor { owned get; private set; }
    public string model { owned get; private set; }
    public string serial { owned get; private set; }
    public uint64 update_time { get; private set; }
    public bool power_supply { get; private set; }
    public bool has_history { get; private set; }
    public bool has_statistics { get; private set; }
    public bool online { get; private set; }
    public double energy { get; private set; }
    public double energy_empty { get; private set; }
    public double energy_full { get; private set; }
    public double energy_full_design { get; private set; }
    public double energy_rate { get; private set; }
    public double voltage { get; private set; }
    public int charge_cycles { get; private set; }
    public double luminosity { get; private set; }
    public int64 time_to_empty { get; private set; }
    public int64 time_to_full { get; private set;}
    public double percentage { get; private set; }
    public double temperature { get; private set; }
    public bool is_present { get; private set; }
    public State state { get; private set; }
    public bool is_rechargable { get; private set; }
    public double capacity { get; private set; }
    public Technology technology { get; private set; }
    public WarningLevel warning_level { get; private set; }
    public BatteryLevel battery_level { get; private set; }
    public string icon_name { owned get; private set; }

    public bool charging { get; private set; }
    public bool is_battery { get; private set; }
    public string battery_icon_name { get; private set; }
    public string device_type_name { get; private set; }
    public string device_type_icon { get; private set; }

    public void sync() {
        device_type = (Type)proxy.Type;
        native_path = proxy.native_path;
        vendor = proxy.vendor;
        model = proxy.model;
        serial = proxy.serial;
        update_time = proxy.update_time;
        power_supply = proxy.power_supply;
        has_history = proxy.has_history;
        has_statistics = proxy.has_statistics;
        online = proxy.online;
        energy = proxy.energy;
        energy_empty = proxy.energy_empty;
        energy_full = proxy.energy_full;
        energy_full_design = proxy.energy_full_design;
        energy_rate = proxy.energy_rate;
        voltage = proxy.voltage;
        charge_cycles = proxy.charge_cycles;
        luminosity = proxy.luminosity;
        time_to_empty = proxy.time_to_empty;
        time_to_full = proxy.time_to_full;
        percentage = proxy.percentage / 100;
        temperature = proxy.temperature;
        is_present = proxy.is_present;
        state = (State)proxy.state;
        is_rechargable = proxy.is_rechargable;
        capacity = proxy.capacity;
        technology = (Technology)proxy.technology;
        warning_level = (WarningLevel)proxy.warning_level;
        battery_level = (BatteryLevel)proxy.battery_level;
        icon_name = proxy.icon_name;

        charging = state == State.FULLY_CHARGED || state == State.CHARGING;
        is_battery = device_type != Type.UNKNOWN && device_type != Type.LINE_POWER;

        if (!is_battery) {
            battery_icon_name = "battery-missing-symbolic";
        } else if (percentage == 1.0 && charging) {
            battery_icon_name = "battery-level-100-charged";
        } else {
            var state = charging ? "-charging" : "";
            var level = (int)Math.round(percentage * 100);
            battery_icon_name = @"battery-level-$level$state-symbolic";
        }

        device_type_name = device_type.get_name();
        device_type_icon = device_type.get_icon_name();
    }
}

[CCode (type_signature = "u")]
public enum State {
    UNKNOWN,
    CHARGING,
    DISCHARGING,
    EMPTY,
    FULLY_CHARGED,
    PENDING_CHARGE,
    PENDING_DISCHARGE,
}

[CCode (type_signature = "u")]
public enum Technology {
    UNKNOWN,
    LITHIUM_ION,
    LITHIUM_POLYMER,
    LITHIUM_IRON_PHOSPHATE,
    LEAD_ACID,
    NICKEL_CADMIUM,
    NICKEL_METAL_HYDRIDE,
}

[CCode (type_signature = "u")]
public enum WarningLevel {
    UNKNOWN,
    NONE,
    DISCHARGING,
    LOW,
    CRITICIAL,
    ACTION,
}

[CCode (type_signature = "u")]
public enum BatteryLevel {
    UNKNOWN,
    NONE,
    LOW,
    CRITICIAL,
    NORMAL,
    HIGH,
    FULL,
}

[CCode (type_signature = "u")]
public enum Type {
    UNKNOWN,
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
    BLUETOOTH_GENERIC;

    // TODO: add more icon names
    public string? get_icon_name () {
        switch (this) {
            case UPS:
                return "uninterruptible-power-supply";
            case MOUSE:
                return "input-mouse";
            case KEYBOARD:
                return "input-keyboard";
            case PDA:
            case PHONE:
                return "phone";
            case MEDIA_PLAYER:
                return "multimedia-player";
            case TABLET:
            case PEN:
                return "input-tablet";
            case GAMING_INPUT:
                return "input-gaming";
            default:
                return null;
        }
    }

    public unowned string? get_name () {
        switch (this) {
            case LINE_POWER:
                return "Plugged In";
            case BATTERY:
                return "Battery";
            case UPS:
                return "UPS";
            case MONITOR:
                return "Display";
            case MOUSE:
                return "Mouse";
            case KEYBOARD:
                return "Keyboard";
            case PDA:
                return "PDA";
            case PHONE:
                return "Phone";
            case MEDIA_PLAYER:
                return "Media Player";
            case TABLET:
                return "Tablet";
            case COMPUTER:
                return "Computer";
            case GAMING_INPUT:
                return "Controller";
            case PEN:
                return "Pen";
            case TOUCHPAD:
                return "Touchpad";
            case MODEM:
                return "Modem";
            case NETWORK:
                return "Network";
            case HEADSET:
                return "Headset";
            case SPEAKERS:
                return "Speakers";
            case HEADPHONES:
                return "Headphones";
            case VIDEO:
                return "Video";
            case OTHER_AUDIO:
                return "Other Audio";
            case REMOVE_CONTROL:
                return "Remove Control";
            case PRINTER:
                return "Printer";
            case SCANNER:
                return "Scanner";
            case CAMERA:
                return "Camera";
            case WEARABLE:
                return "Wearable";
            case TOY:
                return "Toy";
            case BLUETOOTH_GENERIC:
                return "Bluetooth Generic";
            default:
                return "Unknown";
        }
    }
}
}
