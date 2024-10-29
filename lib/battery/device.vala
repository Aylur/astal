namespace AstalBattery {
    /** Get the DisplayDevice. */
    public Device get_default() {
        return Device.get_default();
    }
}

/**
 * Client for a UPower [[https://upower.freedesktop.org/docs/Device.html|device]].
 */
public class AstalBattery.Device : Object {
    private static Device display_device;

    /** Get the DisplayDevice. */
    public static Device? get_default() {
        if (display_device != null) {
            return display_device;
        }

        try {
            display_device = new Device((ObjectPath)"/org/freedesktop/UPower/devices/DisplayDevice");
            return display_device;
        } catch (Error error) {
            critical(error.message);
        }

        return null;
    }

    private IUPowerDevice proxy;

    public Device(ObjectPath path) throws Error {
        proxy = Bus.get_proxy_sync(BusType.SYSTEM, "org.freedesktop.UPower", path);
        proxy.g_properties_changed.connect(sync);
        sync();
    }

    /**
     * If it is [enum@AstalBattery.Type.BATTERY], you will need to verify that the
     * property power-supply has the value `true` before considering it as a laptop battery.
     * Otherwise it will likely be the battery for a device of an unknown type.
     */
    public Type device_type { get; private set; }

    /**
     * Native path of the power source. This is the sysfs path,
     * for example /sys/devices/LNXSYSTM:00/device:00/PNP0C0A:00/power_supply/BAT0.
     * It is blank if the device is being driven by a user space driver.
     */
    public string native_path { owned get; private set; }

    /** Name of the vendor of the battery. */
    public string vendor { owned get; private set; }

    /** Name of the model of this battery. */
    public string model { owned get; private set; }

    /** Unique serial number of the battery. */
    public string serial { owned get; private set; }

    /**
     * The point in time (seconds since the Epoch)
     * that data was read from the power source.
     */
    public uint64 update_time { get; private set; }

    /**
     * If the power device is used to supply the system.
     * This would be set `true` for laptop batteries and UPS devices,
     * but set to `false` for wireless mice or PDAs.
     */
    public bool power_supply { get; private set; }

    /** If the power device has history. */
    // TODO: public bool has_history { get; private set; }

    /** If the power device has statistics. */
    // TODO: public bool has_statistics { get; private set; }

    /**
     * Whether power is currently being provided through line power.
     *
     * This property is only valid if [property@AstalBattery.Device:device_type] is [enum@AstalBattery.Type.LINE_POWER].
     */
    public bool online { get; private set; }

    /**
     * Amount of energy (measured in Wh) currently available in the power source.
     *
     * This property is only valid if [property@AstalBattery.Device:device_type] is [enum@AstalBattery.Type.BATTERY].
     */
    public double energy { get; private set; }

    /**
     * Amount of energy (measured in Wh) in the power source when it's considered to be empty.
     *
     * This property is only valid if [property@AstalBattery.Device:device_type] is [enum@AstalBattery.Type.BATTERY].
     */
    public double energy_empty { get; private set; }

    /**
     * Amount of energy (measured in Wh) in the power source when it's considered full.
     *
     * This property is only valid if [property@AstalBattery.Device:device_type] is [enum@AstalBattery.Type.BATTERY].
     */
    public double energy_full { get; private set; }

    /**
     * Amount of energy (measured in Wh) the power source is designed to hold when it's considered full.
     *
     * This property is only valid if [property@AstalBattery.Device:device_type] is [enum@AstalBattery.Type.BATTERY].
     */
    public double energy_full_design { get; private set; }

    /**
     * Amount of energy being drained from the source, measured in W.
     * If positive, the source is being discharged, if negative it's being charged.
     *
     * This property is only valid if [property@AstalBattery.Device:device_type] is [enum@AstalBattery.Type.BATTERY].
     */
    public double energy_rate { get; private set; }

    /** Voltage in the Cell or being recorded by the meter. */
    public double voltage { get; private set; }

    /**
     * The number of charge cycles as defined by the TCO certification,
     * or -1 if that value is unknown or not applicable.
     */
    public int charge_cycles { get; private set; }

    /** Luminosity being recorded by the meter. */
    public double luminosity { get; private set; }

    /**
     * Number of seconds until the power source is considered empty. Is set to 0 if unknown.
     *
     * This property is only valid if [property@AstalBattery.Device:device_type] is [enum@AstalBattery.Type.BATTERY].
     */
    public int64 time_to_empty { get; private set; }

    /**
     * Number of seconds until the power source is considered full. Is set to 0 if unknown.
     *
     * This property is only valid if [property@AstalBattery.Device:device_type] is [enum@AstalBattery.Type.BATTERY].
     */
    public int64 time_to_full { get; private set;}

    /**
     * The amount of energy left in the power source expressed as a percentage between 0 and 1.
     *
     * This property is only valid if [property@AstalBattery.Device:device_type] is [enum@AstalBattery.Type.BATTERY].
     * The percentage will be an approximation if [property@AstalBattery.Device:battery_level]
     * is set to something other than None.
     */
    public double percentage { get; private set; }

    /**
     * The temperature of the device in degrees Celsius.
     *
     * This property is only valid if [property@AstalBattery.Device:device_type] is [enum@AstalBattery.Type.BATTERY].
     */
    public double temperature { get; private set; }

    /**
     * If the power source is present in the bay.
     *
     * This property is only valid if [property@AstalBattery.Device:device_type] is [enum@AstalBattery.Type.BATTERY].
     */
    public bool is_present { get; private set; }

    /**
     * The battery power state.
     *
     * This property is only valid if [property@AstalBattery.Device:device_type] is [enum@AstalBattery.Type.BATTERY].
     */
    public State state { get; private set; }

    /**
     * If the power source is rechargeable.
     *
     * This property is only valid if [property@AstalBattery.Device:device_type] is [enum@AstalBattery.Type.BATTERY].
     */
    public bool is_rechargable { get; private set; }

    /**
     * The capacity of the power source expressed as a percentage between 0 and 1.
     *
     * This property is only valid if [property@AstalBattery.Device:device_type] is [enum@AstalBattery.Type.BATTERY].
     */
    public double capacity { get; private set; }

    /**
     * Technology used in the battery:
     *
     * This property is only valid if [property@AstalBattery.Device:device_type] is [enum@AstalBattery.Type.BATTERY].
     */
    public Technology technology { get; private set; }

    /** Warning level of the battery. */
    public WarningLevel warning_level { get; private set; }

    /**
     * The level of the battery for devices which do not report a percentage
     * but rather a coarse battery level. If the value is None.
     * then the device does not support coarse battery reporting,
     * and the [property@AstalBattery.Device:percentage] should be used instead.
     */
    public BatteryLevel battery_level { get; private set; }

    /**
     * An icon name representing this Device.
     *
     * NOTE: [property@AstalBattery.Device:battery_icon_name] might be a better fit
     * as it is calculated from percentage.
     */
    public string icon_name { owned get; private set; }

    /**
     * Indicates if [property@AstalBattery.Device:state] is charging or fully charged.
     */
    public bool charging { get; private set; }

    /**
     * Indicates if [property@AstalBattery.Device:device_type] is not line power or unknown.
     */
    public bool is_battery { get; private set; }

    /**
     * An icon name in the form of "battery-level-$percentage-$state-symbolic".
     */
    public string battery_icon_name { get; private set; }

    /**
     * A string representation of this device's [property@AstalBattery.Device:device_type].
     */
    public string device_type_name { get; private set; }

    /**
     * An icon name that can be used to represent this device's [property@AstalBattery.Device:device_type].
     */
    public string device_type_icon { get; private set; }

    // TODO: get_history
    // TODO: get_statistics

    private void sync() {
        device_type = (Type)proxy.Type;
        native_path = proxy.native_path;
        vendor = proxy.vendor;
        model = proxy.model;
        serial = proxy.serial;
        update_time = proxy.update_time;
        power_supply = proxy.power_supply;
        // TODO: has_history = proxy.has_history;
        // TODO: has_statistics = proxy.has_statistics;
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
        capacity = proxy.capacity / 100;
        technology = (Technology)proxy.technology;
        warning_level = (WarningLevel)proxy.warning_level;
        battery_level = (BatteryLevel)proxy.battery_level;
        icon_name = proxy.icon_name;

        charging = state == State.FULLY_CHARGED || state == State.CHARGING;
        is_battery = device_type != Type.UNKNOWN && device_type != Type.LINE_POWER;

        if (!is_battery) {
            battery_icon_name = "battery-missing-symbolic";
        } else if (percentage >= 0.95 && charging) {
            battery_icon_name = "battery-level-100-charged-symbolic";
        } else {
            var state = charging ? "-charging" : "";
            var level = (int)Math.round(percentage * 10)*10;
            battery_icon_name = @"battery-level-$level$state-symbolic";
        }

        device_type_name = device_type.get_name();
        device_type_icon = device_type.get_icon_name();
    }
}

[CCode (type_signature = "u")]
public enum AstalBattery.State {
    UNKNOWN,
    CHARGING,
    DISCHARGING,
    EMPTY,
    FULLY_CHARGED,
    PENDING_CHARGE,
    PENDING_DISCHARGE,
}

[CCode (type_signature = "u")]
public enum AstalBattery.Technology {
    UNKNOWN,
    LITHIUM_ION,
    LITHIUM_POLYMER,
    LITHIUM_IRON_PHOSPHATE,
    LEAD_ACID,
    NICKEL_CADMIUM,
    NICKEL_METAL_HYDRIDE,
}

[CCode (type_signature = "u")]
public enum AstalBattery.WarningLevel {
    UNKNOWN,
    NONE,
    DISCHARGING,
    LOW,
    CRITICIAL,
    ACTION,
}

[CCode (type_signature = "u")]
public enum AstalBattery.BatteryLevel {
    UNKNOWN,
    NONE,
    LOW,
    CRITICIAL,
    NORMAL,
    HIGH,
    FULL,
}

[CCode (type_signature = "u")]
public enum AstalBattery.Type {
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
    internal string? get_icon_name () {
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

    internal unowned string? get_name () {
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
