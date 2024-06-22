namespace AstalBattery{
internal string pascal_to_kebab_case(string pascal) {
    StringBuilder kebab = new StringBuilder();

    for (int i = 0; i < pascal.length; i++) {
        char c = pascal[i];
        if (c.isupper()) {
            if (i != 0)
                kebab.append_c('-');

            kebab.append_c(c.tolower());
        } else {
            kebab.append_c(c);
        }
    }

    return kebab.str;
}

internal string to_json(AstalBattery.Device device) {
    string s = "unknown";
    if (device.state == AstalBattery.DeviceState.CHARGING)
        s = "charging";
    if (device.state == AstalBattery.DeviceState.DISCHARGING)
        s = "discharging";
    if (device.state == AstalBattery.DeviceState.FULLY_CHARGED)
        s = "fully_charged";

    var p = device.percentage;
    var i = device.icon_name;
    var r = device.state == AstalBattery.DeviceState.CHARGING
        ? device.time_to_full : device.time_to_empty;

    return "{ \"percentage\": %f, \"state\": \"%s\", \"icon_name\": \"%s\", \"time_remaining\": %f }".printf(p, s, i, r);
}
}
