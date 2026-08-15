public enum AstalBrightness.Subsystem {
    LEDS,
    BACKLIGHT;

    internal string to_string() {
        switch (this) {
            case LEDS: return "leds";
            case BACKLIGHT: return "backlight";
            default: assert_not_reached();
        }
    }
}

public interface AstalBrightness.Device : Object {
    /** The device type. */
    public abstract Subsystem subsystem { get; }

    /** The name of the device. */
    public abstract string name { get; }

    /** Brightness percentage: `real_brightness / max_brightness`. */
    public abstract float brightness { get; set; }

    /** The brightness value as reported by sysfs. */
    public abstract uint real_brightness { get; set; }

    /** The maximum brightness value as reported by sysfs. */
    public abstract uint max_brightness { get; }

    internal string? read(string file) {
        try {
            string contents;
            FileUtils.get_contents(@"/sys/class/$subsystem/$name/$file", out contents, null);
            return contents.strip();
        } catch (FileError error) {
            critical(error.message);
            return null;
        }
    }
}
