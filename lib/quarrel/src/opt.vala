namespace Quarrel {
/**
 * Base type for command-line options parsed by [class@Command].
 */
public class Opt : Object {
    /** Human-readable description shown in generated help output. */
    public string? description { get; set; }

    /** Placeholder name shown for the option value in help output. */
    public string? name { get; set; }

    /** Long option name, used as `--name`. */
    public string? long { get; set; }

    /** Short option name, used as `-n`. */
    public char short { get; set; default = '\0'; }

    /**
     * Parse and store a value received for this option.
     * Returns optional parse error string.
     */
    public signal string? parse(string value);
}

/**
 * Boolean flag option that becomes `enabled` when present.
 */
public sealed class Flag : Opt {
    /** Parsed flag state. */
    public bool enabled { get; set; }

    construct {
        parse.connect(() => { _enabled = true; });
    }

    /** Create a flag option. */
    public Flag(string? long = null, char short = '\0', string? description = null) {
        Object(long: long, short: short, description: description);
    }
}

/**
 * Similar to [class@Flag] which also turns of validating positional arguments.
 * Used for flags such as `--version` and `--help`.
 */
public sealed class SpecialFlag : Opt {
    /** Parsed flag state. */
    public bool enabled { get; set; }

    construct {
        parse.connect(() => { _enabled = true; });
    }

    /** Create a flag option. */
    public SpecialFlag(string? long = null, char short = '\0', string? description = null) {
        Object(long: long, short: short, description: description);
    }
}

/**
 * Option that stores a single string value.
 */
public sealed class StringOpt : Opt {
    /** Parsed string value. */
    public string? value { get; set; }

    construct {
        parse.connect((value) => { _value = value; });
    }

    /** Create a string-valued option. */
    public StringOpt(string? long = null, char short = '\0', string? description = null) {
        Object(long: long, short: short, description: description, name: "STRING");
    }
}

/**
 * Option that stores a single integer value.
 */
public sealed class IntOpt : Opt {
    /** Parsed integer value. */
    public int value { get; set; }

    construct {
        parse.connect(on_parse);
    }

    private string? on_parse(string value) {
        int parsed;
        if (!int.try_parse(value, out parsed)) {
            return @"invalid integer: $value";
        }
        _value = parsed;
        return null;
    }

    /** Create an integer-valued option. */
    public IntOpt(string? long = null, char short = '\0', string? description = null) {
        Object(long: long, short: short, description: description, name: "INT");
    }
}

/**
 * Option that stores a single floating-point value.
 */
public sealed class FloatOpt : Opt {
    /** Parsed floating-point value. */
    public float value { get; set; }

    construct {
        parse.connect(on_parse);
    }

    /** Parse and store a floating-point value. */
    private string? on_parse(string value) {
        float parsed;
        if (!float.try_parse(value, out parsed)) {
            return @"invalid float: $value";
        }
        _value = parsed;
        return null;
    }

    /** Create a floating-point-valued option. */
    public FloatOpt(string? long = null, char short = '\0', string? description = null) {
        Object(long: long, short: short, description: description, name: "DOUBLE");
    }
}

/**
 * Option that stores a single file path as a [iface@Gio.File].
 */
public sealed class FileOpt : Opt {
    /** Parsed file value. */
    public File? value { get; set; }

    construct {
        parse.connect((value) => { _value = File.new_for_commandline_arg(value); });
    }

    /** Create a file-valued option. */
    public FileOpt(string? long = null, char short = '\0', string? description = null) {
        Object(long : long, short: short, description: description, name: "FILE");
    }
}

/**
 * Option that collects repeated file path values.
 */
public sealed class FileArrayOpt : Opt {
    private List<File> _list = new List<File>();

    /** Parsed file values in the order they were provided. */
    public List<File> value { get { return _list; } }

    construct {
        parse.connect((value) => { _list.append(File.new_for_commandline_arg(value)); });
    }

    /** Create a repeated file-valued option. */
    public FileArrayOpt(string? long = null, char short = '\0', string? description = null) {
        Object(long: long, short: short, description: description, name: "FILES");
    }
}

/**
 * Option that collects repeated string values.
 */
public sealed class StringArrayOpt : Opt {
    /** Parsed string values in the order they were provided. */
    public string[] value { get; default = {}; }

    construct {
        parse.connect((value) => { _value += value; });
    }

    /** Create a repeated string-valued option. */
    public StringArrayOpt(string? long = null, char short = '\0', string? description = null) {
        Object(long: long, short: short, description: description, name: "STRINGS");
    }
}
}
