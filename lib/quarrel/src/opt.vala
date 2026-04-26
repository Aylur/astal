namespace Quarrel {
/**
 * Base type for command-line options parsed by [class@Command].
 */
public abstract class Opt : Object {
    /** Human-readable description shown in generated help output. */
    public string? description { get; set; }

    /** Placeholder name shown for the option value in help output. */
    public string? name { get; set; }

    /** Long option name, used as `--name`. */
    public string? long { get; set; }
    /** Short option name, used as `-n`. */
    public char short { get; set; default = '\0'; }

    /** Parse and store a value received for this option. */
    public abstract void parse(string value) throws ParseError;
}

/**
 * Boolean flag option that becomes `true` when present.
 */
public class Flag : Opt {
    /** Parsed flag state. */
    public bool value { get; set; }

    /** Mark the flag as enabled. */
    public override void parse (string _) throws ParseError {
        _value = true;
    }

    /** Create a flag option. */
    public Flag(string? long = null, char short = '\0', string? description = null) {
        Object(long: long, short: short, description: description);
    }
}

/**
 * Option that stores a single string value.
 */
public class StringOpt : Opt {
    /** Parsed string value. */
    public string? value { get;  set; }

    /** Store the parsed string value. */
    public override void parse (string value) throws ParseError {
        _value = value;
    }

    /** Create a string-valued option. */
    public StringOpt(string? long = null, char short = '\0', string? description = null) {
        Object(long: long, short: short, description: description, name: "STRING");
    }
}

/**
 * Option that stores a single integer value.
 */
public class IntOpt : Opt {
    /** Parsed integer value. */
    public int value { get;  set; }

    /** Parse and store an integer value. */
    public override void parse (string value) throws ParseError {
        int parsed;
        if (!int.try_parse(value, out parsed)) {
            throw new ParseError.INVALID_OPTION(@"invalid integer: $value");
        }
        _value = parsed;
    }

    /** Create an integer-valued option. */
    public IntOpt(string? long = null, char short = '\0', string? description = null) {
        Object(long: long, short: short, description: description, name: "INT");
    }
}

/**
 * Option that stores a single floating-point value.
 */
public class FloatOpt : Opt {
    /** Parsed floating-point value. */
    public float value { get; set; }

    /** Parse and store a floating-point value. */
    public override void parse (string value) throws ParseError {
        float parsed;
        if (!float.try_parse(value, out parsed)) {
            throw new ParseError.INVALID_OPTION(@"invalid float: $value");
        }
        _value = parsed;
    }

    /** Create a floating-point-valued option. */
    public FloatOpt(string? long = null, char short = '\0', string? description = null) {
        Object(long: long, short: short, description: description, name: "DOUBLE");
    }
}

/**
 * Option that stores a single file path as a [iface@Gio.File].
 */
public class FileOpt : Opt {
    /** Parsed file value. */
    public File? value { get; set; }

    /** Parse and store a file path. */
    public override void parse (string value) throws ParseError {
        _value = File.new_for_commandline_arg(value);
    }

    /** Create a file-valued option. */
    public FileOpt(string? long = null, char short = '\0', string? description = null) {
        Object(long : long, short: short, description: description, name: "FILE");
    }
}

/**
 * Option that collects repeated file path values.
 */
public class FileArrayOpt : Opt {
    private List<File> _list = new List<File>();

    /** Parsed file values in the order they were provided. */
    public List<File> value { get { return _list; } }

    /** Append a parsed file path to the collected values. */
    public override void parse (string value) throws ParseError {
        _list.append(File.new_for_commandline_arg(value));
    }

    /** Create a repeated file-valued option. */
    public FileArrayOpt(string? long = null, char short = '\0', string? description = null) {
        Object(long: long, short: short, description: description, name: "FILES");
    }
}

/**
 * Option that collects repeated string values.
 */
public class StringArrayOpt : Opt {
    /** Parsed string values in the order they were provided. */
    public string[] value { get; default = {}; }

    /** Append a parsed string to the collected values. */
    public override void parse (string value) throws ParseError {
        _value += value;
    }

    /** Create a repeated string-valued option. */
    public StringArrayOpt(string? long = null, char short = '\0', string? description = null) {
        Object(long: long, short: short, description: description, name: "STRINGS");
    }
}
}
