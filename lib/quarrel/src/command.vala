namespace Quarrel {
/**
 * Declarative command definition with builder-style APIs for options, arguments, subcommands, parsing, and help generation.
 */
public class Command : Object {
    internal HashTable<string, Command> subcommands = new HashTable<string, Command>(str_hash, str_equal);

    internal HashTable<char, Opt> shorts = new HashTable<char, Opt>(direct_hash, direct_equal);
    internal HashTable<string, Opt> longs = new HashTable<string, Opt>(str_hash, str_equal);
    internal Opt[] opts = {};
    internal string[] argvec = null;

    internal string[] examples = {};

    internal class Arg {
        internal string name;
        internal string description;
        internal bool required;
    }

    internal Arg[] arg_list = {};
    internal uint min_args = 0;
    internal uint max_args = 0;
    internal string? rest_args_name;
    internal string? rest_args_description;

    /** Parent Command that this Command is a subcommand of. */
    public Command? parent { get; construct set; }

    /** Description set via [method@Command.about]. */
    public string? about_text { get; private set; }

    /**
     * Name of this command.
     * The name of the top level command should be the name of the program.
     */
    public string? name { get; construct set; }

    /**
     * Define a new command. Descriptions, arguments, options, subcommands can be added via builder methods.
     */
    public Command(string? name = null) {
        Object(name : name ?? Environment.get_prgname());
    }

    /**
     * Append a subcommand to this command.
     */
    public Command subcommand (Command command) requires(command.name != null && command.parent == null) {
        subcommands.set(command.name, command);
        command.parent = this;
        return this;
    }

    /**
     * Append an option to this command.
     */
    public Command opt (Opt opt) requires(opt.short != '\0' || opt.long != null) {
        if ((opt.short != '\0') && !shorts.set(opt.short, opt)) {
            warning(@"overriding -$(opt.short) option");
        }

        if ((opt.long != null) && !longs.set(opt.long, opt)) {
            warning(@"overriding --$(opt.long) option");
        }

        opts += opt;
        return this;
    }

    /**
     * Set the description of this command.
     */
    public Command about (string about) {
        about_text = about;
        return this;
    }

    /**
     * Define an optional positional argument.
     */
    public Command arg (string name, string description) {
        max_args += 1;
        arg_list += new Arg() {
            name = name,
            description = description,
            required = false,
        };
        return this;
    }

    /**
     * Define a required positional argument.
     * You might want to use [method@Command.arg] instead because this method
     * will cause the parser to throw if the argument is missing.
     */
    public Command required_arg (string name, string description) {
        min_args += 1;
        max_args += 1;
        arg_list += new Arg() {
            name = name,
            description = description,
            required = true,
        };
        return this;
    }

    /**
     * Define the rest of the positional arguments.
     */
    public Command rest_args (string name, string description) {
        max_args = uint.MAX;
        rest_args_name = name;
        rest_args_description = description;
        return this;
    }

    /**
     * Append an example usage of this command.
     */
    public Command example (string example) {
        examples += example;
        return this;
    }

    /**
     * Get an option appended using [method@Command.opt] by either its long or short name.
     */
    public Opt? get_opt (string? long = null, char short = '\0') requires(short != '\0' || long != null) {
        if (long != null) {
            return longs.get(long);
        }

        if (short != '\0') {
            return shorts.get(short);
        }

        return_val_if_reached(null);
    }

    /**
     * Get the parsed positional arguments. Guaranteed to be empty before calling [method@Command.parse].
     */
    public string[] args { get { return argvec; } }

    private void validate_parsed_args (string[] parsed_args) throws ParseError {
        if (parsed_args.length < min_args) {
            throw new ParseError.MISSING_ARGS("expected at least %u arguments, got %u".printf(min_args, parsed_args.length));
        }

        if (parsed_args.length > max_args) {
            throw new ParseError.EXTRA_ARGS("expected at most %u arguments, got %u".printf(max_args, parsed_args.length));
        }
    }

    /**
     * Parse CLI arguments. Not that the first argument `argv[0]` is ignored as it is
     * assumed to be the name of this command.
     * Returns the command that should be invoked.
     */
    public Command parse (string[] argv) throws ParseError {
        string[] parsed_args = {};
        bool parsing_options = true;

        // argv[0] is ignored as it's assumed to be the current command name.
        for (var i = 1; i < argv.length; i++) {
            var token = argv[i];

            if (parsing_options && (token == "--")) {
                parsing_options = false;
                continue;
            }

            if (parsing_options && token.has_prefix("--") && (token.length > 2)) {
                var body = token.substring(2);
                var separator = body.index_of_char('=');
                string name;
                string value = "";

                if (separator >= 0) {
                    name = body.substring(0, separator);
                    value = body.substring(separator + 1);
                } else {
                    name = body;
                }

                var opt = longs.get(name);
                if (opt == null) {
                    throw new ParseError.UNKNOWN_OPTION(@"unknown option --$name");
                }

                if (!(opt is Flag) && (separator < 0)) {
                    i++;
                    if (i >= argv.length) {
                        throw new ParseError.MISSING_OPTION_VALUE(@"missing value for --$name");
                    }

                    value = argv[i];
                }

                opt.parse(value);
                continue;
            }

            if (parsing_options && token.has_prefix("-") && (token != "-")) {
                for (var j = 1; j < token.length; j++) {
                    var short_name = token[j];
                    var opt = shorts.get(short_name);

                    if (opt == null) {
                        throw new ParseError.UNKNOWN_OPTION(@"unknown option -$short_name");
                    }

                    if (opt is Flag) {
                        opt.parse("");
                        continue;
                    }

                    string value;
                    if (j + 1 < token.length) {
                        value = token.substring(j + 1);
                    } else {
                        i++;
                        if (i >= argv.length) {
                            throw new ParseError.MISSING_OPTION_VALUE(@"missing value for -$short_name");
                        }

                        value = argv[i];
                    }

                    opt.parse(value);
                    break;
                }

                continue;
            }

            var subcommand = subcommands.get(token);
            if (subcommand != null) {
                this.argvec = parsed_args;
                validate_parsed_args(parsed_args);
                return subcommand.parse(argv[i: argv.length]);
            }

            parsed_args += token;
        }

        this.argvec = parsed_args;
        validate_parsed_args(parsed_args);
        return this;
    }
}

/**
 * Possible errors on [method@Command.parse] and [method@Opt.parse].
 */
public errordomain ParseError {
    /** Number of arguments is less than expected. */
    MISSING_ARGS,
    /** Number of arguments is more than expected. */
    EXTRA_ARGS,
    /** Option value is missing. */
    MISSING_OPTION_VALUE,
    /** Failed to parse the option value. */
    INVALID_OPTION,
    /** An unknown option was encountered. */
    UNKNOWN_OPTION,
}
}
