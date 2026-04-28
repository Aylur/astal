namespace Quarrel {
private const string INDENT = "  ";
private const string PAD = "   ";

private string bold (string str) {
    return "\x1b[1m%s\x1b[0m".printf(str);
}

private string title (string str) {
    return "\x1b[32m%s".printf(bold(str));
}

private void command_usage (Command command, StringBuilder builder) {
    var parent_names = new Queue<string>();
    Command? parent = command.parent;

    while ((parent) != null) {
        parent_names.push_tail(parent.name);
        parent = parent.parent;
    }

    while (parent_names.length > 0) {
        builder.append(" ");
        builder.append(bold(parent_names.pop_tail()));
    }

    builder.append(@" $(bold(command.name))");

    if (command.opts.length > 0) {
        builder.append(" [%s]".printf(bold("OPTIONS")));
    }

    foreach (var arg in command.arg_list) {
        if (arg.required) {
            builder.append(@" <$(bold(arg.name))>");
        } else {
            builder.append(@" [$(bold(arg.name))]");
        }
    }

    if (command.rest_args_name != null) {
        builder.append(@" [$(bold(command.rest_args_name))]…");
    }

    if (command.subcommands.length > 0) {
        builder.append(" <%s>".printf(bold("COMMAND")));
    }
}

private void command_arguments (Command command, StringBuilder builder) {
    var max_length = 0;

    foreach (var arg in command.arg_list) {
        if (arg.name.length > max_length) {
            max_length = arg.name.length;
        }
    }

    if ((command.rest_args_name != null) && (command.rest_args_name.length >= max_length)) {
        max_length = command.rest_args_name.length + 1; // +1 = '…'
    }

    foreach (var arg in command.arg_list) {
        var padding = string.nfill(max_length - arg.name.length, ' ');

        var desc_padding = string.nfill(max_length + 4, ' '); // 4 = brackets + indent
        var description = arg.description.replace("\n", @"\n$(desc_padding)$(PAD)");

        if (arg.required) {
            builder.append(@"\n$(INDENT)<$(bold(arg.name))>$(padding)$(PAD)$(description)");
        } else {
            builder.append(@"\n$(INDENT)[$(bold(arg.name))]$(padding)$(PAD)$(description)");
        }
    }

    if (command.rest_args_name != null) {
        var padding = string.nfill(max_length - command.rest_args_name.length - 1, ' '); // -1 = '…'

        var desc_padding = string.nfill(max_length + 3, ' '); // 3 = brackets + indent - '…'
        var description = command.rest_args_description.replace("\n", @"\n$(desc_padding)$(PAD)");
        builder.append(@"\n$(INDENT)[$(bold(command.rest_args_name))]…$(padding)$(PAD)$(description)");
    }
}

private void command_subcommands (Command command, StringBuilder builder) {
    var max_length = 0;

    foreach (var cmd in command.subcommands.get_values()) {
        if (cmd.name.length > max_length) {
            max_length = cmd.name.length;
        }
    }

    foreach (var cmd in command.subcommands.get_values()) {
        builder.append(@"\n$(INDENT)$(bold(cmd.name))");

        if (cmd.about_text != null) {
            var line_padding = string.nfill(max_length, ' ');
            var description = cmd.about_text.replace("\n", @"\n$(INDENT)$(line_padding)$(PAD)");

            var padding = string.nfill(max_length - cmd.name.length, ' ');
            builder.append(@"$(padding)$(PAD)$(description)");
        }
    }
}

private void command_options (Command command, StringBuilder builder) {
    var max_long_length = 0;
    var max_name_length = 0;
    var has_short = false;

    foreach (var opt in command.opts) {
        var long_length = (opt.long == null) ? 0 : opt.long.length;
        if (long_length > max_long_length) {
            max_long_length = long_length;
        }

        var name_length = (opt.name == null) ? 0 : opt.name.length;
        if (name_length > max_name_length) {
            max_name_length = name_length;
        }

        if (opt.short != '\0') {
            has_short = true;
        }
    }

    foreach (var opt in command.opts) {
        builder.append(@"\n$(INDENT)");
        if (opt.short != '\0') {
            builder.append(bold(@"-$(opt.short)"));
        } else if (has_short) {
            builder.append(@"  ");
        }

        if (opt.long != null) {
            if (opt.short != '\0') {
                builder.append(@", ");
            } else if (has_short) {
                builder.append(@"  ");
            }
            builder.append(bold(@"--$(opt.long)"));
        }

        if (opt.name != null) {
            builder.append(@" <$(bold(opt.name))>");
        }

        if (opt.description != null) {
            var long_length = ((opt.long == null) ? max_long_length : (max_long_length - opt.long.length));
            var name_length = ((opt.name == null) ? max_name_length : (max_name_length - opt.name.length));
            var padding_length = long_length + name_length;

            if ((opt.long == null) && (max_long_length > 0)) {
                padding_length += 2; // 2 = "--"
                if (has_short) {
                    padding_length += 2; // 2 = ", "
                }
            }
            if ((opt.name == null) && (max_name_length > 0)) {
                padding_length += 3; // 3 = " <>"
            }

            builder.append(string.nfill(padding_length, ' '));

            var line_padding_length = 0;
            if (has_short) {
                line_padding_length += 2; // 2 = "-f"
            }
            if ((max_long_length > 0) && has_short) {
                line_padding_length += 2; // 2 = ", "
            }
            if (max_long_length > 0) {
                line_padding_length += max_long_length + 2; // 2 = "--"
            }
            if (max_name_length > 0) {
                line_padding_length += max_name_length + 3; // 3 = " <>"
            }

            var line_padding = string.nfill(line_padding_length, ' ');
            var description = opt.description.replace("\n", @"\n$(INDENT)$(line_padding)$(PAD)");
            builder.append(@"$(PAD)$(description)");
        }
    }
}

private void command_examples (Command command, StringBuilder builder) {
    foreach (var example in command.examples) {
        var formatted_example = example.replace("\n", @"\n$(INDENT)");
        builder.append(@"\n$(INDENT)$(formatted_example)");
    }
}

/**
 * Generate formatted help text for a command and its declared interface.
 */
public string help (Command command) {
    var builder = new StringBuilder();

    if (command.about_text != null) {
        builder.append(@"$(command.about_text)\n");
    }

    builder.append(title("\nUsage:"));
    command_usage(command, builder);

    if ((command.arg_list.length > 0)) {
        builder.append(title("\n\nArguments:"));
        command_arguments(command, builder);
    }

    if (command.subcommands.length > 0) {
        builder.append(title("\n\nCommands:"));
        command_subcommands(command, builder);
    }

    if (command.opts.length > 0) {
        builder.append(title("\n\nOptions:"));
        command_options(command, builder);
    }

    if (command.examples.length > 0) {
        builder.append(title("\n\nExamples:"));
        command_examples(command, builder);
    }

    return builder.str;
}
}
