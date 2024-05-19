private static bool version;
private static bool help;
private static string? instance_name;

private const GLib.OptionEntry[] options = {
    { "version", 'v', OptionFlags.NONE, OptionArg.NONE, ref version, null, null },
    { "help", 'h', OptionFlags.NONE, OptionArg.NONE, ref help, null, null },
    { "instance-name", 'i', OptionFlags.NONE, OptionArg.STRING, ref instance_name, null, null },
    { null },
};

async int main(string[] argv) {
    try {
        var opts = new OptionContext();
        opts.add_main_entries(options, null);
        opts.set_help_enabled(false);
        opts.set_ignore_unknown_options(false);
        opts.parse(ref argv);
    } catch (OptionError err) {
        printerr (err.message);
        return 1;
    }

    if (help) {
        print("Client for the socket of an Astal.Application instance\n\n");
        print("Usage:\n");
        print("    %s [flags] message\n\n", argv[0]);
        print("Flags:\n");
        print("    -h, --help            Print this help and exit\n");
        print("    -v, --version         Print version number and exit\n");
        print("    -i, --instance-name   Instance name of the Astal instance\n");
        return 0;
    }

    if (version) {
        print("@VERSION@");
        return 0;
    }

    if (instance_name == null)
        instance_name = "astal";

    var request = "";
    for (var i = 1; i < argv.length; ++i) {
        request = request.concat(" ", argv[i]);
    }

    var client = new SocketClient();
    var rundir = GLib.Environment.get_user_runtime_dir();
    var socket = rundir.concat("/", instance_name, ".sock");

    try {
        var conn = client.connect(new UnixSocketAddress(socket), null);

        try {
            yield conn.output_stream.write_async(
                request.concat("\x04").data,
                Priority.DEFAULT);
        } catch (Error err) {
            printerr("could not write to app '%s'", instance_name);
        }

        var stream = new DataInputStream(conn.input_stream);
        size_t size;

        try {
            var res = yield stream.read_upto_async("\x04", -1, Priority.DEFAULT, null, out size);
            if (res != null)
                print("%s", res);
        } catch (Error err) {
            printerr(err.message);
        }
    } catch (Error err) {
        printerr("could not connect to app '%s'", instance_name);
        return 1;
    }

    return 0;
}
