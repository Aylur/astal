/**
 * `Process` provides shortcuts for [class@GLib.Subprocess] with sane defaults.
 */
public class AstalIO.Process : Object {
    private void read_stream(DataInputStream stream, bool err) {
        stream.read_line_utf8_async.begin(Priority.DEFAULT, null, (_, res) => {
            try {
                var output = stream.read_line_utf8_async.end(res);
                if (output != null) {
                    if (err)
                        stdout(output.strip());
                    else
                        stderr(output.strip());

                    read_stream(stream, err);
                }
            } catch (Error err) {
                printerr("%s\n", err.message);
            }
        });
    }

    private DataInputStream out_stream;
    private DataInputStream err_stream;
    private DataOutputStream in_stream;
    private Subprocess process;
    public string[] argv { construct; get; }


    /**
     * When the underlying subprocess writes to its stdout
     * this signal is emitted with that line.
     */
    public signal void stdout (string out);

    /**
     * When the underlying subprocess writes to its stderr
     * this signal is emitted with that line.
     */
    public signal void stderr (string err);

    /**
     * Force quit the subprocess.
     */
    public void kill() {
        process.force_exit();
    }

    /**
     * Send a signal to the subprocess.
     */
    public void signal(int signal_num) {
        process.send_signal(signal_num);
    }

    /**
     * Write a line to the subprocess' stdin synchronously.
     */
    public void write(string in) throws Error {
        in_stream.put_string(in);
    }

    /**
     * Write a line to the subprocess' stdin asynchronously.
     */
    public async void write_async(string in) {
        try {
            yield in_stream.write_all_async(in.data, in.data.length, null, null);
        } catch (Error err) {
            printerr("%s\n", err.message);
        }
    }

    /**
     * Start a new subprocess with the given command.
     *
     * The first element of the vector is executed with the remaining elements as the argument list.
     */
    public Process.subprocessv(string[] cmd) throws Error {
        Object(argv: cmd);
        process = new Subprocess.newv(cmd,
            SubprocessFlags.STDIN_PIPE |
            SubprocessFlags.STDERR_PIPE |
            SubprocessFlags.STDOUT_PIPE
        );
        out_stream = new DataInputStream(process.get_stdout_pipe());
        err_stream = new DataInputStream(process.get_stderr_pipe());
        in_stream = new DataOutputStream(process.get_stdin_pipe());
        read_stream(out_stream, true);
        read_stream(err_stream, false);
    }

    /**
     * Start a new subprocess with the given command
     * which is parsed using [func@Shell.parse_argv].
     */
    public static Process subprocess(string cmd) throws Error {
        string[] argv;
        Shell.parse_argv(cmd, out argv);
        return new Process.subprocessv(argv);
    }

    /**
     * Execute a command synchronously.
     * The first element of the vector is executed with the remaining elements as the argument list.
     *
     * @return stdout of the subprocess
     */
    public static string execv(string[] cmd) throws Error {
        var process = new Subprocess.newv(
            cmd,
            SubprocessFlags.STDERR_PIPE |
            SubprocessFlags.STDOUT_PIPE
        );

        string err_str, out_str;
        process.communicate_utf8(null, null, out out_str, out err_str);
        var success = process.get_successful();
        process.dispose();
        if (success)
            return out_str.strip();
        else
            throw new IOError.FAILED(err_str.strip());
    }

    /**
     * Execute a command synchronously.
     * The command is parsed using [func@Shell.parse_argv].
     *
     * @return stdout of the subprocess
     */
    public static string exec(string cmd) throws Error {
        string[] argv;
        Shell.parse_argv(cmd, out argv);
        return Process.execv(argv);
    }

    /**
     * Execute a command asynchronously.
     * The first element of the vector is executed with the remaining elements as the argument list.
     *
     * @return stdout of the subprocess
     */
    public static async string exec_asyncv(string[] cmd) throws Error {
        var process = new Subprocess.newv(
            cmd,
            SubprocessFlags.STDERR_PIPE |
            SubprocessFlags.STDOUT_PIPE
        );

        string err_str, out_str;
        yield process.communicate_utf8_async(null, null, out out_str, out err_str);
        var success = process.get_successful();
        process.dispose();
        if (success)
            return out_str.strip();
        else
            throw new IOError.FAILED(err_str.strip());
    }

    /**
     * Execute a command asynchronously.
     * The command is parsed using [func@Shell.parse_argv].
     *
     * @return stdout of the subprocess
     */
    public static async string exec_async(string cmd) throws Error {
        string[] argv;
        Shell.parse_argv(cmd, out argv);
        return yield exec_asyncv(argv);
    }
}
