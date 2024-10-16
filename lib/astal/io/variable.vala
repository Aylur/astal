/*
 * Base class for [class@AstalIO.Variable] mainly meant to be used
 * in higher level language bindings such as Lua and Gjs.
 */
public class AstalIO.VariableBase : Object {
    public signal void changed ();
    public signal void dropped ();
    public signal void error (string err);

    // lua-lgi crashes when using its emitting mechanism
    public void emit_changed() { changed(); }
    public void emit_dropped() { dropped(); }
    public void emit_error(string err) { this.error(err); }

    ~VariableBase() {
        dropped();
    }
}

public class AstalIO.Variable : VariableBase {
    public Value value { owned get; set; }

    private uint poll_id = 0;
    private Process? watch_proc;

    private uint poll_interval { get; set; default = 1000; }
    private string[] poll_exec { get; set; }
    private Closure? poll_transform { get; set; }
    private Closure? poll_fn { get; set; }

    private Closure? watch_transform { get; set; }
    private string[] watch_exec { get; set; }

    public Variable(Value init) {
        Object(value: init);
    }

    public Variable poll(
        uint interval,
        string exec,
        Closure? transform
    ) throws Error {
        string[] argv;
        Shell.parse_argv(exec, out argv);
        return pollv(interval, argv, transform);
    }

    public Variable pollv(
        uint interval,
        string[] execv,
        Closure? transform
    ) throws Error {
        if (is_polling())
            stop_poll();

        poll_interval = interval;
        poll_exec = execv;
        poll_transform = transform;
        poll_fn = null;
        start_poll();
        return this;
    }

    public Variable pollfn(
        uint interval,
        Closure fn
    ) throws Error {
        if (is_polling())
            stop_poll();

        poll_interval = interval;
        poll_fn = fn;
        poll_exec = null;
        start_poll();
        return this;
    }

    public Variable watch(
        string exec,
        Closure? transform
    ) throws Error {
        string[] argv;
        Shell.parse_argv(exec, out argv);
        return watchv(argv, transform);
    }

    public Variable watchv(
        string[] execv,
        Closure? transform
    ) throws Error {
        if (is_watching())
            stop_watch();

        watch_exec = execv;
        watch_transform = transform;
        start_watch();
        return this;
    }

    construct {
        notify["value"].connect(() => changed());
        dropped.connect(() => {
            if (is_polling())
                stop_poll();

            if (is_watching())
                stop_watch();
        });
    }

    private void set_closure(string val, Closure? transform) {
        if (transform != null) {
            var str = Value(typeof(string));
            str.set_string(val);

            var ret_val = Value(this.value.type());
            transform.invoke(ref ret_val, { str, this.value });
            this.value = ret_val;
        }
        else {
            if (this.value.type() == Type.STRING && this.value.get_string() == val)
                return;

            var str = Value(typeof(string));
            str.set_string(val);
            this.value = str;
        }
    }

    private void set_fn() {
        var ret_val = Value(this.value.type());
        poll_fn.invoke(ref ret_val, { this.value });
        this.value = ret_val;
    }

    public void start_poll() throws Error {
        return_if_fail(poll_id == 0);

        if (poll_fn != null) {
            set_fn();
            poll_id = Timeout.add(poll_interval, () => {
                set_fn();
                return Source.CONTINUE;
            }, Priority.DEFAULT);
        }
        if (poll_exec != null) {
            Process.exec_asyncv.begin(poll_exec, (_, res) => {
                try {
                    var str = Process.exec_asyncv.end(res);
                    set_closure(str, poll_transform);
                } catch (Error err) {
                    this.error(err.message);
                }
            });
            poll_id = Timeout.add(poll_interval, () => {
                Process.exec_asyncv.begin(poll_exec, (_, res) => {
                    try {
                        var str = Process.exec_asyncv.end(res);
                        set_closure(str, poll_transform);
                    } catch (Error err) {
                        this.error(err.message);
                        Source.remove(poll_id);
                        poll_id = 0;
                    }
                });
                return Source.CONTINUE;
            }, Priority.DEFAULT);
        }
    }

    public void start_watch() throws Error {
        return_if_fail(watch_proc == null);
        return_if_fail(watch_exec != null);

        watch_proc = new Process.subprocessv(watch_exec);
        watch_proc.stdout.connect((str) => set_closure(str, watch_transform));
        watch_proc.stderr.connect((str) => this.error(str));
    }

    public void stop_poll() {
        return_if_fail(poll_id != 0);
        Source.remove(poll_id);
        poll_id = 0;
    }

    public void stop_watch() {
        return_if_fail(watch_proc != null);
        watch_proc.kill();
        watch_proc = null;
    }

    public bool is_polling() { return poll_id > 0; }
    public bool is_watching() { return watch_proc != null; }

    ~Variable() {
        dropped();
    }
}
