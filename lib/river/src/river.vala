namespace AstalRiver {

public static bool is_supported() {
    return !AstalWl.Registry.get_default().find_globals("zriver_status_manager_v1").is_empty();
}

public static unowned River get_default() {
    return AstalRiver.River.get_default();
}

public class River : Object {
    private static River? instance;

    public static unowned River get_default() {
        if (instance == null) instance = new River();
        return instance;
    }

    private List<Output> _outputs;
    public List<weak Output> outputs { owned get { return _outputs.copy(); } }

    public signal void output_added(Output output);
    public signal void output_removed(Output output);

    public Output? focused_output { get; private set; }
    public string? focused_output_name {
        get {
            if (this.focused_output == null) return null;
            return this.focused_output.output.name;
        }
    }

    public string? focused_view { get; private set; }
    public string? mode { get; private set; }

    private AstalWl.Registry astal_registry;
    private ZriverStatusManagerV1 river_status;
    private ZriverSeatStatusV1 seat_status;
    private ZriverControlV1 river_control;
    private RiverLayoutManagerV3 layout_manager;

    private void handle_output_added(AstalWl.Output wl_output) {
        var output = new Output(wl_output, this, river_status);
        this._outputs.append(output);
        this.output_added(output);
        this.notify_property("outputs");
    }

    private void handle_output_removed(AstalWl.Output wl_output) {
        var output = find_output_by_astal_wl_output(wl_output);
        this._outputs.remove(output);
        this.output_removed(output);
        this.notify_property("outputs");
    }

    [GIR(visible = false)]
    public Output? find_output_by_wl_output(Wl.Output wl_output) {
        foreach (var o in this.outputs) {
            if (o.output.get_wl_output() == wl_output) return o;
        }
        return null;
    }

    public Output? find_output_by_astal_wl_output(AstalWl.Output wl_output) {
        foreach (var o in this.outputs) {
            if (o.output == wl_output) return o;
        }
        return null;
    }

    public Output? find_output_by_name(string name) {
        foreach (var o in this.outputs) {
            if (o.output.name == name) return o;
        }
        return null;
    }

    public Output? find_output_by_id(uint32 id) {
        foreach (var o in this.outputs) {
            if (o.output.id == id) return o;
        }
        return null;
    }

    private void handle_focused_output(ZriverSeatStatusV1 seat_status, Wl.Output wl_output) {
        this.focused_output = find_output_by_wl_output(wl_output);
        this.notify_property("focused_output_name");
    }

    private void handle_unfocused_output(ZriverSeatStatusV1 seat_status, Wl.Output wl_output) {
        this.focused_output = null;
        this.notify_property("focused_output_name");
    }

    private void handle_focused_view(ZriverSeatStatusV1 seat_status, string? title) {
        this.focused_view = title;
        if (this.focused_output != null) this.focused_output.focused_view = title;
    }
    private void handle_mode(ZriverSeatStatusV1 seat_status, string? mode) {
        this.mode = mode;
    }

    private struct CallbackResult {
        bool success;
        string msg;
    }

    private static void handle_comman_callback_success(void* data, ZriverCommandCallbackV1 cb, string msg) {
        CallbackResult* result = data;
        result->success = true;
        result->msg = msg;
    }

    private static void handle_comman_callback_failure(void* data, ZriverCommandCallbackV1 cb, string msg) {
        CallbackResult* result = data;
        result->success = false;
        result->msg = msg;
    }

    public bool run_command(string[] cmd, out string? output) {
        if(this.river_control == null) {
            critical("the compositor does not support zriver_control_v1\n");
            output = null;
            return false;
        }
        foreach (var arg in cmd) {
            this.river_control.add_argument(arg);
        }
        var seat = this.astal_registry.get_seats().nth_data(0);
        var cb = this.river_control.run_command(seat.get_wl_seat());
        ZriverCommandCallbackV1Listener cb_listener = {
            handle_comman_callback_success,
            handle_comman_callback_failure,
        };
        CallbackResult result = CallbackResult() {};
        cb.add_listener(cb_listener, &result);
        this.astal_registry.get_display().roundtrip();
        output = result.msg;
        return result.success;
    }

    public async bool run_command_async(string[] cmd, out string? output) {
        return run_command(cmd, out output);
    }

    public Layout? new_layout(string @namespace) {
        if(this.layout_manager == null) {
            critical("the compositor does not support river_layout_v3\n");
            return null;
        }
        return new Layout(this, @namespace, this.layout_manager);
    }

    private const ZriverSeatStatusV1Listener seat_listener = {
        handle_focused_output,
        handle_unfocused_output,
        handle_focused_view,
        handle_mode
    };

    public River() {
        this._outputs = new List<Output>();
        this.astal_registry = AstalWl.Registry.get_default();

        AstalWl.Global? status_global = this.astal_registry.find_globals("zriver_status_manager_v1").nth_data(0);
        if (status_global == null) {
            critical("the compositor does not support zriver_status_manager_v1\n");
            return;
        }

        this.river_status = this.astal_registry.get_registry().bind(status_global.name, ref ZriverStatusManagerV1.iface, uint.min(status_global.version, 4));
        var seat = this.astal_registry.get_seats().nth_data(0);
        if (seat != null) {
            this.seat_status = this.river_status.get_river_seat_status(seat.get_wl_seat());
            this.seat_status.add_listener(seat_listener, this);
        }

        AstalWl.Global? control_global = this.astal_registry.find_globals("zriver_control_v1").nth_data(0);
        if (control_global != null) {
            this.river_control = this.astal_registry.get_registry().bind(control_global.name, ref ZriverControlV1.iface, uint.min(control_global.version, 1));
        }

        AstalWl.Global? layout_global = this.astal_registry.find_globals("river_layout_manager_v3").nth_data(0);
        if (layout_global != null) {
            this.layout_manager = this.astal_registry.get_registry().bind(layout_global.name, ref RiverLayoutManagerV3.iface, uint.min(layout_global.version, 2));
        }

        this.astal_registry.get_outputs().foreach(output => this.handle_output_added(output));
        this.astal_registry.output_added.connect((o) => this.handle_output_added(o));
        this.astal_registry.output_removed.connect((o) => this.handle_output_removed(o));

        this.astal_registry.get_display().roundtrip();
    }
}
}
