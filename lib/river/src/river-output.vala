namespace AstalRiver {
public class Output : Object {
    public AstalWl.Output output { get; construct; }

    private uint _focused_tags;
    public uint focused_tags {
        get {
            return this._focused_tags;
        }
        set {
            string output;
            this.river.run_command({ "set-focused-tags", value.to_string() }, out output);
        }
    }
    public uint occupied_tags { get; private set; }
    public uint urgent_tags { get; private set; }
    public string? focused_view { get; internal set; }
    public string? layout_name { get; private set; }

    private ZriverOutputStatusV1 output_status;
    private River river;

    private void handle_focused_tags(ZriverOutputStatusV1 status, uint32 tags) {
        this._focused_tags = tags;
        this.notify_property("focused-tags");
    }

    private void handle_view_tags(ZriverOutputStatusV1 status, Wl.Array views) {
        uint tags = 0;
        uint32* view;
        for (view = views.data; views.size != 0 && view < (char*)views.data + views.size; view++) {
            tags |= *view;
        }
        this.occupied_tags = tags;
    }

    private void handle_urgent_tags(ZriverOutputStatusV1 zriver_output_status_v1, uint32 tags) {
        this.urgent_tags = tags;
    }

    private void handle_layout_name(ZriverOutputStatusV1 zriver_output_status_v1, string name) {
        this.layout_name = name;
    }

    private void handle_layout_name_clear(ZriverOutputStatusV1 zriver_output_status_v1) {
        this.layout_name = null;
    }

    private const ZriverOutputStatusV1Listener output_status_listener = {
        handle_focused_tags,
        handle_view_tags,
        handle_urgent_tags,
        handle_layout_name,
        handle_layout_name_clear
    };

    internal Output(AstalWl.Output output, River river, ZriverStatusManagerV1 status_manager) {
        Object(output: output);
        this.river = river;
        this.output_status = status_manager.get_river_output_status(output.get_wl_output());
        this.output_status.add_listener(output_status_listener, this);
    }
}
}