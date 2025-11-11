namespace AstalRiver {
public delegate string LayoutDemandCallback(Layout layout, string namespace, Output output, uint view_count, uint usable_width, uint usable_height, out List<AstalWl.Rectangle?> rectangles);

public class Layout : Object {
    private class LayoutData {
        public Layout layout;
        public Output output;
        public RiverLayoutV3 river_layout;
    }

    private River river;
    private unowned RiverLayoutManagerV3 layout_manager;
    private HashTable<string, LayoutData?> layouts;

    public string @namespace { get; construct; }
    private LayoutDemandCallback layout_demand_callback;

    public signal void namespace_in_use(string namespace, Output output);
    public signal void user_command(string command, string namespace, Output output);

    public void set_layout_demand_callback(owned LayoutDemandCallback callback) {
        this.layout_demand_callback = (owned)callback;
    }

    private static void handle_namespace_in_use(void* data, RiverLayoutV3 layout) {
        LayoutData*ld = data;
        ld->layout.namespace_in_use(ld->layout.namespace, ld->output);
    }

    private static void handle_layout_demand(void* data, RiverLayoutV3 layout, uint32 view_count, uint32 usable_width, uint32 usable_height, uint32 tags, uint32 serial) {
        LayoutData*ld = data;
        if (ld->layout.layout_demand_callback != null) {
            List<AstalWl.Rectangle?> rectangles;
            string layout_name = ld->layout.layout_demand_callback(ld->layout, ld->layout.namespace, ld->output, view_count, usable_width, usable_height, out rectangles);
            if (rectangles.length() != view_count) {
                warning("the number of rectangles does not match the requested view count\n");
                return;
            }
            foreach (var view in rectangles) {
                ld->river_layout.push_view_dimensions(view.x, view.y, view.width, view.height, serial);
            }
            ld->river_layout.commit(layout_name, serial);
        }
    }

    private static void handle_user_command(void* data, RiverLayoutV3 layout, string command) {
        LayoutData*ld = data;
        ld->layout.user_command(command, ld->layout.namespace, ld->output);
    }

    private static void handle_user_command_tags(void* data, RiverLayoutV3 layout, uint32 tags) {
        // we can ignore this, because we can already get the tag information (and more) from the Output object
    }

    private const RiverLayoutV3Listener layout_listener = {
        handle_namespace_in_use,
        handle_layout_demand,
        handle_user_command,
        handle_user_command_tags
    };

    private void output_added(Output output) {
        LayoutData data = new LayoutData() {
            layout = this,
            output = output,
        };
        data.river_layout = this.layout_manager.get_layout(output.output.get_wl_output(), this.namespace);
        data.river_layout.add_listener(layout_listener, data);
        this.layouts.insert(output.output.name, data);
    }

    private void output_removed(Output output) {
        this.layouts.remove(output.output.name);
    }

    internal Layout(River river, string @namespace, RiverLayoutManagerV3 layout_manager) {
        Object(@namespace: @namespace);
        this.river = river;
        this.layout_manager = layout_manager;
        this.layouts = new HashTable<string, LayoutData?>(str_hash, str_equal);

        this.river.output_added.connect(output_added);
        this.river.output_removed.connect(output_removed);
        this.river.outputs.foreach(o => output_added(o));
    }
}
}