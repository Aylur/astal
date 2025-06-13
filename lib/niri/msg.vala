namespace AstalNiri {

delegate void ParameterFuncDelegate(Json.Builder b);

struct ParameterFunc {
  ParameterFuncDelegate build;
}

public class msg : Object {
    private static ParameterFunc array_member(string name, ParameterFunc[] items) {
      return member(name, array_value(items));
    }

    private static ParameterFunc array_value(ParameterFunc[] items) {
      return {
        (b) => {
          b.begin_array();
          foreach (var item in items) {
            item.build(b);
          }
          b.end_array();
        }
      };
    }

    private static ParameterFunc bool_member(string name, owned bool? value) {
      if (value == null) return {() => {}};
      return member(name, bool_value(value));
    }

    private static ParameterFunc bool_value(bool value) {
      return {
        (b) => {
          b.add_boolean_value(value);
        }
      };
    }

    private static ParameterFunc double_member(string name, owned double? value) {
      if (value == null) return {() => {}};
      return member(name, double_value(value));
    }

    private static ParameterFunc double_value(double value) {
      return {
        (b) => {
          b.add_double_value(value);
        }
      };
    }

    private static string fmt_str(string str) {
      return "\"%s\"\n".printf(str);
    }

    private static ParameterFunc int_member(string name, owned int? value) {
      if (value == null) { return {() => {}}; }
      return member(name, int_value(value));
    }

    private static ParameterFunc int_value(int value) {
      return {
        (b) => {
          b.add_int_value(value);
        }
      };
    }

    private static ParameterFunc member(string name, owned ParameterFunc value_setter) {
      return {
        (b) => {
          b.set_member_name(name);
          value_setter.build(b);
        }
      };
    }

    private static ParameterFunc obj_member(string name, ParameterFunc[] fields) {
      return member(name, obj_value(fields));
    }

    private static ParameterFunc obj_value(ParameterFunc[] fields) {
      return {
        (b) => {
          b.begin_object();
          foreach (var field in fields) {
            field.build(b);
          }
          b.end_object();
        }
      };
    }

    private static string serialize_fields(ParameterFunc[] fields) {
      var builder = new Json.Builder();
      var generator = new Json.Generator();

      var fields_builder = obj_value(fields);
      fields_builder.build(builder);

      var root = builder.get_root();
      generator.set_root(root);
      return generator.to_data(null);
    }

    private static ParameterFunc str_array_member(string name, owned string[] items) {
      return member(name, str_array_value(items));
    }

    private static ParameterFunc str_array_value(owned string[] items) {
      return {
        (b) => {
          b.begin_array();
          foreach (var item in items) {
            b.add_string_value(item);
          }
          b.end_array();
        }
      };
    }

    private static ParameterFunc str_member(string name, owned string? value) {
      if (value == null) return {() => {}};
      return member(name, str_value(value));
    }

    private static ParameterFunc str_value(string value) {
      return {
        (b) =>{
          b.add_string_value(value);
        }
      };
    }

    //
    // Raw Messsages
    // 

    public static string? send(string message) {
        IPC ipc;

        try {
            ipc = IPC.connect();
            if (ipc == null ) return null;
            var istream = ipc.send(Json.from_string(message));
            return istream.read_line();
        } catch (Error err) {
            critical("command Error: %s", err.message);
            return err.message;
        } finally {
            ipc.close();
        }
    }

    public static async string send_async(string message) {
        IPC ipc;
        try {
            ipc = IPC.connect();
            if (ipc == null ) return "no ipc";
            var istream = ipc.send(Json.from_string(message));
            return yield istream.read_line_async();
        } catch (Error err) {
            critical("command Error: %s", err.message);
            return err.message;
        } finally {
            ipc.close();
        }
    }

    //
    // Niri Actions
    // 
    
    /** Formats a simple action for Niri's IPC   */
    private static bool send_act(string str, string fields = "{}") {
        var cmd = "{\"Action\":{\"%s\":%s}}\n".printf(str, fields);
        // print("Sending: %s", cmd);
        var res = send(cmd);
        if (res == "{\"Ok\":\"Handled\"}") return true;
        return false;
    }

    public static bool center_column() {
      return send_act("CenterColumn");
    }

    public static bool center_visible_columns() {
      return send_act("CenterVisibleColumns");
    }

    public static bool center_window(int? id) {
      return send_act("CenterWindow", serialize_fields({ int_member("id", id) }));
    }

    public static bool clear_dynamic_cast_target() {
      return send_act("ClearDynamicCastTarget");
    }

    public static bool close_overview() {
      return send_act("CloseOverview");
    }

    public static bool close_window(int? id) {
      return send_act("CloseWindow", serialize_fields({ int_member("id", id)}));
    }

    public static bool consume_or_expel_window_left(int? id) {
      return send_act("ConsumeOrExpelWindowLeft", serialize_fields({ int_member("id", id) }));
    }

    public static bool consume_or_expel_window_right(int? id) {
      return send_act("ConsumeOrExpelWindowRight", serialize_fields({ int_member("id", id) }));
    }

    public static bool consume_window_into_column() {
      return send_act("ConsumeWindowIntoColumn");
    }

    public static bool debug_toggle_damage() {
      return send_act("DebugToggleDamage");
    }

    public static bool debug_toggle_opaque_regions() {
      return send_act("DebugToggleOpaqueRegions");
    }

    public static bool do_screen_transition(int? delay_ms) {
      return send_act("DoScreenTransition", serialize_fields({ int_member("delay_ms", delay_ms) }));
    }

    public static bool expand_column_to_available_width() {
      return send_act("ExpandColumnToAvailableWidth");
    }

    public static bool expel_window_from_column() {
      return send_act("ExpelWindowFromColumn");
    }

    public static bool focus_column(int index) {
      return send_act("FocusColumn", serialize_fields({ int_member("index", index) }));
    }

    public static bool focus_column_first() {
      return send_act("FocusColumnFirst");
    }

    public static bool focus_column_last() {
      return send_act("FocusColumnLast");
    }

    public static bool focus_column_left() {
      return send_act("FocusColumnLeft");
    }

    public static bool focus_column_left_or_last() {
      return send_act("FocusColumnLeftOrLast");
    }

    public static bool focus_column_or_monitor_left() {
      return send_act("FocusColumnOrMonitorLeft");
    }

    public static bool focus_column_or_monitor_right() {
      return send_act("FocusColumnOrMonitorRight");
    }

    public static bool focus_column_right() {
      return send_act("FocusColumnRight");
    }

    public static bool focus_column_right_or_first() {
      return send_act("FocusColumnRightOrFirst");
    }

    public static bool focus_floating() {
      return send_act("FocusFloating");
    }

    public static bool focus_monitor(string output) {
      return send_act("FocusMonitor", serialize_fields({ str_member("output", output) }));
    }

    public static bool focus_monitor_down() {
      return send_act("FocusMonitorDown");
    }

    public static bool focus_monitor_left() {
      return send_act("FocusMonitorLeft");
    }

    public static bool focus_monitor_next() {
      return send_act("FocusMonitorNext");
    }

    public static bool focus_monitor_previous() {
      return send_act("FocusMonitorPrevious");
    }

    public static bool focus_monitor_right() {
      return send_act("FocusMonitorRight");
    }

    public static bool focus_monitor_up() {
      return send_act("FocusMonitorUp");
    }

    public static bool focus_tiling() {
      return send_act("FocusTiling");
    }

    public static bool focus_window(int id) {
      return send_act("FocusWindow", serialize_fields({ int_member("id", id) }));
    }

    public static bool focus_window_bottom() {
      return send_act("FocusWindowBottom");
    }

    public static bool focus_window_down() {
      return send_act("FocusWindowDown");
    }

    public static bool focus_window_down_or_column_left() {
      return send_act("FocusWindowDownOrColumnLeft");
    }

    public static bool focus_window_down_or_column_right() {
      return send_act("FocusWindowDownOrColumnRight");
    }

    public static bool focus_window_down_or_top() {
      return send_act("FocusWindowDownOrTop");
    }
    public static bool focus_window_or_workspace_down() {
      return send_act("FocusWindowOrWorkspaceDown");
    }
    public static bool focus_window_or_workspace_up() {
      return send_act("FocusWindowOrWorkspaceUp");
    }

    public static bool focus_window_in_column(int index) {
      return send_act("FocusWindowInColumn", serialize_fields({ int_member("index", index) }));
    }

    public static bool focus_window_or_monitor_down() {
      return send_act("FocusWindowOrMonitorDown");
    }

    public static bool focus_window_or_monitor_up() {
      return send_act("FocusWindowOrMonitorUp");
    }

    public static bool focus_window_previous() {
      return send_act("FocusWindowPrevious");
    }

    public static bool focus_window_top() {
      return send_act("FocusWindowTop");
    }

    public static bool focus_window_up() {
      return send_act("FocusWindowUp");
    }

    public static bool focus_window_up_or_bottom() {
      return send_act("FocusWindowUpOrBottom");
    }

    public static bool focus_window_up_or_column_left() {
      return send_act("FocusWindowUpOrColumnLeft");
    }

    public static bool focus_window_up_or_column_right() {
      return send_act("FocusWindowUpOrColumnRight");
    }

    public static bool focus_workspace_by_id(int workspace_id) {
      return send_act("FocusWorkspace", serialize_fields({ obj_member("reference", { int_member("Id", workspace_id) }) }));
    }

    public static bool focus_workspace_by_index(int workspace_index) {
      return send_act("FocusWorkspace", serialize_fields({ obj_member("reference", { int_member("Index", workspace_index) }) }));
    }

    public static bool focus_workspace_by_name(string workspace_name) {
      return send_act("FocusWorkspace", serialize_fields({ obj_member("reference", { str_member("Name", workspace_name) }) }));
    }

    public static bool focus_workspace_down() {
      return send_act("FocusWorkspaceDown");
    }

    public static bool focus_workspace_previous() {
      return send_act("FocusWorkspacePrevious");
    }

    public static bool focus_workspace_next() {
      return send_act("FocusWorkspaceNext");
    }

    public static bool focus_workspace_up() {
      return send_act("FocusWorkspaceUp");
    }

    public static bool fullscreen_window(int? id) {
      return send_act("FullscreenWindow", serialize_fields({ int_member("id", id) }));
    }

    public string layers() {
      return send(fmt_str("Layers"));
    }
    public static bool maximize_column() {
      return send_act("MaximizeColumn");
    }

    public static bool move_column_left() {
      return send_act("MoveColumnLeft");
    }

    public static bool move_column_left_or_to_monitor_left() {
      return send_act("MoveColumnLeftOrToMonitorLeft");
    }

    public static bool move_column_right() {
      return send_act("MoveColumnRight");
    }

    public static bool move_column_right_or_to_monitor_right() {
      return send_act("MoveColumnRightOrToMonitorRight");
    }

    public static bool move_column_to_first() {
      return send_act("MoveColumnToFirst");
    }

    public static bool move_column_to_index(int index) {
      return send_act("MoveColumnToIndex", serialize_fields({ int_member("index", index) }));
    }

    public static bool move_column_to_last() {
      return send_act("MoveColumnToLast");
    }

    public static bool move_column_to_monitor(string output) {
      return send_act("MoveColumnToMonitor", serialize_fields({ str_member("output", output) }));
    }

    public static bool move_column_to_monitor_down() {
      return send_act("MoveColumnToMonitorDown");
    }

    public static bool move_column_to_monitor_left() {
      return send_act("MoveColumnToMonitorLeft");
    }

    public static bool move_column_to_monitor_next() {
      return send_act("MoveColumnToMonitorNext");
    }

    public static bool move_column_to_monitor_previous() {
      return send_act("MoveColumnToMonitorPrevious");
    }

    public static bool move_column_to_monitor_right() {
      return send_act("MoveColumnToMonitorRight");
    }

    public static bool move_column_to_monitor_up() {
      return send_act("MoveColumnToMonitorUp");
    }

    public static bool move_column_to_workspace_by_id(int workspace_id, bool focus) {
      return send_act("MoveColumnToWorkspace", serialize_fields({ obj_member("reference", { int_member("Id", workspace_id) }), bool_member("focus", focus) }));
    }

    public static bool move_column_to_workspace_by_index(int workspace_index, bool focus) {
      return send_act("MoveColumnToWorkspace", serialize_fields({ obj_member("reference", { int_member("Index", workspace_index) }), bool_member("focus", focus) }));
    }

    public static bool move_column_to_workspace_by_name(string workspace_name, bool focus) {
      return send_act("MoveColumnToWorkspace", serialize_fields({ obj_member("reference", { str_member("Name", workspace_name) }), bool_member("focus", focus) }));
    }

    public static bool move_column_to_workspace_down(bool focus) {
      return send_act("MoveColumnToWorkspaceDown", serialize_fields({ bool_member("focus", focus) }));
    }

    public static bool move_column_to_workspace_up(bool focus) {
      return send_act("MoveColumnToWorkspaceUp", serialize_fields({ bool_member("focus", focus) }));
    }

    public static bool move_floating_window_adjust_x_adjust_y(int? id, double adjust_x, double adjust_y) {
      return send_act("MoveFloatingWindow", serialize_fields({ int_member("id", id), obj_member("x", { double_member("AdjustFixed", adjust_x) }), obj_member("y", { double_member("AdjustFixed", adjust_y) }) }));
    }

    public static bool move_floating_window_adjust_x_set_y(int? id, double adjust_x, double set_y) {
      return send_act("MoveFloatingWindow", serialize_fields({ int_member("id", id), obj_member("x", { double_member("AdjustFixed", adjust_x) }), obj_member("y", { double_member("SetFixed", set_y) }) }));
    }

    public static bool move_floating_window_set_x_adjust_y(int? id, double set_x, double adjust_y) {
      return send_act("MoveFloatingWindow", serialize_fields({ int_member("id", id), obj_member("x", { double_member("SetFixed", set_x) }), obj_member("y", { double_member("AdjustFixed", adjust_y) }) }));
    }

    public static bool move_floating_window_set_x_set_y(int? id, double set_x, double set_y) {
      return send_act("MoveFloatingWindow", serialize_fields({ int_member("id", id), obj_member("x", { double_member("SetFixed", set_x) }), obj_member("y", { double_member("SetFixed", set_y) }) }));
    }

    public static bool move_window_down() {
      return send_act("MoveWindowDown");
    }

    public static bool move_window_down_or_to_workspace_down() {
      return send_act("MoveWindowDownOrToWorkspaceDown");
    }

    public static bool move_window_to_floating(int? id) {
      return send_act("MoveWindowToFloating", serialize_fields({ int_member("id", id) }));
    }

    public static bool move_window_to_monitor(int? id, string output) {
      return send_act("MoveWindowToMonitor", serialize_fields({ int_member("id", id), str_member("output", output) }));
    }

    public static bool move_window_to_monitor_down() {
      return send_act("MoveWindowToMonitorDown");
    }

    public static bool move_window_to_monitor_left() {
      return send_act("MoveWindowToMonitorLeft");
    }
    
    public static bool move_window_to_monitor_right() {
      return send_act("MoveWindowToMonitorRight");
    }

    public static bool move_window_to_monitor_up() {
      return send_act("MoveWindowToMonitorUp");
    }

    public static bool move_window_to_monitor_previous() {
      return send_act("MoveWindowToMonitorPrevious");
    }

    public static bool move_window_to_monitor_next() {
      return send_act("MoveWindowToMonitorNext");
    }

    public static bool move_window_to_tiling(int? id) {
      return send_act("MoveWindowToTiling", serialize_fields({ int_member("id", id) }));
    }

    public static bool move_window_to_workspace_by_id(int? window_id, int workspace_id, bool focus) {
      return send_act("MoveWindowToWorkspace", serialize_fields({ obj_member("reference", { int_member("Id", workspace_id) }), int_member("window_id", window_id), bool_member("focus", focus) }));
    }

    public static bool move_window_to_workspace_by_index(int? window_id, int workspace_index, bool focus) {
      return send_act("MoveWindowToWorkspace", serialize_fields({ obj_member("reference", { int_member("Index", workspace_index) }), int_member("window_id", window_id), bool_member("focus", focus) }));
    }

    public static bool move_window_to_workspace_by_name(int? window_id, string workspace_name, bool focus) {
      return send_act("MoveWindowToWorkspace", serialize_fields({ obj_member("reference", { str_member("Name", workspace_name) }), int_member("window_id", window_id), bool_member("focus", focus) }));
    }

    public static bool move_window_to_workspace_down() {
      return send_act("MoveWindowToWorkspaceDown");
    }

    public static bool move_window_to_workspace_up() {
      return send_act("MoveWindowToWorkspaceUp");
    }

    public static bool move_window_up() {
      return send_act("MoveWindowUp");
    }

    public static bool move_window_up_or_to_workspace_up() {
      return send_act("MoveWindowUpOrToWorkspaceUp");
    }

    public static bool move_workspace_down() {
      return send_act("MoveWorkspaceDown");
    }
    
    public static bool move_workspace_to_index_by_id(int workspace_id, int index) {
      return send_act("MoveWorkspaceToIndex", serialize_fields({ obj_member("reference", { int_member("Id", workspace_id) }), int_member("index", index) }));
    }

    public static bool move_workspace_to_index_by_index(int workspace_index, int index) {
      return send_act("MoveWorkspaceToIndex", serialize_fields({ obj_member("reference", { int_member("Index", workspace_index) }), int_member("index", index) }));
    }

    public static bool move_workspace_to_index_by_name(string workspace_name, int index) {
      return send_act("MoveWorkspaceToIndex", serialize_fields({ obj_member("reference", { str_member("Name", workspace_name) }), int_member("index", index) }));
    }

    public static bool move_workspace_to_monitor_by_id(string output, int workspace_id) {
      return send_act("MoveWorkspaceToMonitor", serialize_fields({ str_member("output", output), obj_member("reference", { int_member("Id", workspace_id) }) }));
    }

    public static bool move_workspace_to_monitor_by_index(string output, int workspace_index) {
      return send_act("MoveWorkspaceToMonitor", serialize_fields({ str_member("output", output), obj_member("reference", { int_member("Index", workspace_index) }) }));
    }

    public static bool move_workspace_to_monitor_by_name(string output, string workspace_name) {
      return send_act("MoveWorkspaceToMonitor", serialize_fields({ str_member("output", output), obj_member("reference", { str_member("Name", workspace_name) }) }));
    }

    public static bool move_workspace_to_monitor_down() {
      return send_act("MoveWorkspaceToMonitorDown");
    }

    public static bool move_workspace_to_monitor_left() {
      return send_act("MoveWorkspaceToMonitorLeft");
    }

    public static bool move_workspace_to_monitor_next() {
      return send_act("MoveWorkspaceToMonitorNext");
    }

    public static bool move_workspace_to_monitor_previous() {
      return send_act("MoveWorkspaceToMonitorPrevious");
    }

    public static bool move_workspace_to_monitor_right() {
      return send_act("MoveWorkspaceToMonitorRight");
    }

    public static bool move_workspace_to_monitor_up() {
      return send_act("MoveWorkspaceToMonitorUp");
    }

    public static bool move_workspace_up() {
      return send_act("MoveWorkspaceUp");
    }

    public static bool open_overview() {
      return send_act("OpenOverview");
    }

    public static async double[]? pick_color() {
      var res = yield send_async(fmt_str("PickColor"));
      if ("Ok" in res) {
        try {
          var json = Json.from_string(res).get_object();
          var rgb = json.get_object_member("Ok").get_object_member("PickedColor").get_array_member("rgb");
          double r = rgb.get_double_element(0);
          double g = rgb.get_double_element(1);
          double b = rgb.get_double_element(2);
          double[] col = new double[3];
          col = {r, g, b};
          return col;
        } catch (Error err) {
          critical("command Error: %s", err.message);
        }
      }
      return null;
    }

    public static async Window? pick_window() {
      var res = yield send_async(fmt_str("PickWindow"));
      if ("Ok" in res) {
        try {
          var json = Json.from_string(res).get_object();
          var window_json = json.get_object_member("Ok").get_object_member("PickedWindow");
          var window = Niri.get_default().get_window(window_json.get_int_member("id"));
          return window;
        } catch (Error err) {
          critical("command Error: %s", err.message);
        }
      }
      return null;
    }

    public static bool power_off_monitors() {
      return send_act("PowerOffMonitors");
    }

    public static bool power_on_monitors() {
      return send_act("PowerOnMonitors");
    }

    public static bool quit(bool skip_confirmation) {
      return send_act("Quit", serialize_fields({ bool_member("skip_confirmation", skip_confirmation)}));
    }

    public static bool reset_window_height(int? id) {
      return send_act("ResetWindowHeight", serialize_fields({ int_member("id", id) }));
    }

    public static bool screenshot(bool show_pointer) {
      return send_act("Screenshot", serialize_fields({ bool_member("show_pointer", show_pointer) }));
    }

    public static bool screenshot_screen(bool write_to_disk, bool show_pointer) {
      return send_act("ScreenshotScreen", serialize_fields({ bool_member("write_to_disk", write_to_disk), bool_member("show_pointer", show_pointer)}));
    }
    
    public static bool screenshot_window(int? id, bool write_to_disk) {
      return send_act("ScreenshotWindow", serialize_fields({ int_member("id", id), bool_member("write_to_disk", write_to_disk) }));
    }

    }

    public static bool set_column_display(ColumnDisplayTag display) {
      string display_field;
      switch (display) {
        case ColumnDisplayTag.Normal:
          display_field = "Normal";
          break;
        case ColumnDisplayTag.Tabbed:
          display_field = "Tabbed";
          break;
        default:
          critical("Unknown display tag");
          return false;
      }
      return send_act("SetColumnDisplay", serialize_fields({ str_member("display", display_field) }));
    }

    public static bool set_column_width_adjust_fixed(int? id, int fixed_value) {
      return send_act("SetColumnWidth", serialize_fields({ int_member("id", id), obj_member("change", { int_member("AdjustFixed", fixed_value) }) }));
    }

    public static bool set_column_width_adjust_proportion(int? id, double proportion) {
      return send_act("SetColumnWidth", serialize_fields({ int_member("id", id), obj_member("change", { double_member("AdjustProportion", proportion) }) }));
    }

    public static bool set_column_width_set_fixed(int? id, int fixed_value) {
      return send_act("SetColumnWidth", serialize_fields({ int_member("id", id), obj_member("change", { int_member("SetFixed", fixed_value) }) }));
    }

    public static bool set_column_width_set_proportion(int? id, double proportion) {
      return send_act("SetColumnWidth", serialize_fields({ int_member("id", id), obj_member("change", { double_member("SetProportion", proportion) }) }));
    }

    public static bool set_dynamic_cast_window(int? id) {
      return send_act("SetDynamicCastWindow", serialize_fields({ int_member("id", id) }));
    }

    public static bool set_dynamic_cast_monitor(string? output) {
      return send_act("SetDynamicCastMonitor", serialize_fields({ str_member("output", output) }));
    }

    public static bool set_window_height_adjust_fixed(int? id, int fixed_value) {
      return send_act("SetWindowHeight", serialize_fields({ int_member("id", id), obj_member("change", { int_member("AdjustFixed", fixed_value) }) }));
    }

    public static bool set_window_height_adjust_proportion(int? id, double proportion) {
      return send_act("SetWindowHeight", serialize_fields({ int_member("id", id), obj_member("change", { double_member("AdjustProportion", proportion) }) }));
    }

    public static bool set_window_height_set_fixed(int? id, int fixed_value) {
      return send_act("SetWindowHeight", serialize_fields({ int_member("id", id), obj_member("change", { int_member("SetFixed", fixed_value) }) }));
    }

    public static bool set_window_height_set_proportion(int? id, double proportion) {
      return send_act("SetWindowHeight", serialize_fields({ int_member("id", id), obj_member("change", { double_member("SetProportion", proportion) }) }));
    }

    public static bool set_window_urgent(int id) {
      return send_act("SetWindowUrgent", serialize_fields({ int_member("id", id) }));
    }

    public static bool set_window_width_adjust_fixed(int? id, int fixed_value) {
      return send_act("SetWindowWidth", serialize_fields({ int_member("id", id), obj_member("change", { int_member("AdjustFixed", fixed_value) }) }));
    }

    public static bool set_window_width_adjust_proportion(int? id, double proportion) {
      return send_act("SetWindowWidth", serialize_fields({ int_member("id", id), obj_member("change", { double_member("AdjustProportion", proportion) }) }));
    }

    public static bool set_window_width_set_fixed(int? id, int fixed_value) {
      return send_act("SetWindowWidth", serialize_fields({ int_member("id", id), obj_member("change", { int_member("SetFixed", fixed_value) }) }));
    }

    public static bool set_window_width_set_proportion(int? id, double proportion) {
      return send_act("SetWindowWidth", serialize_fields({ int_member("id", id), obj_member("change", { double_member("SetProportion", proportion) }) }));
    }

    public static bool set_workspace_name_by_id(int workspace_id, string new_name) {
      return send_act("SetWorkspaceName", serialize_fields({ obj_member("reference", { int_member("Id", workspace_id )}), str_member("name", new_name) }));
    }

    public static bool set_workspace_name_by_index(int workspace_index, string new_name) {
      return send_act("SetWorkspaceName", serialize_fields({ obj_member("reference", { int_member("Index", workspace_index )}), str_member("name", new_name) }));
    }

    public static bool set_workspace_name_by_name(string workspace_name, string new_name) {
      return send_act("SetWorkspaceName", serialize_fields({ obj_member("reference", { str_member("Name", workspace_name )}), str_member("name", new_name) }));
    }

    public static bool show_hotkey_overlay() {
      return send_act("ShowHotkeyOverlay");
    }

    public static bool spawn(owned string[] command) {
      return send_act("Spawn", serialize_fields({ str_array_member("command", command) }));
    }

    public static bool swap_window_left() {
      return send_act("SwapWindowLeft");
    }

    public static bool swap_window_right() {
      return send_act("SwapWindowRight");
    }

    public static bool switch_focus_between_floating_and_tiling() {
      return send_act("SwitchFocusBetweenFloatingAndTiling");
    }

    public static bool switch_layout_index(int index) {
      return send_act("SwitchLayout", serialize_fields({ obj_member("layout", { int_member("Index", index) }) }));
    }

    public static bool switch_layout_next() {
      return send_act("SwitchLayout", serialize_fields({ str_member("layout", "Next") }));
    }

    public static bool switch_layout_prev() {
      return send_act("SwitchLayout", serialize_fields({ str_member("layout", "Prev") }));
    }

    public static bool switch_preset_column_width() {
      return send_act("SwitchPresetColumnWidth");
    }

    public static bool switch_preset_window_height(int? id) {
      return send_act("SwitchPresetWindowHeight", serialize_fields({ int_member("id", id) }));
    }

    public static bool switch_preset_window_width(int? id) {
      return send_act("SwitchPresetWindowWidth", serialize_fields({ int_member("id", id) }));
    }

    public static bool toggle_column_tabbed_display() {
      return send_act("ToggleColumnTabbedDisplay");
    }

    public static bool toggle_debug_tint() {
      return send_act("ToggleDebugTint");
    }

    public static bool toggle_keyboard_shortcuts_inhibit() {
      return send_act("ToggleKeyboardShortcutsInhibit");
    }

    public static bool toggle_overview() {
      return send_act("ToggleOverview");
    }

    public static bool toggle_window_floating(int? id) {
      return send_act("ToggleWindowFloating", serialize_fields({ int_member("id", id) }));
    }

    public static bool toggle_window_rule_opacity(int? id) {
      return send_act("ToggleWindowRuleOpacity", serialize_fields({ int_member("id", id) }));
    }

    public static bool toggle_window_urgent(int id) {
      return send_act("ToggleWindowUrgent", serialize_fields({ int_member("id", id) }));
    }

    public static bool toggle_windowed_fullscreen(int? id) {
      return send_act("ToggleWindowedFullscreen", serialize_fields({ int_member("id", id) }));
    }

    public static bool unset_window_urgent(int id) {
      return send_act("UnsetWindowUrgent", serialize_fields({ int_member("id", id) }));
    }

    public static bool unset_workspace_name_by_id(int workspace_id) {
      return send_act("UnsetWorkspaceName", serialize_fields({ obj_member("reference", { int_member("Id", workspace_id) })}));
    }

    public static bool unset_workspace_name_by_index(int workspace_index) {
      return send_act("UnsetWorkspaceName", serialize_fields({ obj_member("reference", { int_member("Index", workspace_index) })}));
    }

    public static bool unset_workspace_name_by_name(string workspace_name) {
      return send_act("UnsetWorkspaceName", serialize_fields({ obj_member("reference", { str_member("Name", workspace_name) })}));
    }
}
}
