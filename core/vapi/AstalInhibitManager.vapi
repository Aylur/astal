[CCode (cprefix = "Astal", gir_namespace = "Astal", lower_case_cprefix = "astal_")]
namespace Astal {
	[CCode (cheader_filename = "idle-inhibit.h", type_id = "astal_idle_inhibit_manager_get_type()")]
	public class InhibitManager : GLib.Object {
    public static unowned InhibitManager? get_default();
		public Inhibitor inhibit (Gtk.Window window);
	}
  [CCode (cheader_filename = "idle-inhibit.h", free_function = "zwp_idle_inhibitor_v1_destroy")]
  [Compact]
	public class Inhibitor {
	}

}
