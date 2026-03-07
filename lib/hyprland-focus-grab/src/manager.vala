namespace AstalHyprlandFocusGrab {
public bool is_supported() {
    return !AstalWl.Registry.get_default().find_globals("hyprland_focus_grab_manager_v1").is_empty();
}

internal class GrabManager : Object {
    private static GrabManager? instance;

    public static GrabManager? get_default() {
        if (instance == null) {
            instance = new GrabManager();
        }
        return instance;
    }

    private HyprlandFocusGrabManagerV1? manager;

    public HyprlandFocusGrabV1? create_grab() {
        if (manager != null) {
            return manager.create_grab();
        } else {
            return null;
        }
    }

    public GrabManager() {
        var registry = AstalWl.get_default();
        var manager_global = registry.find_globals("hyprland_focus_grab_manager_v1").nth_data(0);
        if (manager_global == null) {
            critical("The compositor does not support hyprland_focus_grab_manager_v1");
            return;
        }
        manager = registry.get_registry().bind(manager_global.name, ref HyprlandFocusGrabManagerV1.iface, uint.min(manager_global.version, 1));
    }
}
}
