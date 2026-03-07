namespace AstalHyprlandFocusGrab {
public class Grab : Object {
    private HyprlandFocusGrabV1? wl_grab;
    private const HyprlandFocusGrabV1Listener grab_listener = {
        handle_cleared,
    };

    public Grab() {
        wl_grab = GrabManager.get_default().create_grab();
    }

    public void add(Gdk.Surface surface) {
        if (wl_grab == null) {
            // unsupported, return to not cause log spam
            return;
        }

        var wl_surface = surface as Gdk.Wayland.Surface;
        if (wl_surface != null) {
            critical("surface isn't a Wayland surface");
            return;
        }

        wl_grab.add_surface(wl_surface.get_wl_surface());
    }

    public void remove(Gdk.Surface surface) {
        if (wl_grab == null) {
            // unsupported, return to not cause log spam
            return;
        }

        var wl_surface = surface as Gdk.Wayland.Surface;
        if (wl_surface != null) {
            critical("surface isn't a Wayland surface");
            return;
        }
        wl_grab.remove_surface(wl_surface.get_wl_surface());
    }

    public void commit() {
        if (wl_grab == null) {
            // unsupported, return to not cause log spam
            return;
        }

        wl_grab.commit();
    }

    private void handle_cleared() {
        cleared();
    }

    public signal void cleared();
}
}
