namespace AstalHyprlandFocusGrab {
public class Grab : Object {
    private HyprlandFocusGrabV1? wl_grab;
    private const HyprlandFocusGrabV1Listener grab_listener = {
        handle_cleared,
    };

    public Grab() {
        wl_grab = GrabManager.get_default().create_grab();
        wl_grab.add_listener(grab_listener, this);
    }

    /**
     * Add a surface to the grab. Doesn't take effect until commit() is called.
     */
    public void add(Gdk.Surface surface) {
        if (wl_grab == null) {
            return;
        }

        var wl_surface = surface as Gdk.Wayland.Surface;
        if (wl_surface == null) {
            critical("surface isn't a Wayland surface");
            return;
        }

        wl_grab.add_surface(wl_surface.get_wl_surface());
    }

    /**
     * Remove a surface from the grab. Doesn't take effect untill commit() is called.
     * Note that surfaces are removed automatically once they are unrealized.
     */
    public void remove(Gdk.Surface surface) {
        if (wl_grab == null) {
            return;
        }

        var wl_surface = surface as Gdk.Wayland.Surface;
        if (wl_surface == null) {
            critical("surface isn't a Wayland surface");
            return;
        }
        wl_grab.remove_surface(wl_surface.get_wl_surface());
    }

    /**
     * Apply the pending state. The grab will only be active if at least one surface was added to it.
     */
    public void commit() {
        if (wl_grab == null) {
            return;
        }

        wl_grab.commit();
    }

    /**
     * Destroy the underlying Wayland object, making this grab inert.
     */
    public void destroy() {
        if (wl_grab == null) {
            return;
        }
        wl_grab = null;
    }

    private void handle_cleared() {
        cleared();
    }

    public signal void cleared();
}
}
