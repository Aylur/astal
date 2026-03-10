namespace AstalHyprlandFocusGrab {
internal struct WindowSignalHandlers {
    ulong realize;
    ulong destroy;
}

public class GrabContext : Object {
    private Grab? grab;
    private HashTable<Gtk.Window, WindowSignalHandlers?> windows;

    /** Whether the grab is active (a click outside any surface added to this object will be consumed). */
    public bool active {
        get {
            return grab != null;
        }

        set {
            // create or destroy the grab
            if (value) {
                if (grab != null) {
                    return;
                }
                assert_null(grab);
                grab = new Grab();
                windows.foreach((window) => {
                        unowned var surf = get_wl_surface_for_window(window);
                        if (surf != null) {
                            grab.add(surf);
                        }
                    });
                grab.commit();
                grab.cleared.connect(handle_cleared);
            } else {
                if (grab == null) {
                    return;
                }
                grab = null;
            }
        }
    }

    construct {
        windows = new HashTable<Gtk.Window, WindowSignalHandlers?>(null, null);
    }

    public void add(Gtk.Window window) {
        WindowSignalHandlers? handlers = {};
        handlers.realize = Signal.connect_object(window, "realize", (Callback)call_handle_realized, this, 0);
        handlers.destroy = Signal.connect_object(window, "destroy", (Callback)call_destroy, this, 0);
        windows.set(window, handlers);

        unowned var surf = get_wl_surface_for_window(window);
        if ((grab != null) && (surf != null)) {
            grab.add(surf);
            grab.commit();
        }
    }
    public void remove(Gtk.Window window) {
        bool exists;
        WindowSignalHandlers? handlers = windows.take(window, out exists);
        if (!exists) {
            return;
        }
        window.disconnect(handlers.realize);
        window.disconnect(handlers.destroy);

        unowned var surf = get_wl_surface_for_window(window);
        if ((grab != null) && (surf != null)) {
            grab.remove(surf);
            grab.commit();
        }
    }

    public signal void cleared();

    private static void call_handle_realized(Gtk.Window window, GrabContext self) {
        self.handle_realized(window);
    }

    private static void call_destroy(Gtk.Window window, GrabContext self) {
        self.remove(window);
    }

    private void handle_realized(Gtk.Window window) {
        if (grab != null) {
            grab.add(get_wl_surface_for_window(window));
            grab.commit();
        }
    }

    private void handle_cleared() {
        active = false;
        cleared();
    }

    private unowned Wl.Surface? get_wl_surface_for_window(Gtk.Window window) {
        var surf = window.get_surface();
        if (surf == null) {
            return null;
        }
        var wayland_surf = surf as Gdk.Wayland.Surface;
        if (wayland_surf == null) {
            critical("Not a Wayland surface");
        }
        return wayland_surf.get_wl_surface();
    }
}
}
