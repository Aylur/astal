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
            if (value) {
                if (grab != null) {
                    return;
                }
                // create a grab
                assert_null(grab);
                grab = new Grab();
                windows.foreach((window) => {
                        var surf = window.get_surface();
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
                // destroy the grab
                grab.destroy();
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

        var surf = window.get_surface();
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

        var surf = window.get_surface();
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
            grab.add(window.get_surface());
            grab.commit();
        }
    }

    private void handle_cleared() {
        active = false;
        cleared();
    }
}
}
