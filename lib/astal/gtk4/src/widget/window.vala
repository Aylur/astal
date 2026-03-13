using GtkLayerShell;

[Flags]
public enum Astal.WindowAnchor {
    NONE,
    TOP,
    RIGHT,
    LEFT,
    BOTTOM,
}

public enum Astal.Exclusivity {
    NORMAL,
    /**
     * Request the compositor to allocate space for this window.
     */
    EXCLUSIVE,
    /**
     * Request the compositor to stack layers on top of each other.
     */
    IGNORE,
}

public enum Astal.Layer {
    BACKGROUND = 0, // GtkLayerShell.Layer.BACKGROUND
    BOTTOM = 1, // GtkLayerShell.Layer.BOTTOM
    TOP = 2, // GtkLayerShell.Layer.TOP
    OVERLAY = 3, // GtkLayerShell.Layer.OVERLAY
}

public enum Astal.Keymode {
    /**
     * Window should not receive keyboard events.
     */
    NONE = 0, // GtkLayerShell.KeyboardMode.NONE
    /**
     * Window should have exclusive focus if it is on the top or overlay layer.
     */
    EXCLUSIVE = 1, // GtkLayerShell.KeyboardMode.EXCLUSIVE
    /**
     * Focus and Unfocues the window as needed.
     */
    ON_DEMAND = 2, // GtkLayerShell.KeyboardMode.ON_DEMAND
}

/**
 * Subclass of [class@Gtk.Window] which integrates GtkLayerShell as class fields.
 */
public class Astal.Window : Gtk.Window {
    /**
     * Get the current [class@Gdk.Monitor] this window resides in.
     */
    public Gdk.Monitor get_current_monitor() {
        return Gdk.Display.get_default().get_monitor_at_surface(base.get_surface());
    }

    private bool check(string action) {
        if (!is_supported()) {
            critical(@"can not $action on window: layer shell not supported");
            print("tip: running from an xwayland terminal can cause this, for example VsCode");
            return true;
        }
        if (!is_layer_window(this)) {
            init_for_window(this);
        }
        return false;
    }

    construct {
        // If the window has no size allocatoted when it gets mapped.
        // It won't show up later either when it size changes by adding children.
        height_request = 1;
        width_request = 1;
        check("initialize layer shell");
    }

    /**
     * Namespace of this window. This can be used to target the layer in compositor rules.
     */
    public string namespace {
        get { return get_namespace(this); }
        set {
            if(check("set namespace"))
                return;

            set_namespace(this, value);
        }
    }

    /**
     * Edges to anchor the window to.
     *
     * If two perpendicular edges are anchored, the surface will be anchored to that corner.
     * If two opposite edges are anchored, the window will be stretched across the screen in that direction.
     */
    public WindowAnchor anchor {
        set {
            if (check("set anchor"))
                return;

            set_anchor(this, Edge.TOP, WindowAnchor.TOP in value);
            set_anchor(this, Edge.BOTTOM, WindowAnchor.BOTTOM in value);
            set_anchor(this, Edge.LEFT, WindowAnchor.LEFT in value);
            set_anchor(this, Edge.RIGHT, WindowAnchor.RIGHT in value);
        }
        get {
            var a = 0;
            if (get_anchor(this, Edge.TOP))
                a = a | WindowAnchor.TOP;

            if (get_anchor(this, Edge.RIGHT))
                a = a | WindowAnchor.RIGHT;

            if (get_anchor(this, Edge.LEFT))
                a = a | WindowAnchor.LEFT;

            if (get_anchor(this, Edge.BOTTOM))
                a = a | WindowAnchor.BOTTOM;

            if (a == 0)
                return WindowAnchor.NONE;

            return a;
        }
    }

    /**
     * Exclusivity of this window.
     */
    public Exclusivity exclusivity {
        set {
            if (check("set exclusivity"))
                return;

            switch (value) {
                case Exclusivity.NORMAL:
                    set_exclusive_zone(this, 0);
                    break;
                case Exclusivity.EXCLUSIVE:
                    auto_exclusive_zone_enable(this);
                    break;
                case Exclusivity.IGNORE:
                    set_exclusive_zone(this, -1);
                    break;
            }
        }
        get {
            if (auto_exclusive_zone_is_enabled(this))
                return Exclusivity.EXCLUSIVE;

            if (get_exclusive_zone(this) == -1)
                return Exclusivity.IGNORE;

            return Exclusivity.NORMAL;
        }
    }

    /**
     * Which layer to appear this window on.
     */
    public Layer layer {
        get { return (Layer)get_layer(this); }
        set {
            if (check("set layer"))
                return;

            set_layer(this, (GtkLayerShell.Layer)value);
        }
    }

    /**
     * Keyboard mode of this window.
     */
    public Keymode keymode {
        get { return (Keymode)get_keyboard_mode(this); }
        set {
            if (check("set keymode"))
                return;

            set_keyboard_mode(this, (GtkLayerShell.KeyboardMode)value);
        }
    }

    /**
     * Which monitor to appear this window on.
     */
    public Gdk.Monitor gdkmonitor {
        get { return get_monitor(this); }
        set {
            if (check("set gdkmonitor"))
                return;

            set_monitor (this, value);
        }
    }

    public new int margin_top {
        get { return GtkLayerShell.get_margin(this, Edge.TOP); }
        set {
            if (check("set margin_top"))
                return;

            GtkLayerShell.set_margin(this, Edge.TOP, value);
        }
    }

    public new int margin_bottom {
        get { return GtkLayerShell.get_margin(this, Edge.BOTTOM); }
        set {
            if (check("set margin_bottom"))
                return;

            GtkLayerShell.set_margin(this, Edge.BOTTOM, value);
        }
    }

    public new int margin_left {
        get { return GtkLayerShell.get_margin(this, Edge.LEFT); }
        set {
            if (check("set margin_left"))
                return;

            GtkLayerShell.set_margin(this, Edge.LEFT, value);
        }
    }

    public new int margin_right {
        get { return GtkLayerShell.get_margin(this, Edge.RIGHT); }
        set {
            if (check("set margin_right"))
                return;

            GtkLayerShell.set_margin(this, Edge.RIGHT, value);
        }
    }

    public new int margin {
        set {
            if (check("set margin"))
                return;

            margin_top = value;
            margin_right = value;
            margin_bottom = value;
            margin_left = value;
        }
    }

    /**
     * Which monitor to appear this window on.
     *
     * CAUTION: the id might not be the same mapped by the compositor.
     */
    public int monitor {
        set {
            if (check("set monitor"))
                return;

            if (value < 0)
                set_monitor(this, (Gdk.Monitor)null);

            var m = (Gdk.Monitor)Gdk.Display.get_default().get_monitors().get_item(value);
            set_monitor(this, m);
        }
        get {
            var m = get_monitor(this);
            var mons = Gdk.Display.get_default().get_monitors();
            for (var i = 0; i < mons.get_n_items(); ++i) {
                if (m == mons.get_item(i))
                    return i;
            }

            return -1;
        }
    }
}
