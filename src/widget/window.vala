using GtkLayerShell;

namespace Astal {
public enum WindowAnchor {
    NONE = 0,
    TOP = 1,
    RIGHT = 2,
    LEFT = 4,
    BOTTOM = 8,
}

public enum Exclusivity {
    NORMAL,
    EXCLUSIVE,
    IGNORE,
}

public enum Layer {
    TOP = GtkLayerShell.Layer.TOP,
    OVERLAY = GtkLayerShell.Layer.OVERLAY,
    BOTTOM = GtkLayerShell.Layer.BOTTOM,
    BACKGROUND = GtkLayerShell.Layer.BACKGROUND,
}

public enum Keymode {
    NONE = GtkLayerShell.KeyboardMode.NONE,
    ON_DEMAND = GtkLayerShell.KeyboardMode.ON_DEMAND,
    EXCLUSIVE = GtkLayerShell.KeyboardMode.EXCLUSIVE,
}

public class Window : Gtk.Window {
    construct {
        height_request = 1;
        width_request = 1;
        init_for_window(this);
        set_namespace(this, name);
        notify["name"].connect(() => set_namespace(this, name));
    }

    public int anchor {
        set {
            set_anchor(this, Edge.TOP, WindowAnchor.TOP in value);
            set_anchor(this, Edge.BOTTOM, WindowAnchor.BOTTOM in value);
            set_anchor(this, Edge.LEFT, WindowAnchor.LEFT in value);
            set_anchor(this, Edge.RIGHT, WindowAnchor.RIGHT in value);
        }
        get {
            var a = WindowAnchor.NONE;
            if (get_anchor(this, Edge.TOP))
                a = a | WindowAnchor.TOP;

            if (get_anchor(this, Edge.RIGHT))
                a = a | WindowAnchor.RIGHT;

            if (get_anchor(this, Edge.LEFT))
                a = a | WindowAnchor.LEFT;

            if (get_anchor(this, Edge.BOTTOM))
                a = a | WindowAnchor.BOTTOM;

            return a;
        }
    }

    public Exclusivity exclusivity {
        set {
            switch (value) {
                case Exclusivity.NORMAL:
                    set_exclusive_zone(this, 0);
                    break;
                case Exclusivity.EXCLUSIVE:
                    auto_exclusive_zone_enable (this);
                    break;
                case Exclusivity.IGNORE:
                    set_exclusive_zone(this, -1);
                    break;
            }
        }
        get {
            if (auto_exclusive_zone_is_enabled (this))
                return Exclusivity.EXCLUSIVE;

            if (get_exclusive_zone(this) == -1)
                return Exclusivity.IGNORE;

            return Exclusivity.NORMAL;
        }
    }

    public Layer layer {
        get { return (Layer)get_layer(this); }
        set { set_layer(this, (GtkLayerShell.Layer)value); }
    }

    public Keymode keymode {
        set { set_keyboard_mode(this, (GtkLayerShell.KeyboardMode)value); }
        get { return (Keymode)get_keyboard_mode(this); }
    }

    public Gdk.Monitor gdkmonitor {
        set { set_monitor (this, value); }
        get { return get_monitor(this); }
    }

    public new int[] margin {
        owned get {
            return {
                GtkLayerShell.get_margin(this, Edge.TOP),
                GtkLayerShell.get_margin(this, Edge.RIGHT),
                GtkLayerShell.get_margin(this, Edge.BOTTOM),
                GtkLayerShell.get_margin(this, Edge.LEFT),
            };
        }
        set {
            int top = 0, right = 0, bottom = 0, left = 0;
            switch (value.length) {
                case 1:
                    top = value[0];
                    right = value[0];
                    bottom = value[0];
                    left = value[0];
                    break;
                case 2:
                    top = value[0];
                    right = value[1];
                    bottom = value[0];
                    left = value[1];
                    break;
                case 3:
                    top = value[0];
                    right = value[1];
                    bottom = value[2];
                    left = value[1];
                    break;
                case 4:
                    top = value[0];
                    right = value[1];
                    bottom = value[2];
                    left = value[3];
                    break;
                default: break;
            }
            GtkLayerShell.set_margin(this, Edge.TOP, top);
            GtkLayerShell.set_margin(this, Edge.LEFT, left);
            GtkLayerShell.set_margin(this, Edge.BOTTOM, bottom);
            GtkLayerShell.set_margin(this, Edge.RIGHT, right);
        }
    }

    /**
     * CAUTION: the id might not be the same mapped by the compositor
     * to reset and let the compositor map it pass a negative number
     */
    public int monitor {
        set {
            if (value < 0)
                set_monitor(this, (Gdk.Monitor)null);

            var m = Gdk.Display.get_default().get_monitor(value);
            set_monitor(this, m);
        }
        get {
            var m = get_monitor(this);
            var d = Gdk.Display.get_default();
            for (var i = 0; i < d.get_n_monitors(); ++i) {
                if (m == d.get_monitor(i))
                    return i;
            }

            return -1;
        }
    }
}

/**
 * CAUTION: the id might not be the same mapped by the compositor
 */
public uint get_num_monitors() {
    return Gdk.Display.get_default().get_n_monitors();
}
}
