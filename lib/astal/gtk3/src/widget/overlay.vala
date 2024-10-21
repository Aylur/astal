public class Astal.Overlay : Gtk.Overlay {
    public bool pass_through { get; set; }

    /**
     * First [property@Astal.Overlay:overlays] element.
     *
     * WARNING: setting this value will remove every overlay but the first.
     */
    public Gtk.Widget? overlay {
        get { return overlays.nth_data(0); }
        set {
            foreach (var ch in get_children()) {
                if (ch != child)
                    remove(ch);
            }

            if (value != null)
                add_overlay(value);
        }
    }

    /**
     * Sets the overlays of this Overlay. [method@Gtk.Overlay.add_overlay].
     */
    public List<weak Gtk.Widget> overlays {
        owned get { return get_children(); }
        set {
            foreach (var ch in get_children()) {
                if (ch != child)
                    remove(ch);
            }

            foreach (var ch in value)
                add_overlay(ch);
        }
    }

    public new Gtk.Widget? child {
        get { return get_child(); }
        set {
            var ch = get_child();
            if (ch != null)
                remove(ch);

            if (value != null)
                add(value);
        }
    }

    construct {
        notify["pass-through"].connect(() => {
            update_pass_through();
        });
    }

    private void update_pass_through() {
        foreach (var child in get_children())
            set_overlay_pass_through(child, pass_through);
    }

    public new void add_overlay(Gtk.Widget widget) {
        base.add_overlay(widget);
        set_overlay_pass_through(widget, pass_through);
    }
}
