namespace Astal {
public class Overlay : Gtk.Overlay {
    public bool pass_through { get; set; }

    public Gtk.Widget overlay {
        get { return overlays.nth_data(0); }
        set {
            foreach (var ch in get_children())
                remove(ch);

            add_overlay(value);
        }
    }

    public List<weak Gtk.Widget> overlays {
        owned get { return get_children(); }
        set {
            foreach (var ch in get_children())
                remove(ch);

            foreach (var ch in value)
                add_overlay(ch);
        }
    }

    public void set_child(Gtk.Widget widget) {
        var ch = get_child();
        if (ch != null) {
            remove(ch);
        }
        add(widget);
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
}
