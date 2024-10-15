public class Astal.Box : Gtk.Box {
    [CCode (notify = false)]
    public bool vertical {
        get { return orientation == Gtk.Orientation.VERTICAL; }
        set { orientation = value ? Gtk.Orientation.VERTICAL : Gtk.Orientation.HORIZONTAL; }
    }

    public List<weak Gtk.Widget> children {
        set { _set_children(value); }
        owned get { return get_children(); }
    }

    public new Gtk.Widget child {
        owned get { return _get_child(); }
        set { _set_child(value); }
    }

    construct {
        notify["orientation"].connect(() => {
            notify_property("vertical");
        });
    }

    private void _set_child(Gtk.Widget child) {
        var list = new List<weak Gtk.Widget>();
        list.append(child);
        _set_children(list);
    }

    private Gtk.Widget? _get_child() {
        foreach(var child in get_children())
            return child;

        return null;
    }

    private void _set_children(List<weak Gtk.Widget> arr) {
        foreach(var child in get_children()) {
            remove(child);
        }

        foreach(var child in arr)
            add(child);
    }

    public Box(bool vertical, List<weak Gtk.Widget> children) {
        this.vertical = vertical;
        _set_children(children);
    }
}
