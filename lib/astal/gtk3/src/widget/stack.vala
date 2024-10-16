public class Astal.Stack : Gtk.Stack {
    public string shown {
        get { return visible_child_name; }
        set { visible_child_name = value; }
    }

    public List<weak Gtk.Widget> children {
        set { _set_children(value); }
        owned get { return get_children(); }
    }

    private void _set_children(List<weak Gtk.Widget> arr) {
        foreach(var child in get_children()) {
            remove(child);
        }

        var i = 0;
        foreach(var child in arr) {
            if (child.name != null) {
                add_named(child, child.name);
            } else {
                add_named(child, (++i).to_string());
            }
        }
    }
}
