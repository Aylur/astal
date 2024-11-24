public class Astal.Box : Gtk.Box {
    public List<weak Gtk.Widget> children {
        set {
            foreach (var child in children) {
                remove(child);
            }
            foreach (var child in value) {
                append(child);
            }
        }
        owned get {
            var list = new List<weak Gtk.Widget>();
            var child = get_first_child();
            while (child != null) {
                list.append(child);
                child = child.get_next_sibling();
            }
            return list;
        }
    }

    public Gtk.Widget? child {
        owned get {
            foreach (var child in children) {
                return child;
            }
            return null;
        }
        set {
            var list = new List<weak Gtk.Widget>();
            list.append(child);
            this.children = children;
        }
    }
}
