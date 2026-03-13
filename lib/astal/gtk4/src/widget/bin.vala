/**
 * A widget with one child.
 * It is useful for deriving subclasses, since it provides common code needed for handling a single child widget.
 */
public class Astal.Bin : Gtk.Widget, Gtk.Buildable {
    construct { set_layout_manager(new Gtk.BinLayout()); }

    Gtk.Widget _child;
    public Gtk.Widget? child {
        get { return _child; }
        set {
            if (_child != null) {
                _child.unparent();
            }

            if (value != null) {
                _child = value;
                value.set_parent(this);
            }
        }
    }

    void add_child(Gtk.Builder builder, Object child, string? type) {
        if (child is Gtk.Widget) {
            this.child = child as Gtk.Widget;
        } else {
            base.add_child(builder, child, type);
        }
    }

    ~Bin() {
        if (child != null) {
            child.unparent();
        }
    }
}
