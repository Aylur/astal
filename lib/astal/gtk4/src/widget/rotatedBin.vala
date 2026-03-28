/**
 * A widget which rotates all of its children by an arbitrary angle.
 * It is better to use `transform: rotate(angle);` in css, but this method
 * does not change the allocated space for the widget, which is ficed by this
 * widget. This widget forces it's child widgets to be at their minimum size
 * to keep the size negotiations simple and predictable. This is fine for most
 * widgets. 
 * 
 * ## CSS nodes
 *
 * `AstalRotatedBin` has a single CSS node called `rotated-bin`.
 */
public class Astal.RotatedBin : Gtk.Widget {

    static construct {
        set_css_name("rotated-bin");
    }

    private float _angle = 0;
    public float angle { 
        get {
            return this._angle;
        }
        set {
            this._angle = value;
            this.queue_resize();
        }
    }

    private int child_height = 0;
    private int child_width = 0;

    public override void measure (Gtk.Orientation orientation, int for_size, out int minimum, out int natural, out int minimum_baseline, out int natural_baseline) {
        int min_w = 0, nat_w = 0, min_h = 0, nat_h = 0;
        for (var child = this.get_first_child(); child != null; child = child.get_next_sibling()) {
            int child_min_w, child_nat_w, child_min_h, child_nat_h;
            
            child.measure (Gtk.Orientation.HORIZONTAL, for_size, out child_min_w, out child_nat_w, null, null);
            child.measure (Gtk.Orientation.VERTICAL, for_size, out child_min_h, out child_nat_h, null, null);

            min_w = int.max (min_w, child_min_w);
            min_h = int.max (min_h, child_min_h);
            nat_w = int.max (nat_w, child_nat_w);
            nat_h = int.max (nat_h, child_nat_h);
        }

        var angle_rad = this.angle / 180 * Math.PI;
        
        this.child_height = min_h;
        this.child_width = min_w;

        if (orientation == Gtk.Orientation.HORIZONTAL) {
            minimum = (int)(min_w * Math.fabs (Math.cos(angle_rad)) + min_h * Math.fabs (Math.sin(angle_rad)));
            natural = (int)(nat_w * Math.fabs (Math.cos(angle_rad)) + nat_h * Math.fabs (Math.sin(angle_rad)));
        } else {
            minimum = (int)(min_h * Math.fabs (Math.cos(angle_rad)) + min_w * Math.fabs (Math.sin(angle_rad)));
            natural = (int)(nat_h * Math.fabs (Math.cos(angle_rad)) + nat_w * Math.fabs (Math.sin(angle_rad)));
        }
        
        minimum_baseline = -1;
        natural_baseline = -1;
    }

    public override void size_allocate (int width, int height, int baseline) {
        var cx = width / 2;
        var cy = height / 2;
        
        var child_cx = this.child_width / 2;
        var child_cy = this.child_height / 2;
        
        var transform = new Gsk.Transform()
            .translate ({cx, cy})
            .rotate (this.angle)
            .translate ({-child_cx, -child_cy});

        for (var child = this.get_first_child(); child != null; child = child.get_next_sibling()) {
            child.allocate (this.child_width, this.child_height, -1, transform);
        }
    }
}
