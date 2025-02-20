/**
 * Helper widget to create custom widgets
 *
 * Based on private GtkGizmo widget
 */
public class Astal.Gizmo : Gtk.Widget {
    public delegate void MeasureFunc(
        Gtk.Orientation orientation, int for_size,
        out int minimum, out int natural,
        out int minimum_baseline, out int natural_baseline
    );

    public delegate void AllocateFunc(
        int width,
        int height,
        int baseline
    );

    public delegate void SnapshotFunc(
        Gtk.Snapshot snapshot
    );

    public delegate bool ContainsFunc(
        double x,
        double y
    );

    public delegate bool FocusFunc(
        Gtk.DirectionType direction
    );

    public delegate bool GrabFocusFunc();

    private MeasureFunc? measure_func;
    private AllocateFunc? allocate_func;
    private SnapshotFunc? snapshot_func;
    private ContainsFunc? contains_func;
    private FocusFunc? focus_func;
    private GrabFocusFunc? grab_focus_func;

    /**
     * Create a new Gizmo
     *
     * @param css_name CSS class name. CSS Node
     * @param measure_func Optional function for measuring the widget
     * @param allocate_func Optional function for allocating size
     * @param snapshot_func Optional function for snapshotting the widget
     * @param contains_func Optional function for checking if a point is within the widget
     * @param focus_func Optional function for handling focus behaviour
     * @param grab_focus_func Optional function for grabbing focus
     */
    public Gizmo(
        string css_name,
        MeasureFunc? measure_func,
        AllocateFunc? allocate_func,
        SnapshotFunc? snapshot_func,
        ContainsFunc? contains_func,
        FocusFunc? focus_func,
        GrabFocusFunc? grab_focus_func
    ) {
        Object(css_name: css_name);
        this.measure_func = measure_func;
        this.allocate_func = allocate_func;
        this.snapshot_func = snapshot_func;
        this.contains_func = contains_func;
        this.focus_func = focus_func;
        this.grab_focus_func = grab_focus_func;
    }

    /**
     * Create a new Gizmo with a specific role
     *
     * @param css_name CSS class name. CSS Node
     * @param role Accessible role
     * @param measure_func Optional function for measuring the widget
     * @param allocate_func Optional function for allocating size
     * @param snapshot_func Optional function for snapshotting the widget
     * @param contains_func Optional function for checking if a point is within the widget
     * @param focus_func Optional function for handling focus behaviour
     * @param grab_focus_func Optional function for grabbing focus
     */
    public Gizmo.with_role(
        string css_name,
        Gtk.AccessibleRole role,
        MeasureFunc? measure_func,
        AllocateFunc? allocate_func,
        SnapshotFunc? snapshot_func,
        ContainsFunc? contains_func,
        FocusFunc? focus_func,
        GrabFocusFunc? grab_focus_func
    ) {
        Object(css_name: css_name, accessible_role: role);
        this.measure_func = measure_func;
        this.allocate_func = allocate_func;
        this.snapshot_func = snapshot_func;
        this.contains_func = contains_func;
        this.focus_func = focus_func;
        this.grab_focus_func = grab_focus_func;
    }

    public override void measure(
        Gtk.Orientation orientation,
        int for_size,
        out int minimum,
        out int natural,
        out int minimum_baseline,
        out int natural_baseline
    ) {
        if (measure_func != null) {
            measure_func(orientation, for_size,
                         out minimum, out natural,
                         out minimum_baseline, out natural_baseline);
        } else {
            minimum = natural = minimum_baseline = natural_baseline = 0;
        }
    }

    public override void size_allocate(int width, int height, int baseline) {
        if (allocate_func != null) {
            allocate_func(width, height, baseline);
        }
    }

    public override void snapshot(Gtk.Snapshot snapshot) {
        if (snapshot_func != null) {
            snapshot_func(snapshot);
        } else {
            base.snapshot(snapshot);
        }
    }

    public override bool contains(double x, double y) {
        if (contains_func != null) {
            return contains_func(x, y);
        }
        return base.contains(x, y);
    }

    public override bool focus(Gtk.DirectionType direction) {
        if (focus_func != null) {
            return focus_func(direction);
        }
        return false;
    }

    public override bool grab_focus() {
        if (grab_focus_func != null) {
            return grab_focus_func();
        }
        return false;
    }
}
