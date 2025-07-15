/**
 * A circular progress bar widget for GTK4.
 *
 * CircularProgressBar is a custom widget that displays progress in a circular format.
 * It supports various styling options including center filling, radius filling, and
 * customizable line properties.
 */
public class Astal.CircularProgressBar : Gtk.Widget, Gtk.Buildable {
    private Astal.Gizmo _progress_arc;
    private Astal.Gizmo _center_fill;
    private Astal.Gizmo _radius_fill;
    private Gtk.Widget _child;

    private int _line_width;
    private double _percentage;
    private double _start_at;
    private double _end_at;

    /**
     * Emitted when the start and end angles are normalized.
     *
     * This signal is emitted in the following cases:
     * - When end_at becomes less than start_at, causing them to swap
     * - When the angles are swapped, the progress direction is automatically inverted to maintain visual consistency
     *
     * Connect to this signal to be notified when the angle normalization occurs
     * and the progress direction changes.
     */
    public signal void angles_changed();

    /**
     * Whether the progress bar is inverted:
     * - True: Clockwise
     * - False: Counter-clockwise
     */
    public bool inverted { get; set; }

    /**
     * Whether the center of the circle is filled.
     */
    public bool center_filled { get; set; }

    /**
     * Whether the radius area is filled.
     */
    public bool radius_filled { get; set; }

    /**
     * The width of the circle's radius line.
     *
     * The line width in pixels. If the value is 0, then the CircularProgress will be shown as a pie chart.
     */
    public int line_width {
        get { return _line_width; }
        set {
            if (value < 0) {
                _line_width = 0;
            } else {
                _line_width = value;
            }
        }
    }

    /**
     * The line cap style for the progress stroke.
     *
     * Check [[https://docs.gtk.org/gsk4/enum.LineCap.html]] for more information.
     */
    public Gsk.LineCap line_cap { get; set; }

    /**
     * The fill rule for the center fill area.
     *
     * Check [[https://docs.gtk.org/gsk4/enum.FillRule.html]] for more information.
     */
    public Gsk.FillRule fill_rule { get; set; }

    /**
     * The progress value between 0.0 and 1.0.
     *
     * Values outside [0.0, 1.0] will be clamped.
     */
    public double percentage {
        get { return _percentage; }
        set {
            if (_percentage != value) {
                if (value > 1.0) {
                    _percentage = 1.0;
                } else if (value < 0.0) {
                    _percentage = 0.0;
                } else {
                    _percentage = value;
                }
            }
        }
    }

    /**
     * The starting position (-1.0 to 1.0).
     * - -1.0 = -2π degrees (3 o'clock)
     * - -0.5 = π degrees (9 o'clock)
     * - 0.0 = 0 degrees (3 o'clock)
     * - 0.5 = π degrees (9 o'clock)
     * - 1.0 = 2π degrees (3 o'clock)
     */
    public double start_at {
        get { return _start_at; }
        set {
            if (value < -1.0) {
                _start_at = -1.0;
            } else if (value > 1.0) {
                _start_at = 1.0;
            } else {
                _start_at = value;
            }
            normalize_angles();
        }
    }

    /**
     * The ending position (-1.0 to 1.0).
     *
     * Similar to {@link start_at}, this property determines the angular position
     * where the circular progress indicator ends.
     */
    public double end_at {
        get { return _end_at; }
        set {
            if (value < -1.0) {
                _end_at = -1.0;
            } else if (value > 1.0) {
                _end_at = 1.0;
            } else {
                _end_at = value;
            }
            normalize_angles();
        }
    }

    private void normalize_angles() {
        if (_end_at < _start_at) {
            var temp = _end_at;
            _end_at = _start_at;
            _start_at = temp;
            inverted = !inverted;
            angles_changed();
        }
    }

    /**
     * The child widget contained within the circular progress.
     */
    public Gtk.Widget? child {
        get { return _child; }
        set {
            if (_child == value) {
                return;
            }

            if (_child != null) {
                _child.unparent();
            }

            _child = value;

            if (_child != null) {
                _child.set_parent(this);
            }
        }
    }

    /*
     * Implements Gtk.Buildable interface.
     */
    public void add_child(Gtk.Builder builder, GLib.Object child, string? type) {
        if (child is Gtk.Widget) {
            this.child = (Gtk.Widget)child;
        } else {
            base.add_child(builder, child, type);
        }
    }

    static construct {
        set_css_name("circularprogress");
    }

    construct {
        _progress_arc = new Gizmo(
            "progress",
            calculate_measurement,
            null,
            progress_arc_snapshot,
            null, null, null
        );

        _start_at = 0.0;
        _end_at = 1;

        _center_fill = new Gizmo(
            "center",
            calculate_measurement,
            null,
            draw_center_fill,
            null, null, null
        );

        _radius_fill = new Gizmo(
            "radius",
            calculate_measurement,
            null,
            draw_radius_fill,
            null, null, null
        );

        _progress_arc.set_parent(this);
        _center_fill.set_parent(this);
        _radius_fill.set_parent(this);

        notify.connect(() => {
            queue_draw();
        });
    }

    public CircularProgressBar() {
        Object(
            name : "circularprogress"
        );
    }

    protected override void dispose() {
        if (_child != null) {
            _child.unparent();
            _child = null;
        }

        if (_progress_arc != null) {
            _progress_arc.unparent();
            _progress_arc = null;
        }

        if (_center_fill != null) {
            _center_fill.unparent();
            _center_fill = null;
        }

        if (_radius_fill != null) {
            _radius_fill.unparent();
            _radius_fill = null;
        }

        base.dispose();
    }

    protected override void snapshot(Gtk.Snapshot snapshot) {
        var width = get_width();
        var height = get_height();
        var radius = float.min(width / 2.0f, height / 2.0f) - 1;

        // Adjust delta based on line width to prevent overflow
        float half_line_width = (float)line_width / 2.0f;
        var delta = radius - half_line_width;

        if (delta < 0) {
            delta = 0;
        }

        var actual_line_width = (float)line_width;
        if (actual_line_width > radius * 2) {
            actual_line_width = radius * 2;
        }

        // Draw in correct order: background to foreground
        if (center_filled) {
            _center_fill.snapshot(snapshot);
        }

        if (radius_filled) {
            _radius_fill.snapshot(snapshot);
        }

        _progress_arc.snapshot(snapshot);

        if (_child != null) {
            snapshot_child(_child, snapshot);
        }
    }

    protected override Gtk.SizeRequestMode get_request_mode() {
        return Gtk.SizeRequestMode.WIDTH_FOR_HEIGHT;
    }

    protected override void size_allocate(int width, int height, int baseline) {
        var radius = float.min(width / 2.0f, height / 2.0f) - 1;
        var half_line_width = (float)line_width / 2.0f;
        var delta = radius - half_line_width;

        if (delta < 0) {
            delta = 0;
        }

        if (_child != null) {
            var max_child_size = (int)(delta * Math.sqrt(2));

            var child_x = (width - max_child_size) / 2;
            var child_y = (height - max_child_size) / 2;

            var child_allocation = Gtk.Allocation() {
                x = child_x,
                y = child_y,
                width = max_child_size,
                height = max_child_size
            };

            _child.allocate_size(child_allocation, baseline);
        }
    }

    protected override void measure(Gtk.Orientation orientation,
                                    int for_size,
                                    out int minimum,
                                    out int natural,
                                    out int minimum_baseline,
                                    out int natural_baseline) {
        minimum = natural = 0;
        minimum_baseline = natural_baseline = -1;

        // Get child's size requirements if it exists
        if (_child != null) {
            int child_minimum, child_natural;
            int child_minimum_baseline, child_natural_baseline;

            _child.measure(orientation, for_size,
                           out child_minimum, out child_natural,
                           out child_minimum_baseline, out child_natural_baseline);

            var padding = (int)(_line_width * 4);
            minimum = child_minimum + padding;
            natural = child_natural + padding;
        } else {
            minimum = natural = 40;
        }
    }

    private void calculate_measurement(Gtk.Orientation orientation,
                                       int for_size,
                                       out int minimum,
                                       out int natural,
                                       out int minimum_baseline,
                                       out int natural_baseline) {
        minimum = natural = get_width();
        minimum_baseline = natural_baseline = -1;
    }

    private struct ArcPoints {
        public float start_x;
        public float start_y;
        public float end_x;
        public float end_y;
    }

    private void draw_full_circle(Gsk.PathBuilder path_builder, float center_x, float center_y, float delta) {
        path_builder.add_circle(
            Graphene.Point().init(center_x, center_y),
            delta
        );
    }

    private void draw_arc(Gsk.PathBuilder path_builder,
                          double start_angle,
                          double progress_angle,
                          double sweep_angle,
                          float center_x,
                          float center_y,
                          float delta,
                          bool as_pie = false) {
        var points = calculate_arc_points(start_angle, progress_angle, center_x, center_y, delta);
        bool large_arc = (_percentage * sweep_angle).abs() > Math.PI;

        if (as_pie) {
            path_builder.move_to(center_x, center_y);
            path_builder.line_to(points.start_x, points.start_y);
        } else {
            path_builder.move_to(points.start_x, points.start_y);
        }

        path_builder.svg_arc_to(
            delta, delta, 0.0f,
            large_arc, inverted,
            points.end_x, points.end_y
        );

        if (as_pie) {
            path_builder.line_to(center_x, center_y);
            path_builder.close();
        }
    }

    private ArcPoints calculate_arc_points(double start_angle, double progress_angle, float center_x, float center_y, float delta) {
        return ArcPoints() {
                   start_x = center_x + (float)(delta * Math.cos(start_angle)),
                   start_y = center_y + (float)(delta * Math.sin(start_angle)),
                   end_x = center_x + (float)(delta * Math.cos(progress_angle)),
                   end_y = center_y + (float)(delta * Math.sin(progress_angle))
        };
    }

    private void progress_arc_snapshot(Gtk.Snapshot snapshot) {
        if (_percentage <= 0) {
            return;
        }

        int width = get_width();
        int height = get_height();
        float radius = float.min(width / 2.0f, height / 2.0f) - 1;
        float half_line_width = (float)line_width / 2.0f;
        float delta = radius - half_line_width;
        float center_x = width / 2.0f;
        float center_y = height / 2.0f;

        if (delta < 0) {
            delta = 0;
        }

        float actual_line_width = (float)line_width;
        if (actual_line_width > radius * 2) {
            actual_line_width = radius * 2;
        }

        var path_builder = new Gsk.PathBuilder();
        var color = _progress_arc.get_color();

        double start_angle = _start_at * 2 * Math.PI;
        double end_angle = _end_at * 2 * Math.PI;

        double sweep_angle = end_angle - start_angle;
        if (end_angle < start_angle) {
            critical("End angle is less than start angle");
        }
        if (sweep_angle.abs() > 2 * Math.PI) {
            sweep_angle = sweep_angle.abs() > 0 ? 2 * Math.PI : -2 * Math.PI;
            end_angle = start_angle + sweep_angle;
        }

        double progress_angle = start_angle;
        if (inverted) {
            progress_angle += (_percentage * sweep_angle);
        } else {
            progress_angle -= (_percentage * sweep_angle);
        }

        progress_angle = progress_angle % (2 * Math.PI);
        if (progress_angle < 0) {
            progress_angle += 2 * Math.PI;
        }

        bool as_pie = actual_line_width <= 0;
        bool as_circle = _percentage >= 1.0 && sweep_angle.abs() >= 2 * Math.PI;

        // Draw as pie when actual_line_width is 0
        if (as_pie) {
            path_builder.move_to(center_x, center_y);

            if (as_circle) {
                draw_full_circle(path_builder, center_x, center_y, delta);
            } else {
                draw_arc(path_builder, start_angle, progress_angle, sweep_angle, center_x, center_y, delta, true);
            }

            snapshot.append_fill(path_builder.to_path(), Gsk.FillRule.EVEN_ODD, color);
        } else {
            // Original stroke drawing code
            if (as_circle) {
                draw_full_circle(path_builder, center_x, center_y, delta);
            } else {
                draw_arc(path_builder, start_angle, progress_angle, sweep_angle, center_x, center_y, delta, false);
            }

            var stroke = new Gsk.Stroke(actual_line_width);
            stroke.set_line_cap(_line_cap);
            snapshot.append_stroke(path_builder.to_path(), stroke, color);
        }
    }

    private void draw_center_fill(Gtk.Snapshot snapshot) {
        if (!center_filled) {
            return;
        }

        var width = get_width();
        var height = get_height();
        var radius = float.min(width / 2.0f, height / 2.0f) - 1;
        var half_line_width = (float)line_width / 2.0f;
        var delta = radius - half_line_width;

        if (delta < 0) {
            delta = 0;
        }

        var color = _center_fill.get_color();
        var path_builder = new Gsk.PathBuilder();

        path_builder.add_circle(
            Graphene.Point().init(width / 2.0f, height / 2.0f),
            delta
        );

        snapshot.append_fill(
            path_builder.to_path(),
            fill_rule,
            color
        );
    }

    private void draw_radius_fill(Gtk.Snapshot snapshot) {
        if (!radius_filled) {
            return;
        }

        var width = get_width();
        var height = get_height();
        var radius = float.min(width / 2.0f, height / 2.0f) - 1;
        var half_line_width = (float)line_width / 2.0f;
        var delta = radius - half_line_width;
        var center_x = width / 2.0f;
        var center_y = height / 2.0f;

        if (delta < 0) {
            delta = 0;
        }

        var color = _radius_fill.get_color();
        var path_builder = new Gsk.PathBuilder();
        var as_pie = _line_width <= 0;

        if (as_pie) {
            draw_full_circle(path_builder, center_x, center_y, delta);
            snapshot.append_fill(path_builder.to_path(), Gsk.FillRule.EVEN_ODD, color);
        } else {
            draw_full_circle(path_builder, center_x, center_y, delta);
            var stroke = new Gsk.Stroke(_line_width);
            snapshot.append_stroke(path_builder.to_path(), stroke, color);
        }
    }
}
