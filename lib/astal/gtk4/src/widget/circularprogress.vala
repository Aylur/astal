/**
 * A circular progress bar widget for GTK4.
 *
 * CircularProgressBar is a custom widget that displays progress in a circular format.
 * It supports various styling options including center filling, radius filling, and
 * customizable line properties.
 */
public class Astal.CircularProgressBar : Gtk.Widget, Gtk.Buildable {
    private ProgressArc _progress_arc;
    private CenterFill _center_fill;
    private RadiusFill _radius_fill;
    private Gtk.Widget _child;

    private int _line_width;
    private double _percentage;
    private double _start_at;
    private double _end_at;

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
     * The normalized starting position (0.0 to 1.0).
     * 0.0 = 0 degrees (3 o'clock)
     * 0.25 = 90 degrees (12 o'clock)
     * 0.5 = 180 degrees (9 o'clock)
     * 0.75 = 270 degrees (6 o'clock)
     * 1.0 = 360 degrees (3 o'clock)
     */
    public double start_at {
        get { return _start_at; }
        set {
            if (value > 1) {
                _start_at = 1;
            } else if (value < 0) {
                _start_at = 0;
            } else {
                _start_at = value;
            }
        }
    }

    /**
     * The normalized ending position (0.0 to 1.0).
     * Values follow the same pattern as start_at.
     */
    public double end_at {
        get { return _end_at; }
        set {
            if (value > 1) {
                _end_at = 1;
            } else if (value < 0) {
                _end_at = 0;
            } else {
                _end_at = value;
            }
        }
    }

    /**
     * Whether the progress moves clockwise instead of counterclockwise.
     */
    public bool inverted  { get; set; default = true; }

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
        _start_at = 0.0;
        _end_at = 1;

        _progress_arc = new ProgressArc();
        _center_fill = new CenterFill();
        _radius_fill = new RadiusFill();

        _progress_arc.set_parent(this);
        _center_fill.set_parent(this);
        _radius_fill.set_parent(this);

        // Set for the child, and better compatibility with Composite Templates
        layout_manager = new Gtk.BinLayout();
        overflow = Gtk.Overflow.HIDDEN;
        can_focus = true;
        focusable = true;
        can_target = true;

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
        _progress_arc.unparent();
        _progress_arc = null;
        _center_fill.unparent();
        _center_fill = null;
        _radius_fill.unparent();
        _radius_fill = null;
        base.dispose();
    }

    public override void snapshot(Gtk.Snapshot snapshot) {
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

        // Update geometries
        _progress_arc.update_geometry(width / 2.0f, height / 2.0f, delta, actual_line_width, line_cap, start_at, end_at, inverted, percentage);

        if (center_filled) {
            _center_fill.update_geometry(width / 2.0f, height / 2.0f, delta, fill_rule);
        }

        if (radius_filled) {
            _radius_fill.update_geometry(width / 2.0f, height / 2.0f, delta, radius, actual_line_width);
        }

        // Draw in correct order: background to foreground
        if (center_filled) {
            snapshot_child(_center_fill, snapshot);
        }

        if (radius_filled) {
            snapshot_child(_radius_fill, snapshot);
        }

        snapshot_child(_progress_arc, snapshot);

        if (_child != null) {
            snapshot_child(_child, snapshot);
        }
    }
}

/**
 * Private widget that handles drawing the progress arc.
 */
private class ProgressArc : Gtk.Widget {
    private float _center_x;
    private float _center_y;
    private float _delta;
    private float _line_width;
    private Gsk.LineCap _line_cap;
    private double _percentage;
    private bool _updating_geometry = false;
    private double _start_at;
    private double _end_at;
    private bool _inverted;

    public ProgressArc() {
        Object(
            name: "progress",
            css_name: "progress"
        );
    }

    public void update_geometry(
        float center_x,
        float center_y,
        float delta,
        float line_width,
        Gsk.LineCap line_cap,
        double start_at,
        double end_at,
        bool inverted,
        double percentage
    ) {
        if (_updating_geometry) {
            return;
        }
        _updating_geometry = true;

        _center_x = center_x;
        _center_y = center_y;
        _delta = delta;
        _line_width = line_width;
        _line_cap = line_cap;
        _percentage = percentage;
        _start_at = start_at;
        _end_at = end_at;
        _inverted = inverted;

        _updating_geometry = false;
        queue_draw();
    }

    public override void snapshot(Gtk.Snapshot snapshot) {
        if (_percentage <= 0) {
            return;
        }

        var color = get_color();

        var start_angle = _start_at * 2 * Math.PI;
        var end_angle = _end_at * 2 * Math.PI;
        var sweep_angle = end_angle - start_angle;

        var progress_angle = start_angle;
        if (_inverted) {
            progress_angle += (_percentage * sweep_angle);
        } else {
            progress_angle -= (_percentage * sweep_angle);
        }

        var path_builder = new Gsk.PathBuilder();

        // Draw as pie when line_width is 0
        if (_line_width <= 0) {
            path_builder.move_to(_center_x, _center_y);

            if (_percentage >= 1.0) {
                path_builder.add_circle(
                    Graphene.Point().init(_center_x, _center_y),
                    _delta
                );
            } else {
                var start_x = _center_x + (float)(_delta * Math.cos(start_angle));
                var start_y = _center_y + (float)(_delta * Math.sin(start_angle));
                var end_x = _center_x + (float)(_delta * Math.cos(progress_angle));
                var end_y = _center_y + (float)(_delta * Math.sin(progress_angle));

                path_builder.line_to(start_x, start_y);
                bool large_arc = (_percentage * sweep_angle).abs() > Math.PI;
                bool sweep = _inverted;

                path_builder.svg_arc_to(
                    _delta, _delta, 0.0f,
                    large_arc, sweep,
                    end_x, end_y
                );
                path_builder.line_to(_center_x, _center_y);
                path_builder.close();
            }

            snapshot.append_fill(path_builder.to_path(), Gsk.FillRule.EVEN_ODD, color);
        } else {
            // Original stroke drawing code
            if (_percentage >= 1.0) {
                path_builder.add_circle(
                    Graphene.Point().init(_center_x, _center_y),
                    _delta
                );
            } else {
                var start_x = _center_x + (float)(_delta * Math.cos(start_angle));
                var start_y = _center_y + (float)(_delta * Math.sin(start_angle));
                var end_x = _center_x + (float)(_delta * Math.cos(progress_angle));
                var end_y = _center_y + (float)(_delta * Math.sin(progress_angle));

                path_builder.move_to(start_x, start_y);
                // Use same arc parameters for stroke drawing
                bool large_arc = (_percentage * sweep_angle).abs() > Math.PI;
                bool sweep = _inverted;

                path_builder.svg_arc_to(
                    _delta, _delta, 0.0f,
                    large_arc, sweep,
                    end_x, end_y
                );
            }

            var stroke = new Gsk.Stroke(_line_width);
            stroke.set_line_cap(_line_cap);
            snapshot.append_stroke(path_builder.to_path(), stroke, color);
        }
    }
}

/**
 * Private widget that handles drawing the center fill.
 */
private class CenterFill : Gtk.Widget {
    private float _center_x;
    private float _center_y;
    private float _delta;
    private Gsk.FillRule _fill_rule;
    private bool _updating_geometry = false;

    public CenterFill() {
        Object(
            name: "center",
            css_name: "center"
        );
    }

    public void update_geometry(
        float center_x,
        float center_y,
        float delta,
        Gsk.FillRule fill_rule
    ) {
        if (_updating_geometry) {
            return;
        }
        _updating_geometry = true;
        _center_x = center_x;
        _center_y = center_y;
        _delta = delta;
        _fill_rule = fill_rule;
        _updating_geometry = false;
        queue_draw();
    }

    public override void snapshot(Gtk.Snapshot snapshot) {
        var color = get_color();
        var path_builder = new Gsk.PathBuilder();

        path_builder.add_circle(
            Graphene.Point().init(_center_x, _center_y),
            _delta
        );

        snapshot.append_fill(
            path_builder.to_path(),
            _fill_rule,
            color
        );
    }
}

/**
 * Private widget that handles drawing the radius fill.
 */
private class RadiusFill : Gtk.Widget {
    private float _center_x;
    private float _center_y;
    private float _delta;
    private float _radius;
    private float _line_width;
    private bool _updating_geometry = false;

    public RadiusFill() {
        Object(
            name: "radius",
            css_name: "radius"
        );
    }

    public void update_geometry(
        float center_x,
        float center_y,
        float delta,
        float radius,
        float line_width
    ) {
        if (_updating_geometry) {
            return;
        }
        _updating_geometry = true;
        _center_x = center_x;
        _center_y = center_y;
        _delta = delta;
        _radius = radius;
        _line_width = line_width;
        _updating_geometry = false;
        queue_draw();
    }

    public override void snapshot(Gtk.Snapshot snapshot) {
        var color = get_color();
        var path_builder = new Gsk.PathBuilder();

        if (_line_width <= 0) {
            path_builder.add_circle(
                Graphene.Point().init(_center_x, _center_y),
                _delta
            );
            snapshot.append_fill(path_builder.to_path(), Gsk.FillRule.EVEN_ODD, color);
        } else {
            path_builder.add_circle(
                Graphene.Point().init(_center_x, _center_y),
                _delta
            );
            var stroke = new Gsk.Stroke(_line_width);
            snapshot.append_stroke(path_builder.to_path(), stroke, color);
        }
    }
}
