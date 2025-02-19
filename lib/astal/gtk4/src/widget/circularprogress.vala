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

    private float _cached_radius = -1;
    private float _cached_delta = -1;
    private int _cached_width = -1;
    private int _cached_height = -1;

    private bool _center_filled;
    private bool _radius_filled;
    private Gsk.LineCap _line_cap;
    private Gsk.FillRule _fill_rule;

    /**
     * Whether the center of the circle is filled.
     */
    public bool center_filled {
        get { return _center_filled; }
        set {
            if (_center_filled != value) {
                _center_filled = value;
                queue_draw();
            }
        }
    }

    /**
     * Whether the radius area is filled.
     */
    public bool radius_filled {
        get { return _radius_filled; }
        set {
            if (_radius_filled != value) {
                _radius_filled = value;
                queue_draw();
            }
        }
    }

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
            invalidate_dimensions();
            queue_draw();
        }
    }

    /**
     * The line cap style for the progress stroke.
     *
     * Check [[https://docs.gtk.org/gsk4/enum.LineCap.html]] for more information.
     */
    public Gsk.LineCap line_cap {
        get { return _line_cap; }
        set {
            if (_line_cap != value) {
                _line_cap = value;
                queue_draw();
            }
        }
    }

    /**
     * The fill rule for the center fill area.
     *
     * Check [[https://docs.gtk.org/gsk4/enum.FillRule.html]] for more information.
     */
    public Gsk.FillRule fill_rule {
        get { return _fill_rule; }
        set {
            if (_fill_rule != value) {
                _fill_rule = value;
                queue_draw();
            }
        }
    }

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
                queue_draw();
            }
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

            queue_draw();
        }
    }

    /*
     * Implements Gtk.Buildable interface.
     */
    public void add_child(Gtk.Builder builder, GLib.Object child, string? type) {
        if (child is Gtk.Widget) {
            this.child = (Gtk.Widget)child;
        } else if (child is Gtk.EventController) {
            this.add_controller((Gtk.EventController)child);
        }
    }

    static construct {
        set_css_name("circular-progress");
        Gtk.CssProvider css_provider = new Gtk.CssProvider();
        css_provider.load_from_string(
            """
			circular-progress progress-arc {
				color: @accent_color;
			}
			circular-progress radius-fill {
				color: @headerbar_backdrop_color;
			}
			circular-progress center-fill {
				color: @headerbar_shade_color;
			}
			"""
        );

        // `add_provider_for_display` is not deprecated even though vala states that it is
        Gtk.StyleContext.add_provider_for_display(
            Gdk.Display.get_default(),
            css_provider,
            Gtk.STYLE_PROVIDER_PRIORITY_APPLICATION
        );
    }

    construct {
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

        // Connect to child's notify signal to redraw when child changes
        notify["child"].connect(() => {
            if (_child != null) {
                _child.notify.connect(() => queue_draw());
            }
        });
    }

    public CircularProgressBar() {
        Object(
            name : "circular-progress"
        );
        notify.connect(() => {
            queue_draw();
        });
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

        // Only recalculate if dimensions changed
        if (width != _cached_width || height != _cached_height) {
            _cached_width = width;
            _cached_height = height;
            _cached_radius = float.min(width / 2.0f, height / 2.0f) - 1;

            // Adjust delta based on line width to prevent overflow
            float half_line_width = (float)line_width / 2.0f;
            _cached_delta = _cached_radius - half_line_width;

            if (_cached_delta < 0) {
                _cached_delta = 0;
            }
        }

        // Use cached values and ensure line width doesn't exceed available space
        var actual_line_width = (float)line_width;
        if (actual_line_width > _cached_radius * 2) {
            actual_line_width = _cached_radius * 2;
        }

        // Update geometries
        _progress_arc.update_geometry(_cached_width / 2.0f, _cached_height / 2.0f, _cached_delta, actual_line_width, line_cap, percentage);

        if (center_filled) {
            _center_fill.update_geometry(_cached_width / 2.0f, _cached_height / 2.0f, _cached_delta, fill_rule);
        }

        if (radius_filled) {
            _radius_fill.update_geometry(_cached_width / 2.0f, _cached_height / 2.0f, _cached_delta, _cached_radius, actual_line_width);
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
            _child.snapshot(snapshot);
        }
    }

    private void invalidate_dimensions() {
        _cached_width = -1;
        _cached_height = -1;
        _cached_radius = -1;
        _cached_delta = -1;
    }

    public override void measure(Gtk.Orientation orientation,
                                 int for_size,
                                 out int minimum,
                                 out int natural,
                                 out int minimum_baseline,
                                 out int natural_baseline) {
        minimum = natural = 24;
        minimum_baseline = natural_baseline = -1;

        if (_child != null) {
            int child_min, child_nat;
            _child.measure(
                orientation,
                for_size,
                out child_min,
                out child_nat,
                null,
                null
            );
            minimum = natural = int.max(24, child_min);
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

    public ProgressArc() {
        Object(
            name: "progress-arc",
            css_name: "progress-arc",
            layout_manager: new Gtk.BinLayout()
        );
    }

    public void update_geometry(float center_x,
                                float center_y,
                                float delta,
                                float line_width,
                                Gsk.LineCap line_cap,
                                double percentage) {
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

        _updating_geometry = false;
        queue_draw();
    }

    public override void snapshot(Gtk.Snapshot snapshot) {
        if (_percentage <= 0) {
            return;
        }

        var color = get_color();
        var start_angle = 1.5f * Math.PI;
        var end_angle = start_angle + (_percentage * 2 * Math.PI);
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
                var end_x = _center_x + (float)(_delta * Math.cos(end_angle));
                var end_y = _center_y + (float)(_delta * Math.sin(end_angle));

                path_builder.line_to(start_x, start_y);
                path_builder.svg_arc_to(
                    _delta, _delta, 0.0f,
                    _percentage > 0.5, true,
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
                var end_x = _center_x + (float)(_delta * Math.cos(end_angle));
                var end_y = _center_y + (float)(_delta * Math.sin(end_angle));

                path_builder.move_to(start_x, start_y);
                path_builder.svg_arc_to(
                    _delta, _delta, 0.0f,
                    _percentage > 0.5, true,
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
            name: "center-fill",
            css_name: "center-fill",
            layout_manager: new Gtk.BinLayout()
        );
    }

    public void update_geometry(float center_x,
                                float center_y,
                                float delta,
                                Gsk.FillRule fill_rule) {
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
            name: "radius-fill",
            css_name: "radius-fill",
            layout_manager: new Gtk.BinLayout()
        );
    }

    public void update_geometry(float center_x, float center_y, float delta, float radius, float line_width) {
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
