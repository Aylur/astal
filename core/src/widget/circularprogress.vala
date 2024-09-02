namespace Astal {
public class CircularProgress : Gtk.Bin {
    public double start_at { get; set; }
    public double end_at { get; set; }
    public double value { get; set; }
    public bool inverted { get; set; }
    public bool rounded { get; set; }

    construct {
        notify["start-at"].connect(queue_draw);
        notify["end-at"].connect(queue_draw);
        notify["value"].connect(queue_draw);
        notify["inverted"].connect(queue_draw);
        notify["rounded"].connect(queue_draw);
        notify["child"].connect(queue_draw);
    }

    static construct {
        set_css_name("circular-progress");
    }

    public override void get_preferred_height(out int minh, out int nath) {
        var val = get_style_context().get_property("min-height", Gtk.StateFlags.NORMAL);
        if (val.get_int() <= 0) {
            minh = 40;
            nath = 40;
        }

        minh = val.get_int();
        nath = val.get_int();
    }

    public override void get_preferred_width(out int minw, out int natw) {
        var val = get_style_context().get_property("min-width", Gtk.StateFlags.NORMAL);
        if (val.get_int() <= 0) {
            minw = 40;
            natw = 40;
        }

        minw = val.get_int();
        natw = val.get_int();
    }

    private double to_radian(double percentage) {
        percentage = Math.floor(percentage * 100);
        return (percentage / 100) * (2 * Math.PI);
    }

    private bool is_full_circle(double start, double end, double epsilon = 1e-10) {
        // Ensure that start and end are between 0 and 1
        start = (start % 1 + 1) % 1;
        end = (end % 1 + 1) % 1;

        // Check if the difference between start and end is close to 1
        return Math.fabs(start - end) <= epsilon;
    }

    private double scale_arc_value(double start, double end, double value) {
        // Ensure that start and end are between 0 and 1
        start = (start % 1 + 1) % 1;
        end = (end % 1 + 1) % 1;

        // Calculate the length of the arc
        var arc_length = end - start;
        if (arc_length < 0)
            arc_length += 1; // Adjust for circular representation

        // Calculate the position on the arc based on the percentage value
        var scaled = arc_length + value;

        // Ensure the position is between 0 and 1
        return (scaled % 1 + 1) % 1;
    }

    private double min(double[] arr) {
        double min = arr[0];
        foreach(var i in arr)
            if (min > i) min = i;
        return min;
    }

    private double max(double[] arr) {
        double max = arr[0];
        foreach(var i in arr)
            if (max < i) max = i;
        return max;
    }

    public override bool draw(Cairo.Context cr) {
        Gtk.Allocation allocation;
        get_allocation(out allocation);

        var styles = get_style_context();
        var width = allocation.width;
        var height = allocation.height;
        var thickness = styles.get_property("font-size", Gtk.StateFlags.NORMAL).get_double();
        var margin = styles.get_margin(Gtk.StateFlags.NORMAL);
        var fg = styles.get_color(Gtk.StateFlags.NORMAL);
        var bg = styles.get_background_color(Gtk.StateFlags.NORMAL);

        var bg_stroke = thickness + min({margin.bottom, margin.top, margin.left, margin.right});
        var fg_stroke = thickness;
        var radius = min({width, height}) / 2.0 - max({bg_stroke, fg_stroke}) / 2.0;
        var center_x = width / 2;
        var center_y = height / 2;

        var start_background = to_radian(start_at);
        var end_background = to_radian(end_at);
        var ranged_value = value + start_at;

        var is_circle = is_full_circle(this.start_at, this.end_at);

        if (is_circle) {
            // Redefine end_draw in radius to create an accurate full circle
            end_background = start_background + 2 * Math.PI;
            ranged_value = to_radian(value);
        } else {
            // Range the value for the arc shape
            ranged_value = to_radian(scale_arc_value(
                start_at,
                end_at,
                value
            ));
        }

        double start_progress, end_progress;

        if (inverted) {
            start_progress = end_background - ranged_value;
            end_progress = end_background;
        } else {
            start_progress = start_background;
            end_progress = start_background + ranged_value;
        }

        // Draw background
        cr.set_source_rgba(bg.red, bg.green, bg.blue, bg.alpha);
        cr.arc(center_x, center_y, radius, start_background, end_background);
        cr.set_line_width(bg_stroke);
        cr.stroke();

        // Draw rounded background ends
        if (rounded) {
            var start_x = center_x + Math.cos(start_background) * radius;
            var start_y = center_y + Math.sin(start_background) * radius;
            var end_x = center_x + Math.cos(end_background) * radius;
            var end_y = center_y + Math.sin(end_background) * radius;
            cr.set_line_width(0);
            cr.arc(start_x, start_y, bg_stroke / 2, 0, 0 - 0.01);
            cr.fill();
            cr.arc(end_x, end_y, bg_stroke / 2, 0, 0 - 0.01);
            cr.fill();
        }

        // Draw progress
        cr.set_source_rgba(fg.red, fg.green, fg.blue, fg.alpha);
        cr.arc(center_x, center_y, radius, start_progress, end_progress);
        cr.set_line_width(fg_stroke);
        cr.stroke();

        // Draw rounded progress ends
        if (rounded) {
            var start_x = center_x + Math.cos(start_progress) * radius;
            var start_y = center_y + Math.sin(start_progress) * radius;
            var end_x = center_x + Math.cos(end_progress) * radius;
            var end_y = center_y + Math.sin(end_progress) * radius;
            cr.set_line_width(0);
            cr.arc(start_x, start_y, fg_stroke / 2, 0, 0 - 0.01);
            cr.fill();
            cr.arc(end_x, end_y, fg_stroke / 2, 0, 0 - 0.01);
            cr.fill();
        }

        if (get_child() != null) {
            get_child().size_allocate(allocation);
            propagate_draw(get_child(), cr);
        }

        return true;
    }
}
}
