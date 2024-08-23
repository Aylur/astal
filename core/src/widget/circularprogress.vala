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

    private double _to_radian(double percentage) {
        percentage = Math.floor(percentage * 100);
        return (percentage / 100) * (2 * Math.PI);
    }

    private bool _is_full_circle(double start, double end, double epsilon = 1e-10) {
        // Ensure that start and end are between 0 and 1
        start = (start % 1 + 1) % 1;
        end = (end % 1 + 1) % 1;

        // Check if the difference between start and end is close to 1
        return Math.fabs(start - end) <= epsilon;
    }

    private double _map_arc_value_to_range(double start, double end, double value) {
        // Ensure that start and end are between 0 and 1
        start = (start % 1 + 1) % 1;
        end = (end % 1 + 1) % 1;

        // Calculate the length of the arc
        var arcLength = end - start;
        if (arcLength < 0)
            arcLength += 1; // Adjust for circular representation

        // Calculate the position on the arc based on the percentage value
        var position = start + (arcLength * value);

        // Ensure the position is between 0 and 1
        position = (position % 1 + 1) % 1;

        return position;
    }

    private double _min(double[] arr) {
        double min = arr[0];
        foreach(var i in arr)
            if (min > i) min = i;
        return min;
    }

    private double _max(double[] arr) {
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

        var bg_stroke = thickness + _min({margin.bottom, margin.top, margin.left, margin.right});
        var fg_stroke = thickness;
        var radius = _min({width, height}) / 2.0 - _max({bg_stroke, fg_stroke}) / 2.0;
        var center_x = width / 2;
        var center_y = height / 2;

        var start_background = _to_radian(this.start_at);
        var end_background = _to_radian(this.end_at);
        var ranged_value = this.value + this.start_at;

        var is_circle = _is_full_circle(this.start_at, this.end_at);

        if (is_circle) {
            // Redefine endDraw in radius to create an accurate full circle
            end_background = start_background + 2 * Math.PI;
        } else {
            // Range the value for the arc shape
            ranged_value = _map_arc_value_to_range(
                this.start_at,
                this.end_at,
                this.value
            );
        }

        var to = _to_radian(ranged_value);
        double start_progress, end_progress;

        if (this.inverted) {
            start_progress = (2 * Math.PI - to) - start_background;
            end_progress = (2 * Math.PI - start_background) - start_background;
        } else {
            start_progress = start_background;
            end_progress = to;
        }

        // Draw background
        cr.set_source_rgba(bg.red, bg.green, bg.blue, bg.alpha);
        cr.arc(center_x, center_y, radius, start_background, end_background);

        cr.set_line_width(bg_stroke);
        cr.stroke();

        // Draw progress
        cr.set_source_rgba(fg.red, fg.green, fg.blue, fg.alpha);
        cr.arc(center_x, center_y, radius, start_progress, end_progress);
        cr.set_line_width(fg_stroke);
        cr.stroke();

        // Draw rounded ends
        if (this.rounded) {
            var start_x = center_x + Math.cos(start_background);
            var start_y = center_y + Math.cos(start_background);
            var end_x = center_x + Math.cos(to) * radius;
            var end_y = center_y + Math.cos(to) * radius;
            cr.set_line_width(0);
            cr.arc(start_x, start_y, fg_stroke / 2, 0, 0 - 0.01);
            cr.fill();
            cr.arc(end_x, end_y, fg_stroke / 2, 0, 0 - 0.01);
            cr.fill();
        }

        if (this.get_child() != null) {
            this.get_child().size_allocate(allocation);
            this.propagate_draw(this.get_child(), cr);
        }

        return true;
    }
}
}
