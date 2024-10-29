/**
 * Subclass of [class@Gtk.Scale] which adds a signal and property for the drag state.
 */
public class Astal.Slider : Gtk.Scale {
    /**
     * Corresponds to [property@Gtk.Orientable :orientation].
     */
    [CCode (notify = false)]
    public bool vertical {
        get { return orientation == Gtk.Orientation.VERTICAL; }
        set { orientation = value ? Gtk.Orientation.VERTICAL : Gtk.Orientation.HORIZONTAL; }
    }

    /**
     * Emitted when the user drags the slider or uses keyboard arrows and its value changes.
     */
    public signal void dragged();

    construct {
        draw_value = false;

        if (adjustment == null)
            adjustment = new Gtk.Adjustment(0,0,0,0,0,0);

        if (max == 0 && min == 0) {
            max = 1;
        }

        if (step == 0) {
            step = 0.05;
        }

        notify["orientation"].connect(() => {
            notify_property("vertical");
        });

        button_press_event.connect(() => { dragging = true; });
        key_press_event.connect(() => { dragging = true; });
        button_release_event.connect(() => { dragging = false; });
        key_release_event.connect(() => { dragging = false; });
        scroll_event.connect((event) => {
            dragging = true;
            if (event.delta_y > 0)
                value -= step;
            else
                value += step;
            dragging = false;
        });

        value_changed.connect(() => {
            if (dragging)
                dragged();
        });
    }

    /**
     * `true` when the user drags the slider or uses keyboard arrows.
     */
    public bool dragging { get; private set; }

    /**
     * Value of this slider. Defaults to `0`.
     */
    public double value {
        get { return adjustment.value; }
        set { if (!dragging) adjustment.value = value; }
    }

    /**
     * Minimum possible value of this slider. Defaults to `0`.
     */
    public double min {
        get { return adjustment.lower; }
        set { adjustment.lower = value; }
    }

    /**
     * Maximum possible value of this slider. Defaults to `1`.
     */
    public double max {
        get { return adjustment.upper; }
        set { adjustment.upper = value; }
    }

    /**
     * Size of step increments. Defaults to `0.05`.
     */
    public double step {
        get { return adjustment.step_increment; }
        set { adjustment.step_increment = value; }
    }

    // TODO: marks
}
