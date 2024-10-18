public class Astal.CenterBox : Gtk.Box {
    [CCode (notify = false)]
    public bool vertical {
        get { return orientation == Gtk.Orientation.VERTICAL; }
        set { orientation = value ? Gtk.Orientation.VERTICAL : Gtk.Orientation.HORIZONTAL; }
    }

    construct {
        notify["orientation"].connect(() => {
            notify_property("vertical");
        });
    }

    static construct {
        set_css_name("centerbox");
    }

    private Gtk.Widget _start_widget;
    public Gtk.Widget start_widget {
        get { return _start_widget; }
        set {
            if (_start_widget != null)
                remove(_start_widget);

            if (value != null)
                pack_start(value, true, true, 0);
        }
    }

    private Gtk.Widget _end_widget;
    public Gtk.Widget end_widget {
        get { return _end_widget; }
        set {
            if (_end_widget != null)
                remove(_end_widget);

            if (value != null)
                pack_end(value, true, true, 0);
        }
    }

    public Gtk.Widget center_widget {
        get { return get_center_widget(); }
        set {
            if (center_widget != null)
                remove(center_widget);

            if (value != null)
                set_center_widget(value);
        }
    }
}
