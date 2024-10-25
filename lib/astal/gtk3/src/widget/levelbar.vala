public class Astal.LevelBar : Gtk.LevelBar {
    /**
     * Corresponds to [property@Gtk.Orientable :orientation].
     */
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
}
