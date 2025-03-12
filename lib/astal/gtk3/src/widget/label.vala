using Pango;

public class Astal.Label : Gtk.Label {
    /**
     * Shortcut for setting [property@Gtk.Label:ellipsize] to [enum@Pango.EllipsizeMode.END]
     */
    public bool truncate {
        set { ellipsize = value ? EllipsizeMode.END : EllipsizeMode.NONE; }
        get { return ellipsize == EllipsizeMode.END; }
    }

    /**
     * Shortcut for setting [property@Gtk.Label:justify] to [enum@Gtk.Justification.FILL]
     */
    public new bool justify_fill {
        set { justify = value ? Gtk.Justification.FILL : Gtk.Justification.LEFT; }
        get { return justify == Gtk.Justification.FILL; }
    }

    construct {
        notify["ellipsize"].connect(() => notify_property("truncate"));
        notify["justify"].connect(() => notify_property("justify_fill"));
    }
}
