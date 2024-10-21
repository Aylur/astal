/**
 * Subclass of [class@Gtk.ScrolledWindow] which has its policy default to
 * [enum@Gtk.PolicyType.AUTOMATIC].
 *
 * Its css selector is `scrollable`.
 * Its child getter returns the child of the inner
 * [class@Gtk.Viewport], instead of the viewport.
 */
public class Astal.Scrollable : Gtk.ScrolledWindow {
    private Gtk.PolicyType _hscroll = Gtk.PolicyType.AUTOMATIC;
    private Gtk.PolicyType _vscroll = Gtk.PolicyType.AUTOMATIC;

    public Gtk.PolicyType hscroll {
        get { return _hscroll; }
        set {
            _hscroll = value;
            set_policy(value, vscroll);
        }
    }

    public Gtk.PolicyType vscroll {
        get { return _vscroll; }
        set {
            _vscroll = value;
            set_policy(hscroll, value);
        }
    }

    static construct {
        set_css_name("scrollable");
    }

    construct {
        if (hadjustment != null)
            hadjustment = new Gtk.Adjustment(0,0,0,0,0,0);

        if (vadjustment != null)
            vadjustment = new Gtk.Adjustment(0,0,0,0,0,0);
    }

    public new Gtk.Widget get_child() {
        var ch = base.get_child();
        if (ch is Gtk.Viewport) {
            return ch.get_child();
        }
        return ch;
    }
}
