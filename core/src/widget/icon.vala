namespace Astal {
public Gtk.IconInfo? lookup_icon(string icon) {
    var theme = Gtk.IconTheme.get_default();
    return theme.lookup_icon(icon, 16, Gtk.IconLookupFlags.USE_BUILTIN);
}

public class Icon : Gtk.Image {
    private IconType type = IconType.NAMED;
    private double size { get; set; default = 14; }

    public new Gdk.Pixbuf pixbuf { get; set; }
    public string icon { get; set; default = ""; }
    public GLib.Icon g_icon {get; set;}

    private async void display_icon() {
        switch(type) {
        case IconType.NAMED:
            icon_name = icon;
            pixel_size = (int)size;
            break;
        case IconType.FILE:
            try {
                var file = File.new_for_path(icon);
                var stream = yield file.read_async();
                var pb = yield new Gdk.Pixbuf.from_stream_at_scale_async(
                    stream,
                    (int)size * scale_factor,
                    (int)size * scale_factor,
                    true,
                    null
                );
                var cs = Gdk.cairo_surface_create_from_pixbuf(pb, 0, this.get_window());
                set_from_surface(cs);
            } catch (Error err) {
                printerr(err.message);
            }
            break;
        case IconType.PIXBUF:
            var pb_scaled = pixbuf.scale_simple(
                (int)size * scale_factor,
                (int)size * scale_factor,
                Gdk.InterpType.BILINEAR
            );
            if (pb_scaled != null) {
                var cs = Gdk.cairo_surface_create_from_pixbuf(pb_scaled, 0, this.get_window());
                set_from_surface(cs);
            }
            break;
        case IconType.GICON:
            pixel_size = (int)size;
            gicon = g_icon;
            break;

        }
    }

    static construct {
        set_css_name("icon");
    }

    construct {
        notify["icon"].connect(() => {
            if(FileUtils.test(icon, GLib.FileTest.EXISTS))
                type = IconType.FILE;
            else if (lookup_icon(icon) != null)
                type = IconType.NAMED;
            else {
                type = IconType.NAMED;
                warning("cannot assign %s as icon, "+
                    "it is not a file nor a named icon", icon);
            }
            display_icon.begin();
        });

        notify["pixbuf"].connect(() => {
            type = IconType.PIXBUF;
            display_icon.begin();
        });

        notify["g-icon"].connect(() => {
            type = IconType.GICON;
            display_icon.begin();
        });
        
        size_allocate.connect(() => {
            size = get_style_context()
                .get_property("font-size", Gtk.StateFlags.NORMAL).get_double();

            display_icon.begin();
        });

        get_style_context().changed.connect(() => {
            size = get_style_context()
                .get_property("font-size", Gtk.StateFlags.NORMAL).get_double();

            display_icon.begin();
        });
    }
}

private enum IconType {
    NAMED,
    FILE,
    PIXBUF,
    GICON,
}
}
