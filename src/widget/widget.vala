namespace Astal {
private class Css {
    private static HashTable<Gtk.Widget, Gtk.CssProvider> _providers;
    public static HashTable<Gtk.Widget, Gtk.CssProvider> providers {
        get {
            if (_providers == null) {
                _providers = new HashTable<Gtk.Widget, Gtk.CssProvider>(
                    (w) => (uint)w,
                    (a, b) => a == b);
            }

            return _providers;
        }
    }
}

private void remove_provider(Gtk.Widget widget) {
    var providers = Css.providers;

    if (providers.contains(widget)) {
        var p = providers.get(widget);
        widget.get_style_context().remove_provider(p);
        providers.remove(widget);
        p.dispose();
    }
}

public void widget_set_css(Gtk.Widget widget, string css) {
    var providers = Css.providers;

    if (providers.contains(widget)) {
        remove_provider(widget);
    } else {
        widget.destroy.connect(() => {
            remove_provider(widget);
        });
    }

    var style = !css.contains("{") || !css.contains("}")
        ? "* { ".concat(css, "}") : css;

    var p = new Gtk.CssProvider();
    widget.get_style_context()
        .add_provider(p, Gtk.STYLE_PROVIDER_PRIORITY_USER);

    try {
        p.load_from_data(style, style.length);
        providers.set(widget, p);
    } catch (Error err) {
        warning(err.message);
    }
}

public string widget_get_css(Gtk.Widget widget) {
    var providers = Css.providers;

    if (providers.contains(widget))
        return providers.get(widget).to_string();

    return "";
}

public void widget_set_class_names(Gtk.Widget widget, string[] class_names) {
    foreach (var name in widget_get_class_names(widget))
        widget_toggle_class_name(widget, name, false);

    foreach (var name in class_names)
        widget_toggle_class_name(widget, name, true);
}

public List<weak string> widget_get_class_names(Gtk.Widget widget) {
    return widget.get_style_context().list_classes();
}

public void widget_toggle_class_name(
    Gtk.Widget widget,
    string class_name,
    bool condition = true
) {
    var c = widget.get_style_context();
    if (condition)
        c.add_class(class_name);
    else
        c.remove_class(class_name);
}

private class Cursor {
    private static HashTable<Gtk.Widget, string> _cursors;
    public static HashTable<Gtk.Widget, string> cursors {
        get {
            if (_cursors == null) {
                _cursors = new HashTable<Gtk.Widget, string>(
                    (w) => (uint)w,
                    (a, b) => a == b);
            }
            return _cursors;
        }
    }
}

private void widget_setup_cursor(Gtk.Widget widget) {
    widget.add_events(Gdk.EventMask.ENTER_NOTIFY_MASK);
    widget.add_events(Gdk.EventMask.LEAVE_NOTIFY_MASK);
    widget.enter_notify_event.connect(() => {
        widget.get_window().set_cursor(
            new Gdk.Cursor.from_name(
                Gdk.Display.get_default(),
                Cursor.cursors.get(widget)));
        return false;
    });
    widget.leave_notify_event.connect(() => {
        widget.get_window().set_cursor(
            new Gdk.Cursor.from_name(
                Gdk.Display.get_default(),
                "default"));
        return false;
    });
    widget.destroy.connect(() => {
        if (Cursor.cursors.contains(widget))
            Cursor.cursors.remove(widget);
    });
}

public void widget_set_cursor(Gtk.Widget widget, string cursor) {
    if (!Cursor.cursors.contains(widget))
        widget_setup_cursor(widget);

    Cursor.cursors.set(widget, cursor);
}

public string widget_get_cursor(Gtk.Widget widget) {
    return Cursor.cursors.get(widget);
}
}
