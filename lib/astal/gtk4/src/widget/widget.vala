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
        p.load_from_string(style);
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

public string[] widget_get_class_names(Gtk.Widget widget) {
    return widget.get_css_classes();
}

public void widget_toggle_class_name(
    Gtk.Widget widget,
    string class_name,
    bool condition = true
) {
    if (condition)
        widget.add_css_class(class_name);
    else
        widget.remove_css_class(class_name);
}

public void widget_set_cursor(Gtk.Widget widget, string cursor) {
    widget.set_cursor_from_name(cursor);
}

public string widget_get_cursor(Gtk.Widget widget) {
    return widget.cursor.name;
}

private class ClickThrough {
    private static HashTable<Gtk.Widget, bool> _click_through;
    public static HashTable<Gtk.Widget, bool> click_through {
        get {
            if (_click_through == null) {
                _click_through = new HashTable<Gtk.Widget, bool>(
                    (w) => (uint)w,
                    (a, b) => a == b);
            }
            return _click_through;
        }
    }
}

public void widget_set_click_through(Gtk.Widget widget, bool click_through) {
    ClickThrough.click_through.set(widget, click_through);
}

public bool widget_get_click_through(Gtk.Widget widget) {
    return ClickThrough.click_through.get(widget);
}
}
