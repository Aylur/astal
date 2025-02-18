class Workspaces : Gtk.Box {
    AstalHyprland.Hyprland hypr = AstalHyprland.get_default();
    public Workspaces() {
        Astal.widget_set_class_names(this, {"Workspaces"});
        hypr.notify["workspaces"].connect(sync);
        sync();
    }

    void sync() {
        foreach (var child in get_children())
            child.destroy();

        foreach (var ws in hypr.workspaces) {
            // filter out special workspaces
            if (!(ws.id >= -99 && ws.id <= -2)) {
                add(button(ws));
            }
        }
    }

    Gtk.Button button(AstalHyprland.Workspace ws) {
        var btn = new Gtk.Button() {
            visible = true,
            label = ws.id.to_string()
        };

        hypr.notify["focused-workspace"].connect(() => {
            var focused = hypr.focused_workspace == ws;
            if (focused) {
                Astal.widget_set_class_names(btn, {"focused"});
            } else {
                Astal.widget_set_class_names(btn, {});
            }
        });

        btn.clicked.connect(ws.focus);
        return btn;
    }
}

class FocusedClient : Gtk.Box {
    public FocusedClient() {
        Astal.widget_set_class_names(this, {"Focused"});
        AstalHyprland.get_default().notify["focused-client"].connect(sync);
        sync();
    }

    void sync() {
        foreach (var child in get_children())
            child.destroy();

        var client = AstalHyprland.get_default().focused_client;
        if (client == null)
            return;

        var label = new Gtk.Label(client.title) { visible = true };
        client.bind_property("title", label, "label", BindingFlags.SYNC_CREATE);
        add(label);
    }
}

class Media : Gtk.Box {
    AstalMpris.Mpris mpris = AstalMpris.get_default();

    public Media() {
        Astal.widget_set_class_names(this, {"Media"});
        mpris.notify["players"].connect(sync);
        sync();
    }

    void sync() {
        foreach (var child in get_children())
            child.destroy();

        if (mpris.players.length() == 0) {
            add(new Gtk.Label("Nothing Playing"));
            return;
        }

        var player = mpris.players.nth_data(0);
        var label = new Gtk.Label(null);
        var cover = new Gtk.Box(Gtk.Orientation.HORIZONTAL, 0) {
            valign = Gtk.Align.CENTER
        };

        Astal.widget_set_class_names(cover, {"Cover"});
        player.bind_property("metadata", label, "label", BindingFlags.SYNC_CREATE, (_, src, ref trgt) => {
            var title = player.title;
            var artist = player.artist;
            trgt.set_string(@"$artist - $title");
            return true;
        });

        var id = player.notify["cover-art"].connect(() => {
            var art = player.cover_art;
            Astal.widget_set_css(cover, @"background-image: url('$art')");
        });

        cover.destroy.connect(() => player.disconnect(id));
        add(cover);
        add(label);
    }
}

class SysTray : Gtk.Box {
    HashTable<string, Gtk.Widget> items = new HashTable<string, Gtk.Widget>(str_hash, str_equal);
    AstalTray.Tray tray = AstalTray.get_default();

    public SysTray() {
        Astal.widget_set_class_names(this, { "SysTray" });
        tray.item_added.connect(add_item);
        tray.item_removed.connect(remove_item);
    }

    void add_item(string id) {
        if (items.contains(id))
            return;

        var item = tray.get_item(id);
        var btn = new Gtk.MenuButton() { use_popover = false, visible = true };
        var icon = new Astal.Icon() { visible = true };

        item.bind_property("tooltip-markup", btn, "tooltip-markup", BindingFlags.SYNC_CREATE);
        item.bind_property("gicon", icon, "gicon", BindingFlags.SYNC_CREATE);
        item.bind_property("menu-model", btn, "menu-model", BindingFlags.SYNC_CREATE);
        btn.insert_action_group("dbusmenu", item.action_group);
        item.notify["action-group"].connect(() => {
            btn.insert_action_group("dbusmenu", item.action_group);
        });

        btn.add(icon);
        add(btn);
        items.set(id, btn);
    }

    void remove_item(string id) {
        if (items.contains(id)) {
            items.remove(id);
        }
    }
}

class Wifi : Astal.Icon {
    public Wifi() {
        Astal.widget_set_class_names(this, {"Wifi"});
        var wifi = AstalNetwork.get_default().get_wifi();
        if (wifi != null) {
            wifi.bind_property("ssid", this, "tooltip-text", BindingFlags.SYNC_CREATE);
            wifi.bind_property("icon-name", this, "icon", BindingFlags.SYNC_CREATE);
        }
    }
}

class AudioSlider : Gtk.Box {
    Astal.Icon icon = new Astal.Icon();
    Astal.Slider slider = new Astal.Slider() { hexpand = true };

    public AudioSlider() {
        add(icon);
        add(slider);
        Astal.widget_set_class_names(this, {"AudioSlider"});
        Astal.widget_set_css(this, "min-width: 140px");

        var speaker = AstalWp.get_default().audio.default_speaker;
        speaker.bind_property("volume-icon", icon, "icon", BindingFlags.SYNC_CREATE);
        speaker.bind_property("volume", slider, "value", BindingFlags.SYNC_CREATE);
        slider.dragged.connect(() => speaker.volume = slider.value);
    }
}

class Battery : Gtk.Box {
    Astal.Icon icon = new Astal.Icon();
    Astal.Label label = new Astal.Label();

    public Battery() {
        add(icon);
        add(label);
        Astal.widget_set_class_names(this, {"Battery"});

        var bat = AstalBattery.get_default();
        bat.bind_property("is-present", this, "visible", BindingFlags.SYNC_CREATE);
        bat.bind_property("battery-icon-name", icon, "icon", BindingFlags.SYNC_CREATE);
        bat.bind_property("percentage", label, "label", BindingFlags.SYNC_CREATE, (_, src, ref trgt) => {
            var p = Math.floor(src.get_double() * 100);
            trgt.set_string(@"$p%");
            return true;
        });
    }
}

class Time : Astal.Label {
    string format;
    AstalIO.Time interval;

    void sync() {
        label = new DateTime.now_local().format(format);
    }

    public Time(string format = "%H:%M - %A %e.") {
        this.format = format;
        interval = AstalIO.Time.interval(1000, null);
        interval.now.connect(sync);
        destroy.connect(interval.cancel);
        Astal.widget_set_class_names(this, {"Time"});
    }
}

class Left : Gtk.Box {
    public Left() {
        Object(hexpand: true, halign: Gtk.Align.START);
        add(new Workspaces());
        add(new FocusedClient());
    }
}

class Center : Gtk.Box {
    public Center() {
        add(new Media());
    }
}

class Right : Gtk.Box {
    public Right() {
        Object(hexpand: true, halign: Gtk.Align.END);
        add(new SysTray());
        add(new Wifi());
        add(new AudioSlider());
        add(new Battery());
        add(new Time());
    }
}

class Bar : Astal.Window {
    public Bar(Gdk.Monitor monitor) {
        Object(
            anchor: Astal.WindowAnchor.TOP
                | Astal.WindowAnchor.LEFT
                | Astal.WindowAnchor.RIGHT,
            exclusivity: Astal.Exclusivity.EXCLUSIVE,
            gdkmonitor: monitor
        );

        Astal.widget_set_class_names(this, {"Bar"});

        add(new Astal.CenterBox() {
            start_widget = new Left(),
            center_widget = new Center(),
            end_widget = new Right(),
        });

        show_all();
    }
}
