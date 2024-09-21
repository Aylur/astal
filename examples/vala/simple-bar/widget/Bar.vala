class Battery : Gtk.Box {
    Astal.Icon icon = new Astal.Icon();
    Astal.Label label = new Astal.Label();

    public Battery() {
        add(icon);
        add(label);

        var bat = AstalBattery.get_default();
        bat.bind_property("battery-icon-name", icon, "icon", BindingFlags.SYNC_CREATE);
        bat.bind_property("percentage", label, "label", BindingFlags.SYNC_CREATE, (_, src, ref trgt) => {
            var p = Math.floor(src.get_double() * 100);
            trgt.set_string(@"$p%");
            return true;
        });
    }
}

class Clock : Astal.Label {
    string format;
    Astal.Time interval;

    void sync() {
        label = new DateTime.now_local().format(format);
    }

    public Clock(string format = "%H:%M - %A %e.") {
        this.format = format;
        interval = Astal.Time.interval(1000, null);
        interval.now.connect(sync);
        destroy.connect(interval.cancel);
    }
}

class Left : Gtk.Box {
    public Left() {
        add(new Clock());
        add(new Battery());
    }
}

class Bar : Astal.Window {
    public Bar() {
        add(new Astal.CenterBox() {
            start_widget = new Left()
        });

        show_all();
    }
}
