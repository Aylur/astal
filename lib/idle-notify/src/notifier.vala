namespace AstalIdleNotify {

public static bool is_supported() {
    return !AstalWl.Registry.get_default().find_globals("ext_idle_notifier_v1").is_empty();
}

public static unowned Notifier get_default() {
    return AstalIdleNotify.Notifier.get_default();
}

public class Notifier : Object {
    private static Notifier? instance;

    public static unowned Notifier get_default() {
        if (instance == null) instance = new Notifier();
        return instance;
    }

    private AstalWl.Registry astal_registry;
    private ExtIdleNotifierV1 notifier;

    public Notification get_idle_notification_for_seat(uint timeout, AstalWl.Seat seat) {
        assert(this.notifier != null);
        assert(seat != null);
        return new Notification(this.notifier.get_idle_notification(timeout, seat.get_wl_seat())); 
    }
    
    public Notification get_idle_notification(uint timeout) {
        return this.get_idle_notification_for_seat(timeout, this.astal_registry.get_seats().nth_data(0));
    }

    public Notification get_input_idle_notification_for_seat(uint timeout, AstalWl.Seat seat) {
        assert(this.notifier != null);
        assert(seat != null);
        assert(this.notifier.get_version() >= 2);
        return new Notification(this.notifier.get_input_idle_notification(timeout, seat.get_wl_seat())); 
    }
    
    public Notification get_input_idle_notification(uint timeout) {
        return this.get_input_idle_notification_for_seat(timeout, this.astal_registry.get_seats().nth_data(0));
    }

    public Notifier() {
        this.astal_registry = AstalWl.Registry.get_default();

        AstalWl.Global? notifier_global = this.astal_registry.find_globals("ext_idle_notifier_v1").nth_data(0);
        if (notifier_global == null) {
            critical("the compositor does not support ext_idle_notifier_v1\n");
            return;
        }

        this.notifier = this.astal_registry.get_registry().bind(notifier_global.name, ref ExtIdleNotifierV1.iface, uint.min(notifier_global.version, 2));

        this.astal_registry.roundtrip();
    }
}
}
