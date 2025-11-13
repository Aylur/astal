[CCode(cheader_filename = "wayland-client.h", cname = "wl_seat_interface")]
private extern Wl.Interface wl_seat_interface;

namespace AstalWl {
public class Seat : Object {
    [Flags]
    public enum Capabilities {
        POINTER = 1,
        KEYBOARD = 2,
        TOUCH = 4
    }

    private Wl.Seat seat;

    [GIR(visible = false)]
    public unowned Wl.Seat get_wl_seat() {
        return this.seat;
    }

    public uint32 id { get; construct; }
    public string? name { get; private set; }
    public Capabilities capabilities { get; private set; }

    private void handle_capabilities (Wl.Seat wl_seat, uint32 capabilities) {
        this.capabilities = capabilities;
    }
    private void handle_name (Wl.Seat wl_seat, string name) {
        this.name = name;
    }

    private const Wl.SeatListener seat_listener = {
        handle_capabilities,
        handle_name,
    };

    internal Seat(Global global, Wl.Registry registry, Wl.Display display) {
        Object(id: global.name);
        this.seat = registry.bind<Wl.Seat>(global.name, ref wl_seat_interface, uint.min(global.version, 10));
        this.seat.add_listener(seat_listener, this);
        display.roundtrip();
    }
}
}