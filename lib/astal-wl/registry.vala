namespace AstalWl {
extern unowned Wl.Display get_wl_display();

public struct Global {
    uint32 name;
    string interface;
    uint32 version;
}

public Registry get_default() {
    return Registry.get_default();
}

public class Registry : Object {
    private static Registry? instance;

    public static unowned Registry get_default() {
        if (instance == null) instance = new Registry();
        return instance;
    }

    private Source source;
    private Wl.Registry registry;
    private unowned Wl.Display display;
    private ZxdgOutputManagerV1 output_manager;

    public HashTable<uint32, Global?> globals;
    public List<weak Global?> get_globals() {
        return this.globals.get_values();
    }
    public signal void global_added(Global global);
    public signal void global_removed(Global global);

    public HashTable<uint32, Output> outputs;
    public List<weak Output> get_outputs() {
        return this.outputs.get_values();
    }
    public signal void output_added(Output output);
    public signal void output_removed(Output output);

    public HashTable<uint32, Seat> seats;
    public List<weak Seat> get_seats() {
        return this.seats.get_values();
    }
    public signal void seat_added(Seat seat);
    public signal void seat_removed(Seat seat);

    [GIR(visible = false)]
    public unowned Wl.Registry? get_registry() {
        return this.registry;
    }

    [GIR(visible = false)]
    public unowned Wl.Display? get_display() {
        return this.display;
    }

    private void registry_handle_global_added (Wl.Registry wl_registry, uint32 name, string @interface, uint32 version) {
        Global global = { name, @interface, version };
        this.globals.insert(name, global);

        if (@interface == "zxdg_output_manager_v1") {
            this.output_manager = this.registry.bind<ZxdgOutputManagerV1>(name, ref ZxdgOutputManagerV1.iface, uint.min(version, 4));
            HashTableIter<uint32, Output> iter = HashTableIter<uint32, Output>(this.outputs);
            unowned Output output;
            uint32 id;

            while (iter.next(out id, out output)) {
                output.init_xdg(this.output_manager, this.display);
            }
        } else if (@interface == "wl_output") {
            var output = new Output(global, this.registry, this.display, this.output_manager);
            this.outputs.insert(name, output);
            output_added(output);
        } else if (@interface == "wl_seat") {
            var seat = new Seat(global, this.registry, this.display);
            this.seats.insert(name, seat);
            seat_added(seat);
        }
        global_added(global);
    }

    private void registry_handle_global_removed(Wl.Registry wl_registry, uint32 name) {
        var global = this.globals.lookup(name);
        this.globals.remove(name);
        if (global.interface == "wl_output") {
            var output = this.outputs.lookup(name);
            this.outputs.remove(name);
            output_removed(output);
        } else if (global.interface == "wl_seat") {
            var seat = this.seats.lookup(name);
            this.seats.remove(name);
            seat_removed(seat);
        }
        global_removed(global);
    }

    public List<Global?>? find_globals(string? @interface = null) {
        List<Global?> list = new List<Global?>();

        HashTableIter<uint32, Global?> iter = HashTableIter<uint32, Global?>(this.globals);
        unowned Global? global;
        uint32 id;

        while (iter.next(out id, out global)) {
            if ((@interface == null) || (global.interface == @interface)) list.append(global);
        }
        return list;
    }

    public Global? get_global_by_id(uint32 id) {
        return this.globals.lookup(id);
    }

    public Output? get_output_by_id(uint32 id) {
        return this.outputs.lookup(id);
    }

    public Output? get_output_by_name(string name) {
        HashTableIter<uint32, Output> iter = HashTableIter<uint32, Output>(this.outputs);
        unowned Output output;
        uint32 id;

        while (iter.next(out id, out output)) {
            if (output.name == name) return output;
        }
        return null;
    }

    [GIR(visible = false)]
    public Output? get_output_by_wl_output(Wl.Output wl_output) {
        HashTableIter<uint32, Output> iter = HashTableIter<uint32, Output>(this.outputs);
        unowned Output output;
        uint32 id;

        while (iter.next(out id, out output)) {
            if (output.get_wl_output() == wl_output) return output;
        }
        return null;
    }

    private const Wl.RegistryListener registry_listener = {
        registry_handle_global_added,
        registry_handle_global_removed,
    };

    public Seat? get_seat_by_id(uint32 id) {
        return this.seats.lookup(id);
    }

    public Seat? get_seat_by_name(string name) {
        HashTableIter<uint32, Seat> iter = HashTableIter<uint32, Seat?>(this.seats);
        unowned Seat seat;
        uint32 id;

        while (iter.next(out id, out seat)) {
            if (seat.name == name) return seat;
        }
        return null;
    }

    [GIR(visible = false)]
    public Seat? get_seat_by_wl_seat(Wl.Seat wl_seat) {
        HashTableIter<uint32, Seat> iter = HashTableIter<uint32, Seat>(this.seats);
        unowned Seat seat;
        uint32 id;

        while (iter.next(out id, out seat)) {
            if (seat.get_wl_seat() == wl_seat) return seat;
        }
        return null;
    }

    construct {
        this.globals = new HashTable<uint32, Global?>(direct_hash, direct_equal);
        this.outputs = new HashTable<uint32, Output>(direct_hash, direct_equal);
        this.seats = new HashTable<uint32, Seat>(direct_hash, direct_equal);

        this.display = get_wl_display();

        if (this.display == null) {
            debug("Could not find Gdk Wayland Display, falling back to creating a new Wayland Display");
            this.source = new Source();
            this.display = this.source.display;
        }

        if (this.display == null) {
            critical("Could not connect to wayland.");
            return;
        }

        this.registry = this.display.get_registry();
        this.registry.add_listener(registry_listener, this);
        this.display.roundtrip();
    }
}
}
