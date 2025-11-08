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
    
    public List<Global?> globals;
    public unowned List<Global?> get_globals() {
        return this.globals;
    }
    public signal void global_added(Global global);
    public signal void global_removed(Global global);

    public List<Output?> outputs;
    public unowned List<Output?> get_outputs() {
        return this.outputs;
    }
    public signal void output_added(Output output);
    public signal void output_removed(Output output);

    [GIR(visible = false)]
    public unowned Wl.Registry? get_registry() {
        return this.registry;
    }

    [GIR(visible = false)]
    public unowned Wl.Display? get_display() {
        return this.display;
    }

    private void registry_handle_global_added (Wl.Registry wl_registry, uint32 name, string @interface, uint32 version) {
        Global global = {name, @interface, version};
        this.globals.append(global);

        if(@interface == "wl_output") {
            var output = new Output(global, this.registry, this.display);
            this.outputs.append(output);
            output_added(output);
        }

        global_added(global);
    }

    private void registry_handle_global_removed(Wl.Registry wl_registry, uint32 name) {
        foreach (var global in this.globals) {
            if(global.name == name) {
                this.globals.remove(global);
                if(global.interface == "wl_output") {
                    this.outputs.foreach((output) => {
                        if(output.id == name) {
                            this.outputs.remove(output);
                            output_removed(output);
                            return;
                        }
                    });
                }
                global_removed(global);
                break;
            }
        }
    }

    public List<Global?>? find_globals(string? interface) {
        List<Global?> list = new List<Global?>();
        
        foreach (var global in this.globals) {
            if(interface == null || global.interface == interface) list.append(global);
        }

        return list;
    }

    private const Wl.RegistryListener registry_listener = {
        registry_handle_global_added,
        registry_handle_global_removed,
    };

    construct {
        this.globals = new List<Global?>();
        this.outputs = new List<Output?>();

        this.display = get_wl_display();

        if(this.display == null) {
            debug("Could not find Gdk Wayland Display, falling back to creating a new Wayland Display");
            this.source = new Source();
            this.display = this.source.display;
        }

        if(this.display == null) {
            critical("Could not connect to wayland.");
            return;
        }
        
        this.registry = this.display.get_registry ();
        this.registry.add_listener (registry_listener, this);
        this.display.roundtrip();
    }
}
}
