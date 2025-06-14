namespace AstalWl {

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
    private Wl.Registry _registry;
    private List<Global?> globals;

    public Wl.Registry registry { get { return this._registry; } }
    public Wl.Display display { get { return this.source.display; } }
   
    public signal void global_added(Global global);
    public signal void global_removed(Global global);

    private void registry_handle_global_added (Wl.Registry wl_registry, uint32 name, string @interface, uint32 version) {
        Global global = {name, @interface, version};
        this.globals.append(global);
        global_added(global);
    }

    private void registry_handle_global_removed(Wl.Registry wl_registry, uint32 name) {
        foreach (var global in this.globals) {
            if(global.name == name) {
              this.globals.remove(global);
              global_removed(global);
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
        this.source = new Source();
        
        this._registry = this.source.display.get_registry ();
        this._registry.add_listener (registry_listener, this);
        this.source.display.roundtrip();
    }
}
}
