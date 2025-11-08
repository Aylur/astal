[CCode (cheader_filename = "wayland-client.h", cname = "wl_output_interface")]
private extern Wl.Interface wl_output_interface;

namespace AstalWl {

public class Output : Object {

    public enum Subpixel {
        UNKNOWN = 0,
        NONE = 1,
        HORIZONTAL_RGB = 2,
        HORIZONTAL_BGR = 3,
        VERTICAL_RGB = 4,
        VERTICAL_BGR = 5
    }

    public enum Transform {
        NORMAL = 0,
        ROTATE_90 = 1,
        ROTATE_180 = 2,
        ROTATE_270 = 3,
        FLIPPED = 4,
        FLIPPED_90 = 5,
        FLIPPED_180 = 6,
        FLIPPED_270 = 7,
    }

    private Wl.Output output;

    [GIR(visible = false)]
    public unowned Wl.Output get_wl_output() {
        return this.output;
    }
    
    public uint32 id {get; construct set; }
    public int x { get; private set; }
    public int y { get; private set; }
    public int width { get; private set; }
    public int height { get; private set; }
    public int physical_width { get; private set; }
    public int physical_height { get; private set; }
    public double refresh_rate { get; private set; }
    public Transform transform { get; private set; }
    public Subpixel subpixel { get; private set; }
    public string? make { get; private set; }
    public string? model { get; private set; }
    public double scale { get; private set; }
    public string? name { get; private set; }
    public string? description { get; private set; }

	private void handle_geometry (Wl.Output wl_output, int32 x, int32 y, int32 physical_width, int32 physical_height, int32 subpixel, string make, string model, int32 transform){
	    this.freeze_notify();
	    this.x = x;
	    this.y = y;
	    this.physical_width = physical_width;
	    this.physical_height = physical_height;
	    this.subpixel = subpixel;
	    this.make = make;
	    this.model = model; 
	    this.transform = transform;
	    this.thaw_notify();
	}

	private void handle_mode (Wl.Output wl_output, uint32 flags, int32 width, int32 height, int32 refresh) {
        if((flags&1) == 0) return; 
        this.freeze_notify();
        this.height = height;
        this.width = width;
        this.refresh_rate = refresh / 1000;
	    this.thaw_notify();
	}
	private void handle_done (Wl.Output wl_output) {
	}
	private void handle_scale (Wl.Output wl_output, int32 factor) {
	    this.scale = scale;
	}
	private void handle_name(Wl.Output wl_output, string name) {
	    this.name = name;
	}
	private void handle_description (Wl.Output wl_output, string description) {
	    this.description = description;
	}

    private const Wl.OutputListener output_listener = {
        handle_geometry,
        handle_mode,
        handle_done,
        handle_scale,
        handle_name,
        handle_description
    };

    internal Output(Global global, Wl.Registry registry, Wl.Display display) {
        this.id = global.name;
        this.output = registry.bind<Wl.Output>(global.name, ref wl_output_interface, uint.min(global.version, 4));
        this.output.add_listener(output_listener, this);
        display.roundtrip();
    }

}
}
