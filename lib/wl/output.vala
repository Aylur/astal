[CCode(cheader_filename = "wayland-client.h", cname = "wl_output_interface")]
private extern Wl.Interface wl_output_interface;

namespace AstalWl {
/**
 * Represents a display output device and tracks properties associated with it.
 *
 * This class listens to Wayland `wl_output` and optional
 * `zxdg_output_v1` events if supported by the compositor
 * to maintain accurate state information.
 */
public class Output : Object {
    /**
     * This enumeration describes how the physical pixels on an output are laid out.
     */
    public enum Subpixel {
        UNKNOWN = 0,
        NONE = 1,
        HORIZONTAL_RGB = 2,
        HORIZONTAL_BGR = 3,
        VERTICAL_RGB = 4,
        VERTICAL_BGR = 5
    }

    /**
     * This describes transformations that clients and compositors apply to buffer contents.
     * The flipped values correspond to an initial flip around a vertical axis followed by rotation.
     */
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
    private ZxdgOutputV1? xdg_output;

    /**
     * Returns the underlying `wl_output` proxy pointer.
     */
    [GIR(visible = false)]
    public unowned Wl.Output get_wl_output() {
        return this.output;
    }
    /**
     * The unique registry ID of this output as provided by the compositor.
     */
    public uint32 id { get; construct; }
    /**
     * The logical geometry of the output in compositor coordinates.
     * This reflects the visible area after applying scaling and transform.
     */
    public Rectangle? geometry { get; private set; }
    private Rectangle? output_geometry { get; private set; }
    /**
     * The physical width of the output in millimeters.
     */
    public int physical_width { get; private set; }
    /**
     * The physical height of the output in millimeters.
     */
    public int physical_height { get; private set; }
    /**
     * The refresh rate of the current output mode in Hz.
     */
    public double refresh_rate { get; private set; }
    /**
     * The rotation or flip transform of the output surface.
     */
    public Transform transform { get; private set; }
    /**
     * The subpixel layout of the physical monitor.
     */
    public Subpixel subpixel { get; private set; }
    /**
     * The manufacturer name of the display device.
     */
    public string? make { get; private set; }
    /**
     * The product or model name of the display device.
     */
    public string? model { get; private set; }
    /**
     * The scaling factor of the output.
     */
    public double scale { get; private set; }
    /**
     * The compositor-assigned name of this output.
     * Usually corresponds to an identifier like "HDMI-A-1".
     */
    public string? name { get; private set; }
    /**
     * A description of the output.
     */
    public string? description { get; private set; }

    private void handle_geometry (Wl.Output wl_output, int32 x, int32 y, int32 physical_width, int32 physical_height, int32 subpixel, string make, string model, int32 transform) {
        this.freeze_notify();
        this.output_geometry.x = x;
        this.output_geometry.y = y;
        this.subpixel = subpixel;
        this.make = make;
        this.model = model;
        this.transform = transform;
        switch (this.transform) {
            case ROTATE_90:
            case ROTATE_270:
            case FLIPPED_90:
            case FLIPPED_270:
                this.physical_width = physical_height;
                this.physical_height = physical_width;
                break;
            default:
                this.physical_width = physical_width;
                this.physical_height = physical_height;
                break;
        }
        this.thaw_notify();
    }

    private void handle_mode (Wl.Output wl_output, uint32 flags, int32 width, int32 height, int32 refresh) {
        if ((flags & 1) == 0) return;
        this.freeze_notify();
        switch (this.transform) {
            case ROTATE_90:
            case ROTATE_270:
            case FLIPPED_90:
            case FLIPPED_270:
                this.output_geometry.height = width;
                this.output_geometry.width = height;
                break;
            default:
                this.output_geometry.height = height;
                this.output_geometry.width = width;
                break;
        }
        this.refresh_rate = refresh / 1000;
        this.thaw_notify();
    }

    private void handle_done (Wl.Output wl_output) {
        if (this.xdg_output == null) {
            this.geometry.x = (int)(this.output_geometry.x / this.scale);
            this.geometry.y = (int)(this.output_geometry.y / this.scale);
            switch (this.transform) {
                case ROTATE_90:
                case ROTATE_270:
                case FLIPPED_90:
                case FLIPPED_270:
                    this.geometry.width = (int)(this.output_geometry.height / this.scale);
                    this.geometry.height = (int)(this.output_geometry.width / this.scale);
                    break;
                default:
                    this.geometry.width = (int)(this.output_geometry.width / this.scale);
                    this.geometry.height = (int)(this.output_geometry.height / this.scale);
                    break;
            }
        } else {
            this.scale = double.max(this.output_geometry.width / (double)this.geometry.width,
                    this.output_geometry.height / (double)this.geometry.height);
        }
        this.notify_property("geometry");
    }

    private void handle_scale (Wl.Output wl_output, int32 factor) {
        this.scale = factor;
    }

    private void handle_name(Wl.Output wl_output, string name) {
        this.name = name;
    }

    private void handle_description (Wl.Output wl_output, string description) {
        this.description = description;
    }

    private void handle_xdg_logical_position(ZxdgOutputV1 zxdg_output_v1, int32 x, int32 y) {
        this.geometry.x = x;
        this.geometry.y = y;
    }

    private void handle_xdg_logical_size(ZxdgOutputV1 zxdg_output_v1, int32 width, int32 height) {
        this.geometry.width = width;
        this.geometry.height = height;
    }

    /**
     * deprecated, the compositor will send wl_output.done instead.
     */
    private void handle_xdg_done(ZxdgOutputV1 zxdg_output_v1) {}

    /**
     * deprecated, use wl_output.name instead.
     */
    private void handle_xdg_name(ZxdgOutputV1 zxdg_output_v1, string name) {}

    /**
     * deprecated, use wl_output.description instead.
     */
    private void handle_xdg_description(ZxdgOutputV1 zxdg_output_v1, string description) {}

    private const ZxdgOutputV1Listener xdg_output_listener = {
        handle_xdg_logical_position,
        handle_xdg_logical_size,
        handle_xdg_done,
        handle_xdg_name,
        handle_xdg_description
    };

    private const Wl.OutputListener output_listener = {
        handle_geometry,
        handle_mode,
        handle_done,
        handle_scale,
        handle_name,
        handle_description
    };

    internal void init_xdg(ZxdgOutputManagerV1 output_manager, Wl.Display display) {
        this.xdg_output = output_manager.get_xdg_output(this.output);
        this.xdg_output.add_listener(xdg_output_listener, this);
        display.roundtrip();
    }

    internal Output(Global global, Wl.Registry registry, Wl.Display display, ZxdgOutputManagerV1 output_manager) {
        Object(id: global.name);
        this.geometry = Rectangle();
        this.output_geometry = Rectangle();
        this.output = registry.bind<Wl.Output>(global.name, ref wl_output_interface, uint.min(global.version, 4));
        this.output.add_listener(output_listener, this);
        display.roundtrip();
        if (output_manager != null) init_xdg(output_manager, display);
    }
}
}
