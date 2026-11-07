/**
 * A collection of devices of the same [enum@AstalBrightness.Subsystem].
 */
public class AstalBrightness.DeviceList : Object, ListModel {
    public Subsystem subsystem { get; construct; }

    FileMonitor monitor;
    HashTable<string, Device> device_table = new HashTable<string, Device>(str_hash, str_equal);
    List<weak Device> device_list = new List<weak Device>();

    /**
     * Get the full list of devices.
     */
    public List<weak Device> devices {
        owned get {
            return device_table.get_values();
        }
    }

    public Object? get_item(uint position) {
        if (position >= device_table.length) return null;
        return device_list.nth_data(position);
    }

    public Type get_item_type() {
        return typeof(Device);
    }

    public uint get_n_items() {
        return device_table.length;
    }

    public Device? get_device(string name) {
        return device_table.get(name);
    }

    /**
     * Emitted when a new sysfs device appears in the subsystem this collection is for.
     */
    public signal void device_appeared(Device device);

    /**
     * Emitted when a sysfs device disappears in the subsystem this collection is for.
     */
    public signal void device_removed(Device device);

    internal DeviceList(Subsystem subsystem) {
        Object(subsystem: subsystem);

        var dir = File.new_for_path(@"/sys/class/$subsystem");

        try {
            var enumerator = dir.enumerate_children(
                FileAttribute.STANDARD_NAME,
                FileQueryInfoFlags.NONE
            );

            FileInfo info;
            while ((info = enumerator.next_file()) != null) {
                set_device(info.get_name());
            }
        } catch (Error error) {
            critical(error.message);
        }

        try {
            monitor = File.new_for_path(@"/sys/class/$subsystem").monitor_directory(
                FileMonitorFlags.WATCH_MOVES,
                null
            );

            monitor.changed.connect((file, other_file, event) => {
                switch (event) {
                    /* *INDENT-OFF* */
                    // FIXME: uncrustify switch formatting
                    case FileMonitorEvent.CREATED:
                    case FileMonitorEvent.MOVED_IN:
                        set_device(file.get_basename());
                        sync_list();
                        break;

                    case FileMonitorEvent.DELETED:
                    case FileMonitorEvent.MOVED_OUT:
                        remove_device(file.get_basename());
                        sync_list();
                        break;

                    case FileMonitorEvent.RENAMED:
                    case FileMonitorEvent.MOVED:
                        remove_device(file.get_basename());
                        if (other_file != null) {
                            set_device(other_file.get_basename());
                        }
                        sync_list();
                        break;

                    default:
                        break;
                    /* *INDENT-ON* */
                }
            });
        } catch (Error error) {
            critical(error.message);
        }

        sync_list();
    }

    ~DeviceList() {
        if (monitor != null) {
            monitor.cancel();
        }
    }

    private void sync_list() {
        List<weak Device> next = device_table.get_values();

        uint position = 0;
        var previous_length = device_list.length();
        var next_length = next.length();
        while (
            position < previous_length &&
            position < next_length &&
            device_list.nth_data(position) == next.nth_data(position)
        ) {
            position++;
        }

        var previous_end = previous_length;
        var next_end = next_length;
        while (
            previous_end > position &&
            next_end > position &&
            device_list.nth_data(previous_end - 1) == next.nth_data(next_end - 1)
        ) {
            previous_end--;
            next_end--;
        }

        device_list = (owned)next;

        if ((position < previous_end) || (position < next_end)) {
            items_changed(position, previous_end - position, next_end - position);
        }

        notify_property("devices");
    }

    private void set_device(string name) {
        if (device_table.contains(name)) return;

        try {
            var device = new BrightnessDevice(subsystem, name);
            device_table.set(name, device);
            device_appeared(device);
        } catch (Error error) {
            critical(error.message);
        }
    }

    private void remove_device(string name) {
        var device = device_table.get(name);
        if (device != null) {
            device_removed(device);
            device_table.remove(name);
        }
    }
}
