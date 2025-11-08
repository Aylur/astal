/**
 * Object representing a [[https://github.com/bluez/bluez/blob/master/doc/org.bluez.Battery.rst|battery]].
 */
internal class AstalBluetooth.Battery : Object {
    private IBattery proxy;

    internal ObjectPath object_path { owned get; private set; }

    internal Battery(IBattery proxy) {
        this.proxy = proxy;
        this.object_path = (ObjectPath)proxy.g_object_path;
        proxy.g_properties_changed.connect((props) => {
            var map = (HashTable<string, Variant>)props;
            foreach (var key in map.get_keys()) {
                var prop = kebab_case(key);
                if (get_class().find_property(prop) != null) {
                    notify_property(prop);
                }
            }
        });
    }

    /**
     * The percentage of battery left as an unsigned 8-bit integer.
     */
    public uint percentage { get { return proxy.percentage; } }

    /**
     * Describes where the battery information comes from.
     */
    public string source { owned get { return proxy.source; } }
}
