class AstalBrightness.DummyDevice : Object, Device {
    public Subsystem subsystem { get { return Subsystem.BACKLIGHT; } }
    public string name { get { return "dummy"; } }
    public float brightness {
        get { return -1; }
        set {}
    }
    public uint real_brightness {
        get { return uint.MAX; }
        set {}
    }
    public uint max_brightness { get { return 0; } }
}

class AstalBrightness.DeviceProxy : Object, Device {
    static string[] device_properties = {
        "max-brightness",
        "real-brightness",
        "brightness",
    };

    Device _proxied = new DummyDevice();
    ulong[] _handlers = {};

    public Device proxied {
        get {
            return _proxied;
        }
        set {
            if (_handlers.length > 0) {
                foreach (var id in _handlers) {
                    _proxied.disconnect(id);
                }
            }

            _handlers = {};
            _proxied = value;

            foreach (var property in device_properties) {
                notify_property(property);

                _handlers += _proxied.notify[property].connect(() => {
                    notify_property(property);
                });
            }

            notify_property("name");
        }
    }

    public Subsystem subsystem {
        get { return _proxied.subsystem; }
    }

    public string name {
        get { return _proxied.name; }
    }

    public float brightness {
        get { return _proxied.brightness; }
        set { _proxied.brightness = value; }
    }

    public uint real_brightness {
        get { return _proxied.real_brightness; }
        set { _proxied.real_brightness = value; }
    }

    public uint max_brightness {
        get { return _proxied.max_brightness; }
    }
}
