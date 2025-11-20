namespace AstalBrightness {
  public Brightness get_default() {
    return Brightness.get_default();
  }
}

public class AstalBrightness.Brightness : Object {
  private static Brightness _instance;

  public static Brightness get_default() {
    if (_instance == null)
      _instance = new Brightness();

    return _instance;
  }

  private string _backlight_model;
  private int _max_brightness;
  private double _brightness;
  public FileMonitor _file_monitor;

  public double brightness {
    get { return _brightness; }
    set {
      if (_brightness == value)
        return;

      value = value.clamp(0.0, 1.0);

      string brightness_file = "/sys/class/backlight/" + _backlight_model + "/brightness";

      int new_value = (int)(value * _max_brightness);

      try {
        FileUtils.set_contents_full(brightness_file, new_value.to_string(), -1, GLib.FileSetContentsFlags.ONLY_EXISTING, 0666);
      } catch (Error e) {
        critical(e.message);
      }
    }
  }

  construct {
    try {
      Dir dir = Dir.open("/sys/class/backlight/", 0);
      if (dir != null) {
        _backlight_model = dir.read_name();
      }

      string max_brightness_file = "/sys/class/backlight/" + _backlight_model + "/max_brightness";
      string brightness_file = "/sys/class/backlight/" + _backlight_model + "/brightness";

      string content;

      FileUtils.get_contents(max_brightness_file, out content);
      _max_brightness = int.parse(content.strip());

      FileUtils.get_contents(brightness_file, out content);
      _brightness = int.parse(content.strip()) / (double)_max_brightness;

      var file = File.new_for_path(brightness_file);
      _file_monitor = file.monitor(FileMonitorFlags.NONE);
      _file_monitor.changed.connect(on_brightness_file_changed);
    } catch (Error e) {
      critical(e.message);
    }
  }

  private void on_brightness_file_changed(FileMonitor monitor, File file, File? other_file, FileMonitorEvent event_type) {
    if (event_type == FileMonitorEvent.CHANGES_DONE_HINT) {
      string brightness_file = "/sys/class/backlight/" + _backlight_model + "/brightness";
      try {
        string content;
        FileUtils.get_contents(brightness_file, out content);
        _brightness = int.parse(content.strip()) / (double)_max_brightness;
        notify_property("brightness");
      } catch (Error e) {
        critical(e.message);
      }
    }
  }
}

