namespace AstalHyprland {
public class Bind : Object {
    public bool locked { get; construct set; }
    public bool mouse { get; construct set; }
    public bool release { get; construct set; }
    public bool repeat { get; construct set; }
    public bool non_consuming { get; construct set; }
    public int64 modmask { get; construct set; }
    public string submap { get; construct set; }
    public string key { get; construct set; }
    public int64 keycode { get; construct set; }
    public bool catch_all { get; construct set; }
    public string dispatcher { get; construct set; }
    public string arg { get; construct set; }

    internal Bind.from_json(Json.Object obj) {
        locked = obj.get_boolean_member("locked");
        mouse = obj.get_boolean_member("mouse");
        release = obj.get_boolean_member("release");
        repeat = obj.get_boolean_member("repeat");
        non_consuming = obj.get_boolean_member("non_consuming");
        modmask = obj.get_int_member("modmask");
        submap = obj.get_string_member("submap");
        key = obj.get_string_member("key");
        keycode = obj.get_int_member("keycode");
        catch_all = obj.get_boolean_member("catch_all");
        dispatcher = obj.get_string_member("dispatcher");
        arg = obj.get_string_member("arg");
    }
}

public class Position : Object {
    public int x { get; construct set; }
    public int y  { get; construct set; }

    internal Position.cursorpos(string pos) {
        var xy = pos.split(",");
        x = int.parse(xy[0].strip());
        y = int.parse(xy[1].strip());
    }
}
}
