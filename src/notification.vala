namespace AstalNotifd {
public enum Urgency {
    LOW = 0,
    NORMAL = 1,
    CRITICAL = 2,
}

public class Action {
    public Action(string id, string label) {
        this.id = id;
        this.label = label;
    }
    public string id;
    public string label;
}

public class Notification : Object {
    private List<Action> _actions;
    private HashTable<string, Variant> hints;

    public int64 time { construct set; get; }
    public string app_name { construct set; get; }
    public string app_icon { construct set; get; }
    public string summary { construct set; get; }
    public string body { construct set; get; }
    public uint id { construct set; get; }
    public int expire_timeout { construct set; get; }
    public List<Action> actions { get { return _actions; } }

    public string image { get { return get_str_hint("image-path"); } }
    public bool action_icons { get { return get_bool_hint("action-icons"); } }
    public string category { get { return get_str_hint("category"); } }
    public string desktop_entry { get { return get_str_hint("desktop-entry"); } }
    public bool resident { get { return get_bool_hint("resident"); } }
    public string sound_file { get { return get_str_hint("sound-file"); } }
    public string sound_name { get { return get_str_hint("sound-name"); } }
    public bool suppress_sound { get { return get_bool_hint("suppress-sound"); } }
    public bool transient { get { return get_bool_hint("transient"); } }
    public int x { get { return get_int_hint("x"); } }
    public int y { get { return get_int_hint("y"); } }
    public Urgency urgency { get { return get_int_hint("urgency"); } }

    internal Notification(
        string app_name,
        uint id,
        string app_icon,
        string summary,
        string body,
        string[] actions,
        HashTable<string, Variant> hints,
        int expire_timeout
    ) {
        Object(
            app_name: app_name,
            id: id,
            app_icon: app_icon,
            summary: summary,
            body: body,
            expire_timeout: expire_timeout,
            time: new DateTime.now_local().to_unix()
        );

        this.hints = hints;
        _actions = new List<Action>();
        for (var i = 0; i < actions.length; i += 2) {
            _actions.append(new Action(actions[i], actions[i + 1]));
        }
    }

    public Variant? get_hint(string hint) {
        return hints.contains(hint) ? hints.get(hint) : null;
    }

    public unowned string get_str_hint(string hint) {
        return hints.contains(hint) ? hints.get(hint).get_string() : null;
    }

    public bool get_bool_hint(string hint) {
        return hints.contains(hint) ? hints.get(hint).get_boolean() : false;
    }

    public int get_int_hint(string hint) {
        return hints.contains(hint) ? hints.get(hint).get_int32() : 0;
    }

    public signal void resolved(ClosedReason reason);
    public signal void dismissed();
    public signal void invoked(string action);

    public void dismiss() { dismissed(); }
    public void invoke(string action) { invoked(action); }

    public Notification.from_json(Json.Object root) throws GLib.Error {
        foreach (var key in root.get_members()) {
            var node = root.get_member(key);
            switch (key) {
                case "id": id = (uint)node.get_int(); break;
                case "time": time = node.get_int(); break;
                case "expire_timeout": expire_timeout = (int)node.get_int(); break;
                case "app_name": app_name = node.get_string(); break;
                case "app_icon": app_icon = node.get_string(); break;
                case "summary": summary = node.get_string(); break;
                case "body": body = node.get_string(); break;
                case "hints":
                    hints = new HashTable<string, Variant>(str_hash, str_equal);
                    var obj = node.get_object();
                    foreach (var hint in obj.get_members()) {
                        hints.set(hint, Json.gvariant_deserialize(obj.get_member(hint), null));
                    }
                    break;
                case "actions":
                    _actions = new List<Action>();
                    for (var i = 0; i < node.get_array().get_length(); ++i) {
                        var o = node.get_array().get_object_element(i);
                        _actions.append(new Action(
                            o.get_member("id").get_string(),
                            o.get_member("label").get_string()
                        ));
                    }
                    break;
                default: break;
            }
        }
    }

    public static Notification from_json_string(string json) throws GLib.Error {
        var parser = new Json.Parser();
        parser.load_from_data(json);
        return new Notification.from_json(parser.get_root().get_object());
    }

    public string to_json_string() {
    	var generator = new Json.Generator();
        generator.set_root(to_json());
        return generator.to_data(null);
    }

    public Json.Node to_json() {
        var acts = new Json.Builder().begin_array();
        foreach (var action in actions) {
            acts.begin_object()
            .set_member_name("id").add_string_value(action.id)
            .set_member_name("label").add_string_value(action.label)
            .end_object();
        }
        acts.end_array();

        return new Json.Builder()
            .begin_object()
            .set_member_name("id").add_int_value(id)
            .set_member_name("time").add_int_value(time)
            .set_member_name("expire_timeout").add_int_value(expire_timeout)
            .set_member_name("app_name").add_string_value(app_name)
            .set_member_name("app_icon").add_string_value(app_icon)
            .set_member_name("summary").add_string_value(summary)
            .set_member_name("body").add_string_value(body)
            .set_member_name("actions").add_value(acts.get_root())
            .set_member_name("hints").add_value(Json.gvariant_serialize(hints))
            .end_object()
            .get_root();
    }
}
}
