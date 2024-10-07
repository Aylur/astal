public enum AstalNotifd.Urgency {
    LOW = 0,
    NORMAL = 1,
    CRITICAL = 2,
}

public struct AstalNotifd.Action {
    public string id;
    public string label;
}

/**
 * Class representing a notification.
 */
public class AstalNotifd.Notification : Object {
    private List<Action?> _actions;
    private HashTable<string, Variant> hints;

    /** Unix time of when the notification was sent. */
    public int64 time { private construct set; get; }

    /** Name of the sending application. */
    public string app_name { private construct set; get; }

    /** Icon name of the sending application. */
    public string app_icon { private construct set; get; }

    /** Single line overview of the notification. */
    public string summary { private construct set; get; }

    /** Multi-line body of text, where each line is a paragraph. May contain markup. */
    public string body { private construct set; get; }

    /** Id of the notification. */
    public uint id { private construct set; get; }

    /** Time in milliseconds after the notification expires. */
    public int expire_timeout { private construct set; get; }

    /**
     * List of {@link Action} of the notification.
     *
     * Can be invoked by calling {@link Notification.invoke} with the action's id.
     */
    public List<Action?> actions { get { return _actions; } }

    /** Path of an image  */
    public string image { get { return get_str_hint("image-path"); } }

    /** Indicates whether {@link Action} identifier should be interpreted as a named icon.  */
    public bool action_icons { get { return get_bool_hint("action-icons"); } }

    /** [[https://specifications.freedesktop.org/notification-spec/latest/categories.html|Category of the notification.]] */
    public string category { get { return get_str_hint("category"); } }

    /** Specifies the name of the desktop filename representing the calling program. */
    public string desktop_entry { get { return get_str_hint("desktop-entry"); } }

    /** Indicates whether notification is kept after action invocation.  */
    public bool resident { get { return get_bool_hint("resident"); } }

    /** The path to a sound file to play when the notification pops up.  */
    public string sound_file { get { return get_str_hint("sound-file"); } }

    /** A themeable named sound from to play when the notification pops up */
    public string sound_name { get { return get_str_hint("sound-name"); } }

    /** Indicates to suppress playing any sounds. */
    public bool suppress_sound { get { return get_bool_hint("suppress-sound"); } }

    /** Indicates that the notification should be excluded from persistency. */
    public bool transient { get { return get_bool_hint("transient"); } }

    /** Specifies the X location on the screen that the notification should point to. The "y" hint must also be specified.  */
    public int x { get { return get_int_hint("x"); } }

    /** Specifies the Y location on the screen that the notification should point to. The "x" hint must also be specified.  */
    public int y { get { return get_int_hint("y"); } }

    /** {@link Urgency} level of the notification. */
    public Urgency urgency { get { return get_byte_hint("urgency"); } }

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
        _actions = new List<Action?>();
        for (var i = 0; i < actions.length; i += 2) {
            _actions.append(Action() {
                id = actions[i],
                label = actions[i + 1]
            });
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

    public uint8 get_byte_hint(string hint) {
        if (!hints.contains(hint))
            return 0;

        var v = hints.get(hint);
        if (v.get_type_string() == "b")
            return v.get_byte();

        if (v.get_type_string() == "x")
            return (uint8)v.get_int64();

        return 0;
    }

    /**
     * Emitted when this {@link Notification} is resolved.
     *
     * @param reason The reason how the Notification was resolved.
     */
    public signal void resolved(ClosedReason reason);

    /**
     * Emitted when the user dismisses this {@link Notification}
     *
     * @see dismiss
     */
    public signal void dismissed();

    /**
     * Emitted when an {@link Action} of this {@link Notification} is invoked.
     *
     * @param action_id id of the invoked action
     * @see invoke
     */
    public signal void invoked(string action_id);

    /**
     * Dismiss this notification popup
     *
     * This method doesn't have any functionality on its own, but should be handled
     * by frontend implementation to hide notification popups.
     */
    public void dismiss() { dismissed(); }

    /**
     * Invoke an {@link Action} of this {@link Notification}
     *
     * Note that this method just notifies the client that this action was invoked
     * by the user. If for example this notification persists through the lifetime
     * of the sending program this action will have no effect.
     */
    public void invoke(string action_id) { invoked(action_id); }

    internal Notification.from_json(Json.Object root) throws GLib.Error {
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
                    _actions = new List<Action?>();
                    for (var i = 0; i < node.get_array().get_length(); ++i) {
                        var o = node.get_array().get_object_element(i);
                        _actions.append(Action() {
                            id = o.get_member("id").get_string(),
                            label = o.get_member("label").get_string()
                        });
                    }
                    break;
                default: break;
            }
        }
    }

    internal static Notification from_json_string(string json) throws GLib.Error {
        var parser = new Json.Parser();
        parser.load_from_data(json);
        return new Notification.from_json(parser.get_root().get_object());
    }

    internal string to_json_string() {
    	var generator = new Json.Generator();
        generator.set_root(to_json());
        return generator.to_data(null);
    }

    internal Json.Node to_json() {
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
