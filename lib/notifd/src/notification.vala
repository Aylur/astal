/**
 * Class representing a notification.
 */
public class AstalNotifd.Notification : Object {
    uint32 _id = 0;
    string _app_name = "";
    string _app_icon = "";
    string _summary = "";
    string _body = "";
    Variant _hints;
    int32 _expire_timeout = -1;
    List<Action> _actions = new List<Action>();
    List<weak Action> actions_copy;

    /** State of the notification. */
    internal State state { set; get; default = State.DRAFT; }

    /** Unix time of when the notification was sent or received. */
    public int64 time { internal set; get; default = 0; }

    /** Id of the notification. */
    public uint32 id {
        get { return _id; }
        set { set_field("id", () => { _id = value; }); }
    }

    /** Name of the sending application. */
    public string app_name {
        get { return _app_name; }
        set { set_field("app-name", () => { _app_name = value; }); }
    }

    /** Icon name of the sending application. */
    public string app_icon {
        get { return _app_icon; }
        set { set_field("app-icon", () => { _app_icon = value; }); }
    }

    /** Single line overview of the notification. */
    public string summary {
        get { return _summary; }
        set { set_field("summary", () => { _summary = value; }); }
    }

    /** Multi-line body of text, where each line is a paragraph. May contain markup. */
    public string body {
        get { return _body; }
        set { set_field("body", () => { _body = value; }); }
    }

    /** Time in milliseconds after the notification expires. */
    public int32 expire_timeout {
        get { return _expire_timeout; }
        set { set_field("expire-timeout", () => { _expire_timeout = value; }); }
    }

    /**
     * List of [class@AstalNotifd.Action] of the notification.
     * Can be invoked by calling [method@AstalNotifd.Notification.invoke] with the action's id.
     */
    public List<weak Action> actions {
        get {
            if (actions_copy == null) actions_copy = _actions.copy();
            return actions_copy;
        }
    }

    /**
     * Hints of the notification. Hints are a way to provide extra data to servers.
     * To set hints on a [enum@AstalNotifd.State.DRAFT] Notification use [method@AstalNotifd.Notification.set_hint]
     * or the property setters for standard hints.
     */
    public Variant hints {
        get { return _hints; }
        internal set { _hints = value; }
    }

    /** Standard `image-path` hint. Path of an image  */
    public string image {
        owned get { return get_str_hint("image-path"); }
        set { set_hint("image-path", new Variant.string(value)); }
    }

    /**
     * Standard `action-icons` hint.
     * Indicates whether [class@AstalNotifd.Action] identifier should be interpreted as a named icon.
     */
    public bool action_icons {
        get { return get_bool_hint("action-icons"); }
        set { set_hint("action-icons", new Variant.boolean(value)); }
    }

    /**
     * Standard `category` hint.
     * [[https://specifications.freedesktop.org/notification-spec/latest/categories.html]]
     */
    public string category {
        owned get { return get_str_hint("category"); }
        set { set_hint("category", new Variant.string(value)); }
    }

    /**
     * Standard `desktop-entry` hint.
     * Specifies the name of the desktop filename representing the calling program.
     */
    public string desktop_entry {
        owned get { return get_str_hint("desktop-entry"); }
        set { set_hint("desktop-entry", new Variant.string(value)); }
    }

    /**
     * Standard `resident` hint.
     * Indicates whether notification is kept after action invocation.
     */
    public bool resident {
        get { return get_bool_hint("resident"); }
        set { set_hint("resident", new Variant.boolean(value)); }
    }

    /**
     * Standard `sound-file` hint.
     * The path to a sound file to play when the notification pops up.
     */
    public string sound_file {
        owned get { return get_str_hint("sound-file"); }
        set { set_hint("sound-file", new Variant.string(value)); }
    }

    /**
     * Standard `sound-name` hint.
     * A themeable named sound from to play when the notification pops up
     */
    public string sound_name {
        owned get { return get_str_hint("sound-name"); }
        set { set_hint("sound-name", new Variant.string(value)); }
    }

    /**
     * Standard `suppress-sound` hint.
     * Indicates to suppress playing any sounds.
     */
    public bool suppress_sound {
        get { return get_bool_hint("suppress-sound"); }
        set { set_hint("suppress-sound", new Variant.boolean(value)); }
    }

    /**
     * Standard `transient` hint.
     * Indicates that the notification should be excluded from persistency.
     */
    public bool transient {
        get { return get_bool_hint("transient"); }
        set { set_hint("transient", new Variant.boolean(value)); }
    }

    /**
     * Standard `x` hint.
     * Specifies the X location on the screen that the notification should point to.
     * The "y" hint must also be specified.
     */
    public int x {
        get { return get_int_hint("x"); }
        set { set_hint("x", new Variant.int32(value)); }
    }

    /**
     * Standard `y` hint.
     * Specifies the Y location on the screen that the notification should point to.
     * The "x" hint must also be specified.
     */
    public int y {
        get { return get_int_hint("y"); }
        set { set_hint("y", new Variant.int32(value)); }
    }

    /**
     * Standard `urgency` hint.
     * [enum@AstalNotifd.Urgency] level of the notification.
     */
    public Urgency urgency {
        get {
            var v = get_hint("urgency");
            if (v != null) {
                if (v.get_type_string() == "y") return (Urgency)v.get_byte();
                if (v.get_type_string() == "x") return (Urgency)v.get_int64();
            }
            return Urgency.NORMAL;
        }
        set { set_hint("urgency", new Variant.byte(value)); }
    }

    /**
     * Emitted when this this notification is resolved.
     *
     * @param reason The reason how the Notification was resolved.
     */
    public signal void resolved(ClosedReason reason);

    /**
     * Emitted when an [class@AstalNotifd.Action] of this notification is invoked.
     *
     * @param action_id id of the invoked action
     */
    public signal void invoked(string action_id);

    /**
     * Resolve this notification with [enum@AstalNotifd.ClosedReason.DISMISSED_BY_USER].
     */
    public void dismiss() {
        if (state == State.RECEIVED) {
            dismissed();
        } else {
            warning("notification cannot be dismissed: not a received notification");
        }
    }
    internal signal void dismissed();

    /**
     * Invoke an [class@AstalNotifd.Action] of this notification.
     */
    public void invoke(string action_id) {
        if (state == State.RECEIVED) {
            invoked(action_id);
        } else {
            warning("action cannot be invoked: not a received notification");
        }
    }

    public Notification add_action(Action action) {
        if (state != State.DRAFT) {
            critical("cannot add action: notification is not a draft");
            return this;
        }

        if (action.notification != null) {
            critical("cannot add action: action is already added to a notification");
            return this;
        }

        action.notification = this;
        _actions.append(action);
        notify_property("actions");
        return this;
    }

    public Notification set_hint(string name, Variant value) {
        if (state != State.DRAFT) {
            critical(@"cannot set hint '$name': notification is not a draft");
            return this;
        }

        var dict = new VariantDict(_hints);
        dict.insert_value(name, value);
        _hints = dict.end();
        notify_property("hints");
        return this;
    }

    public Variant? get_hint(string name) {
        return new VariantDict(_hints).lookup_value(name, VariantType.VARIANT);
    }

    private string get_str_hint(string name) {
        return get_hint(name) ? .get_string(null) ?? "";
    }

    private int32 get_int_hint(string name) {
        return get_hint(name) ? .get_int32() ?? 0;
    }

    private bool get_bool_hint(string name) {
        return get_hint(name) ? .get_boolean() ?? false;
    }

    internal Notification.deserialize(Variant variant) {
        var dict = new VariantDict(variant);
        time = dict.lookup_value("time", VariantType.INT64).get_int64();
        _id = dict.lookup_value("id", VariantType.UINT32).get_uint32();
        _app_name = dict.lookup_value("app-name", VariantType.STRING).get_string();
        _app_icon = dict.lookup_value("app-icon", VariantType.STRING).get_string();
        _summary = dict.lookup_value("summary", VariantType.STRING).get_string();
        _body = dict.lookup_value("body", VariantType.STRING).get_string();
        _hints = dict.lookup_value("hints", VariantType.DICTIONARY);
        _expire_timeout = dict.lookup_value("expire-timeout", VariantType.INT32).get_int32();

        _actions = new List<Action>();
        var actions = dict.lookup_value("actions", VariantType.ARRAY);
        VariantIter iter = actions.iterator();
        string? id;
        string? label;

        while (iter.next("{ss}", out id, out label)) {
            _actions.append(new Action(id, label));
        }
    }

    internal Variant serialize() {
        var actions = new VariantBuilder(new VariantType.array(new VariantType("{ss}")));
        foreach (var action in _actions) {
            actions.add("{ss}", action.id, action.label);
        }

        return new Variant.array(new VariantType("{sv}"), {
            new Variant("{sv}", "time", new Variant.int64(time)),
            new Variant("{sv}", "id", new Variant.uint32(_id)),
            new Variant("{sv}", "app-name", new Variant.string(_app_name)),
            new Variant("{sv}", "app-icon", new Variant.string(_app_icon)),
            new Variant("{sv}", "summary", new Variant.string(_summary)),
            new Variant("{sv}", "body", new Variant.string(_body)),
            new Variant("{sv}", "hints", _hints),
            new Variant("{sv}", "expire-timeout", new Variant.int32(_expire_timeout)),
            new Variant("{sv}", "actions", actions.end()),
        });
    }

    private delegate void VoidFunc();
    private void set_field(string name, VoidFunc fn) {
        if (state != State.DRAFT) {
            critical(@"cannot set $name: notification is not a draft");
            return;
        }
        fn();
    }

    construct {
        if (_hints == null) {
            _hints = new Variant.array(new VariantType("{sv}"), {});
        }
    }
}
