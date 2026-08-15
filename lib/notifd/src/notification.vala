/**
 * Class representing a notification.
 */
public class AstalNotifd.Notification : Object {
    uint32 _id = 0;
    string? _app_name = "";
    string? _app_icon = "";
    string _summary = "";
    string _body = "";
    Variant _hints = new Variant.array(new VariantType("{sv}"), {});
    int32 _expire_timeout = -1;
    List<Action> _actions = new List<Action>();
    List<weak Action> _actions_copy;

    /** State of the notification. */
    public State state { internal set; get; default = State.DRAFT; }

    /** Unix time when the notification was sent or received. */
    public int64 time { internal set; get; default = 0; }

    /** ID of the notification. */
    public uint32 id {
        get { return _id; }
        set { set_field("id", () => { _id = value; }); }
    }

    /** Name of the sending application. */
    public string? app_name {
        get { return _app_name; }
        set { set_field("app-name", () => { _app_name = value; }); }
    }

    /** Icon name of the sending application. */
    public string? app_icon {
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

    /** Time in milliseconds after which the notification expires. */
    public int32 expire_timeout {
        get { return _expire_timeout; }
        set { set_field("expire-timeout", () => { _expire_timeout = value; }); }
    }

    /**
     * List of [class@AstalNotifd.Action]s associated with the notification.
     */
    public List<weak Action> actions {
        get {
            if (_actions_copy == null) _actions_copy = _actions.copy();
            return _actions_copy;
        }
    }

    /**
     * Notification hints. Hints are a way to provide extra data to servers.
     * To set hints on a `DRAFT` notification, use [method@AstalNotifd.Notification.set_hint]
     * or the dedicated property setters for standard hints.
     */
    public Variant hints {
        get { return _hints; }
    }

    /**
     * Extra hints that are merged with [property@AstalNotifd.Notification:hints].
     */
    public Variant extra_hints {
        construct {
            if (value == null) return;
            return_if_fail(value.get_type().dup_string() != "");

            var iter = value.iterator();
            var dict = new VariantDict(_hints);

            string key;
            Variant variant;
            while (iter.next("{sv}", out key, out variant)) {
                print(@"$key $(variant.print(false))\n");
                dict.insert_value(key, variant);
            }

            _hints = dict.end();
        }
    }

    /** Standard `image-path` hint. Path to an image. */
    public string image {
        owned get { return get_str_hint("image-path"); }
        set { set_hint("image-path", new Variant.string(value)); }
    }

    /**
     * Standard `action-icons` hint.
     * Indicates whether [class@AstalNotifd.Action] identifiers should be interpreted as named icons.
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
     * Indicates whether the notification is kept after action invocation.
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
     * A themeable named sound to play when the notification pops up.
     */
    public string sound_name {
        owned get { return get_str_hint("sound-name"); }
        set { set_hint("sound-name", new Variant.string(value)); }
    }

    /**
     * Standard `suppress-sound` hint.
     * Indicates to suppress playing any sound.
     */
    public bool suppress_sound {
        get { return get_bool_hint("suppress-sound"); }
        set { set_hint("suppress-sound", new Variant.boolean(value)); }
    }

    /**
     * Standard `transient` hint.
     * Indicates that the notification should be excluded from persistence.
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
            if (get_hint("urgency") == null) return Urgency.NORMAL;
            var v = get_int_hint("urgency");
            if (v < Urgency.LOW) return Urgency.LOW;
            if (v > Urgency.CRITICAL) return Urgency.CRITICAL;
            return (Urgency)v;
        }
        set { set_hint("urgency", new Variant.byte(value)); }
    }

    /**
     * Emitted when this notification is resolved.
     *
     * @param reason The reason the notification was resolved.
     */
    public signal void resolved(ClosedReason reason);

    /**
     * Emitted when an [class@AstalNotifd.Action] of this notification is invoked.
     *
     * @param action_id ID of the invoked action.
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
     * Resolve this notification with [enum@AstalNotifd.ClosedReason.EXPIRED].
     * Note that there should be no reason to use this method because expiration should be
     * left to the daemon.
     */
    public void expire() {
        if (state == State.RECEIVED) {
            expired();
        } else {
            warning("notification cannot be expired: not a received notification");
        }
    }
    internal signal void expired();

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
        var hint = new VariantDict(_hints).lookup_value(name, VariantType.ANY);
        // lookup_value only unwraps the outer variant, but D-Bus allows nesting them
        while (hint != null && hint.is_of_type(VariantType.VARIANT)) {
            hint = hint.get_variant();
        }
        return hint;
    }

    private string get_str_hint(string name) {
        var hint = get_hint(name);
        if (hint == null) return "";
        if (
            hint.is_of_type(VariantType.STRING)
            || hint.is_of_type(VariantType.OBJECT_PATH)
            || hint.is_of_type(VariantType.SIGNATURE)
        ) {
            return hint.get_string(null) ?? "";
        }
        if (hint.is_of_type(VariantType.BYTESTRING)) return hint.get_bytestring() ?? "";
        return "";
    }

    private int32 get_int_hint(string name) {
        var hint = get_hint(name);
        if (hint == null) return 0;
        if (hint.is_of_type(VariantType.INT32)) return hint.get_int32();
        if (hint.is_of_type(VariantType.UINT32)) return (int32)hint.get_uint32();
        if (hint.is_of_type(VariantType.BYTE)) return hint.get_byte();
        if (hint.is_of_type(VariantType.INT16)) return hint.get_int16();
        if (hint.is_of_type(VariantType.UINT16)) return hint.get_uint16();
        if (hint.is_of_type(VariantType.INT64)) return (int32)hint.get_int64();
        if (hint.is_of_type(VariantType.UINT64)) return (int32)hint.get_uint64();
        if (hint.is_of_type(VariantType.HANDLE)) return hint.get_handle();
        if (hint.is_of_type(VariantType.DOUBLE)) return (int32)hint.get_double();
        if (hint.is_of_type(VariantType.BOOLEAN)) return hint.get_boolean() ? 1 : 0;
        return 0;
    }

    private bool get_bool_hint(string name) {
        var hint = get_hint(name);
        if (hint == null) return false;
        if (hint.is_of_type(VariantType.BOOLEAN)) return hint.get_boolean();
        return get_int_hint(name) != 0;
    }

    internal Notification.deserialize(Variant variant) {
        var dict = new VariantDict(variant);
        time = dict.lookup_value("time", VariantType.INT64).get_int64();
        _id = dict.lookup_value("id", VariantType.UINT32).get_uint32();

        var app_name = dict.lookup_value("app-name", VariantType.STRING);
        if (app_name != null) _app_name = app_name.get_string();

        var app_icon = dict.lookup_value("app-icon", VariantType.STRING);
        if (app_icon != null) _app_icon = app_icon.get_string();

        _summary = dict.lookup_value("summary", VariantType.STRING).get_string();
        _body = dict.lookup_value("body", VariantType.STRING).get_string();
        _hints = dict.lookup_value("hints", VariantType.DICTIONARY);
        _expire_timeout = dict.lookup_value("expire-timeout", VariantType.INT32).get_int32();

        var actions = dict.lookup_value("actions", VariantType.ARRAY);
        VariantIter iter = actions.iterator();
        string? id;
        string? label;

        while (iter.next("{ss}", out id, out label)) {
            add_action(new Action(id, label));
        }
    }

    internal Variant serialize() {
        var actions_builder = new VariantBuilder(new VariantType.array(new VariantType("{ss}")));
        foreach (var action in this.actions) {
            actions_builder.add("{ss}", action.id, action.label);
        }

        var dict = new VariantDict();
        dict.insert_value("time", new Variant.int64(time));
        dict.insert_value("id", new Variant.uint32(_id));
        if ((_app_name != "") && (_app_name != null)) {
            dict.insert_value("app-name", new Variant.string(_app_name));
        }
        if ((_app_icon != "") && (_app_icon != null)) {
            dict.insert_value("app-icon", new Variant.string(_app_icon));
        }
        dict.insert_value("summary", new Variant.string(_summary));
        dict.insert_value("body", new Variant.string(_body));
        dict.insert_value("hints", _hints);
        dict.insert_value("expire-timeout", new Variant.int32(_expire_timeout));
        dict.insert_value("actions", actions_builder.end());

        return dict.end();
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
