/**
 * Notification action.
 */
public class AstalNotifd.Action : Object {
    /** Id of this action. */
    public string id { construct set; get; default = "1"; }

    /** Label of this action that should be displayed to user. */
    public string label { construct set; get; default = ""; }

    /** Emitted when the notification this action was added to invoked this action. */
    public signal void invoked();

    public Action(string id, string label) {
        Object(id: id, label: label);
    }

    private Notification _notification;
    internal Notification notification {
        get { return _notification; }
        set {
            if (_notification == null) {
                _notification = value;
                value.invoked.connect((action_id) => {
                    if (action_id == id) {
                        invoked();
                    }
                });
            }
        }
    }

    /**
     * Invoke this action.
     * Note that this method just notifies the client that this action was invoked
     * by the user. If for example this notification persists through the lifetime
     * of the sending application this action will have no effect.
     */
    public void invoke() {
        if (notification == null) {
            critical("cannot invoke action: not added to any notification");
        } else {
            notification.invoked(id);
        }
    }

    internal static List<Action> new_list(string[] strv) {
        var actions = new List<Action>();
        for (var i = 0; i < strv.length; i += 2) {
            actions.append(new Action(strv[i], strv[i + 1]));
        }
        return actions;
    }
}
