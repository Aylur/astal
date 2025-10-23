namespace AstalNiri {
public class Overview : Object {
    public bool is_open   {get; internal set;}

    internal Overview.from_json(Json.Object object) {
        sync(object);
    }

    public async bool toggle() {
        return AstalNiri.msg.toggle_overview();
    }

    internal void sync(Json.Object object) {
        is_open = object.get_boolean_member("is_open");
    }
}
}
