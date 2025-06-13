namespace AstalNiri {
public class Overview : Object {
    public bool is_open   {get; internal set;}

    internal Overview.from_json(Json.Object object) {
        sync(object);
    }

    // finish after merging messages
    // public async bool toggle() {
    //     Niri.get_default().msg("toggle-overview");
    //     return is_open;
    // }

    internal void sync(Json.Object object) {
        is_open = object.get_boolean_member("is_open");
    }
}
}
