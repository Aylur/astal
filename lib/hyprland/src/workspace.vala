namespace AstalHyprland {
public class Workspace : Object {
    public signal void removed();

    public List<weak Client> _clients = new List<weak Client>();
    public List<weak Group> _groups = new List<weak Group>();

    public int id { get; private set; }
    public string name { get; private set; }
    public Monitor monitor { get; private set; }
    public List<weak Client> clients { owned get { return _clients.copy(); } }
    public List<weak Group> groups { owned get { return _groups.copy(); } }
    public bool has_fullscreen { get; private set; }
    public Client last_client { get; private set; }

    public Workspace.dummy(int id, Monitor ? monitor) {
        this.id = id;
        this.name = id.to_string();
        this.monitor = monitor;
    }

    internal List<weak Client> filter_clients() {
        var hyprland = Hyprland.get_default();
        var list = new List<weak Client>();
        foreach (var client in hyprland.clients) {
            if (client.workspace == this) {
                list.append(client);
            }
        }

        return list;
    }

    internal List<weak Group> filter_groups() {
        var hyprland = Hyprland.get_default();
        var list = new List<weak Group>();
        foreach (var group in hyprland.groups) {
            if (group.workspace == this) {
                list.append(group);
            }
        }

        return list;
    }

    internal void sync(Json.Object obj) {
        var hyprland = Hyprland.get_default();

        id = (int)obj.get_int_member("id");
        name = obj.get_string_member("name");
        has_fullscreen = obj.get_boolean_member("hasfullscreen");

        monitor = hyprland.get_monitor((int)obj.get_int_member("monitorID"));
        last_client = hyprland.get_client(obj.get_string_member("lastwindow"));

        var clients_list = filter_clients();
        if (_clients.length() != clients_list.length()) {
            _clients = clients_list.copy();
            notify_property("clients");
        }

        var groups_list = filter_groups();
        if (_groups.length() != groups_list.length()) {
            _groups = groups_list.copy();
            notify_property("groups");
        }
    }

    public void focus() {
        Hyprland.get_default().dispatch("workspace", id.to_string());
    }

    public void move_to(Monitor m) {
        Hyprland.get_default().dispatch("moveworkspacetomonitor", id.to_string() + " " + m.id.to_string());
    }
}
}
