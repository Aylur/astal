namespace AstalHyprland {
public class Group : Object {
    public signal void destroyed();
    public signal void moved_to(Workspace workspace);

    public weak Client visible_client { get; private set; }
    private List<weak Client> _clients = new List<weak Client> ();
    public List<weak Client> clients { owned get { return _clients.copy(); } }

    public string address { get; private set; }
    public bool mapped { get; private set; }
    public bool hidden { get; private set; }
    public int x { get; private set; }
    public int y { get; private set; }
    public int width { get; private set; }
    public int height { get; private set; }
    public bool floating { get; private set; }
    public bool pinned { get; private set; }
    public Workspace workspace { get; private set; }
    public Monitor monitor { get; private set; }

    private void setClientsFromAdresses(List<string> addresses) {
        var hyprland = Hyprland.get_default();

        _clients = new List<weak Client>();
        foreach (var addr in addresses) {
            var client = hyprland?.get_client(addr);
            if (client != null)
                _clients.prepend((owned)client);
        }
        _clients.reverse();
        notify_property("clients");
    }

    internal void sync(GLib.List<string> addresses) {
        var hyprland = Hyprland.get_default();

        address = hyprland.pick_primary_address(addresses);

        setClientsFromAdresses(addresses);

        foreach (var c in _clients) {
            if (c.hidden == false) {
                visible_client = c;
                break;
            }
        }
        
        mapped = visible_client.mapped;
        hidden = visible_client.hidden;
        floating = visible_client.floating;
        pinned = visible_client.pinned;
        x = visible_client.x;
        y = visible_client.y;
        width = visible_client.width;
        height = visible_client.height;
        workspace = visible_client.workspace;
        monitor = visible_client.monitor;
    }

    public void focus() {
        visible_client.focus();
    }

    public void toggle_floating() {
        visible_client.toggle_floating();
    }
}
}