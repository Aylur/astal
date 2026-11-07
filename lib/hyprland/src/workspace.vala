namespace AstalHyprland {
    public class Workspace : Object {
        public signal void removed();

        public List<weak Client> _clients = new List<weak Client>();

        /**
         * Numeric ID or a local compatibility handle.
         *
         * For workspaces without a numeric ID this is a unique negative handle,
         * valid only for this connection. Use the workspace methods for IPC; the handle is not a compositor ID.
         */
        public int id { get; private set; }
        /**
         * Canonical Hyprland identity, independent of the display name.
         */
        public string address { get; private set; }

        internal void identify(int id, string address) {
            this.id = id;
            this.address = address;
        }
        private string target_selector;
        internal string selector { get { return target_selector ?? address; } }

        public string name { get; private set; }
        public Monitor monitor { get; private set; }
        public List<weak Client> clients { owned get { return _clients.copy(); } }
        public bool has_fullscreen { get; private set; }
        public Client last_client { get; private set; }

        public Workspace.dummy(int id, Monitor? monitor) {
            identify(id, id.to_string());
            target_selector = id.to_string();
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

        internal void sync(Json.Object obj) {
            var hyprland = Hyprland.get_default();

            name = obj.get_string_member("name");
            var canonical = WorkspaceIdentity.string_member(obj, "address");
            var kind = WorkspaceIdentity.string_member(obj, "type");
            if (kind == "special" || (canonical == null && name.has_prefix("special:")))
                target_selector = canonical ?? name;
            else if (obj.has_member("id") && id > 0) target_selector = id.to_string();
            else target_selector = "name:" + (canonical ?? name);
            has_fullscreen = obj.get_boolean_member("hasfullscreen");

            monitor = hyprland.get_monitor((int)obj.get_int_member("monitorID"));
            last_client = hyprland.get_client(obj.get_string_member("lastwindow"));

            refresh_clients();
        }

        internal void refresh_clients() {
            var list = filter_clients();
            bool changed = _clients.length() != list.length();
            foreach (var client in list) {
                if (_clients.find(client) == null) changed = true;
            }
            if (changed) {
                _clients = list.copy();
                notify_property("clients");
            }
        }

        public void focus() {
            Hyprland.get_default().dispatch("workspace", selector);
        }

        public void move_to(Monitor m) {
            Hyprland.get_default().dispatch(
                "moveworkspacetomonitor",
                selector + " " + m.id.to_string()
            );
        }
    }
}
