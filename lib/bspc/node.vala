namespace AstalBspc {
    public class Node : Object {
        public signal void removed();
        public signal void moved_to (Desktop desktop);

        public string id { get; internal set; }
        public bool floating { get; internal set; }
        public Desktop desktop { get; internal set; }
        public int x { get; internal set; }
        public int y { get; internal set; }
        public int width { get; internal set; }
        public int height { get; internal set; }
        public NodeState state { get; internal set; }
        public string class_name { get; private set; }
        public string instance_name { get; private set; }

        public async void focus() {
            Bspc.get_default().message_async.begin(@"node $id -f", (_, task) =>
                Bspc.get_default().message_async.end(task));
        }

        public async void kill() {
            Bspc.get_default().message_async.begin(@"node $id -c", (_, task) =>
                Bspc.get_default().message_async.end(task));
        }

        internal void sync(Json.Object obj) {
            var client = obj.get_object_member("client");
            var geom = client.get_object_member(client.get_string_member("state") + "Rectangle");

            class_name = client.get_string_member("className");
            instance_name = client.get_string_member("instanceName");
            x = (int)geom.get_int_member("x");
            y = (int)geom.get_int_member("y");
            width = (int)geom.get_int_member("width");
            height = (int)geom.get_int_member("height");
            state = NodeState.parse(client.get_string_member("state"));
        }
    }

    public enum NodeState {
        TILED = 0,
        PSEUDO_TILED = 1,
        FLOATING = 2,
        FULLSCREEN = 3; 

        public static NodeState parse(string value) {
            switch (value) {
                case "tiled":
                    return NodeState.TILED;
                case "pseudo_tiled":
                    return NodeState.PSEUDO_TILED;
                case "floating":
                    return NodeState.FLOATING;
                case "fullscreen":
                    return NodeState.FULLSCREEN;
                default:
                    return NodeState.TILED;
            }
        }
    }
}
