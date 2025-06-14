namespace AstalBspc {
    public class Desktop : Object {
        public signal void removed();

        internal HashTable<string, Node> _nodes   = new HashTable<string, Node>(str_hash, str_equal);

        public string id { get; internal set; }
        public string name { get; internal set; }
        public string layout { get; private set;}
        public Padding padding { get; private set;}

        public List<weak Node> nodes { owned get { return _nodes.get_values(); } }

        internal void sync(Json.Object obj) {
            name = obj.get_string_member("name");
            layout = obj.get_string_member("layout");
            padding = new Padding(obj.get_object_member("padding"));
        }

        public void focus() {
            Bspc.get_default().message_async.begin(@"desktop $id -f", (_, task) => 
                Bspc.get_default().message_async.end(task));
        }
    }
}
