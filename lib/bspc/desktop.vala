namespace AstalBspc {
    public class Desktop : Object {
        public signal void removed ();
        public signal void swap ();

        internal HashTable<string, Node> _nodes   = new HashTable<string, Node>(str_hash, str_equal);

        public string id { get; internal set; }
        public string name { get; private set; }
        public string layout { get; private set;}
        public List<weak Node> nodes { owned get { return _nodes.get_values(); } }

        internal void sync(Json.Object obj) {}

        public void focus() {
            Bspc.get_default().message_async.begin(@"desktop $id -f", (_, task) => 
                Bspc.get_default().message_async.end(task));
        }
    }
}
