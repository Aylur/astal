namespace AstalBspc {
    public class Desktop : Object {
        public signal void removed ();
        public signal void swap ();

        public List<weak Node> _nodes = new List<weak Node>();

        public string id { get; private set; }
        public string name { get; private set; }
        public string layout { get; private set;}
        public List<weak Node> nodes { owned get { return _nodes.copy(); } }



        internal List<weak Node> filter_nodes() {
            var bspc = Bspc.get_default();
            var list = new List<weak Node>();
            foreach (var node in bspc.nodes) {
                if (node.desktop == this) {
                    list.append(node);
                }
            }

            return list;
        }


        public void focus() {
            Bspc.get_default().message_async(@"desktop $id -f");
        }

    }
}
