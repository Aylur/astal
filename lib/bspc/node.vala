namespace AstalBspc     {
    public class Node : Object {
        public signal void removed ();
        public signal void moved_to (Desktop desktop);

        public string id { get; internal set; }
        public bool focused { get; internal set;}
        public bool floating { get; internal set; }
        public Desktop desktop { get; internal set; }
        public int x { get; internal set; }
        public int y { get; internal set; }
        public int width { get; internal set; }
        public int height { get; internal set; }
        public string class_name { get; private set; }
        public string instance_name { get; private set; }

        public void focus() {
            Bspc.get_default().message_async.begin(@"node $id -f", (_, task) =>
                Bspc.get_default().message_async.end(task));
        }

        public void kill() {
            Bspc.get_default().message_async.begin(@"node $id -c", (_, task) =>
                Bspc.get_default().message_async.end(task));
        }

        internal void sync(Json.Object obj) {
            //id = "0x%X".printf((uint)obj.get_int_member("id"));
            //class_name = obj.get_object_member("client").get_string_member("className");
            //instance_name = obj.get_object_member("client").get_string_member("instanceName");
            //x = (int)obj.get_array_member("at").get_int_element(0);
            //y = (int)obj.get_array_member("at").get_int_element(1);
            //width = (int)obj.get_array_member("size").get_int_element(0);
            //height = (int)obj.get_array_member("size").get_int_element(1);
        }

    }

}
