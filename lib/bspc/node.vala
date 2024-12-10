namespace AstalBspc     {
    public class Node : Object {
        public signal void removed ();
        public signal void moved_to (Desktop desktop);

        public string id { get; private set; }
        public bool focused { get; private set;}
        public bool floating { get; private set; }
        public Desktop desktop { get; private set; }
        public int x { get; private set; }
        public int y { get; private set; }
        public int width { get; private set; }
        public int height { get; private set; }
        public string class_name { get; private set; }
        public string instance_name { get; private set; }

        public void focus() {
            Bspc.get_default().message_async(@"node $id -f");
        }

        public void kill() {
            Bspc.get_default().message_async(@"node $id -c");
        }

        internal void sync(Json.Object obj) {
            var bspc = Bspc.get_default();
            id = "0x%X".printf((uint)obj.get_int_member("id"));
    
            class_name = obj.get_object_member("client").get_string_member("className");
            instance_name = obj.get_object_member("client").get_string_member("instanceName");
            //x = (int)obj.get_array_member("at").get_int_element(0);
            //y = (int)obj.get_array_member("at").get_int_element(1);
            //width = (int)obj.get_array_member("size").get_int_element(0);
            //height = (int)obj.get_array_member("size").get_int_element(1);
        }

    }

}
