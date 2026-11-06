namespace AstalHyprland {
// Decode workspace identities without treating absent JSON members as zero.
internal class WorkspaceIdentity : Object {
    internal static int? take_handle(ref int next) {
        if (next >= 0) return null;
        return next++;
    }

    public string address;
    public int? number;

    public static string? string_member(Json.Object obj, string member) {
        var node = obj.get_member(member);
        if (node == null || node.get_node_type() != Json.NodeType.VALUE ||
            node.get_value_type() != typeof(string)) return null;
        return node.get_string();
    }

    public static Json.Object? object_member(Json.Object obj, string member) {
        var node = obj.get_member(member);
        if (node == null || node.get_node_type() != Json.NodeType.OBJECT) return null;
        return node.get_object();
    }

    public static WorkspaceIdentity? from_json(Json.Object? obj) {
        if (obj == null) return null;
        int? number = null;
        var node = obj.get_member("id");
        if (node != null) {
            if (node.get_node_type() != Json.NodeType.VALUE ||
                node.get_value_type() != typeof(int64)) return null;
            var value = node.get_int();
            if (value < int.MIN || value > int.MAX) return null;
            number = (int)value;
        }
        var address = string_member(obj, "address");
        if (address == null || address == "") {
            // Before Hyprland 0.57 all workspaces had a numeric identity.
            if (number == null || number == 0) return null;
            address = ((int)number).to_string();
        }
        return new WorkspaceIdentity() { address = address, number = number };
    }
}
}
