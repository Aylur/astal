namespace AstalBspc {
    public class ReportFormat {
        public string? M; // Focused monitor
        public string? m; // Unfocused monitor
        public string? O; // Occupied focused desktop
        public string? o; // Occupied unfocused desktop
        public string? F; // Free focused desktop
        public string? f; // Free unfocused desktop
        public string? U; // Urgent focused desktop
        public string? u; // Urgent unfocused desktop
        public string? L; // Layout of focused desktop
        public string? T; // State of focused node
        public string? G; // Active flags of focused node

        public ReportFormat(string[] parts) {
            foreach (string part in parts) {
                if (part.has_prefix("M")) {
                    M = part.substring(1);
                } else if (part.has_prefix("m")) {
                    m = part.substring(1);
                } else if (part.has_prefix("O")) {
                    O = part.substring(1);
                } else if (part.has_prefix("o")) {
                    o = part.substring(1);
                } else if (part.has_prefix("F")) {
                    F = part.substring(1);
                } else if (part.has_prefix("f")) {
                    f = part.substring(1);
                } else if (part.has_prefix("U")) {
                    U = part.substring(1);
                } else if (part.has_prefix("u")) {
                    u = part.substring(1);
                } else if (part.has_prefix("L")) {
                    L = part.substring(1);
                } else if (part.has_prefix("T")) {
                    T = part.substring(1);
                } else if (part.has_prefix("G")) {
                    G = part.substring(1);
                }
            }
        }
    }
    public class Padding : Object {
        public int top { get; internal set; }
        public int bottom { get; internal set; }
        public int left { get; internal set; }
        public int right { get; internal set; }

        public Padding(Json.Object obj) {
            top = (int)obj.get_int_member("top");
            bottom = (int)obj.get_int_member("bottom");
            left = (int)obj.get_int_member("left");
            right = (int)obj.get_int_member("right");
        }
    }
}
