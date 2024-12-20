namespace AstalBspc {
    public class ReportFormat {
        public string? M;
        public string? m;
        public string? O;
        public string? o;
        public string? F;
        public string? f;
        public string? U;
        public string? u;
        public string? L;
        public string? T;
        public string? G;

        public ReportFormat(string[] parts) {
            M = (parts.length > 0) ? parts[0] : null;
            m = (parts.length > 1) ? parts[1] : null;
            O = (parts.length > 2) ? parts[2] : null;
            o = (parts.length > 3) ? parts[3] : null;
            F = (parts.length > 4) ? parts[4] : null;
            f = (parts.length > 5) ? parts[5] : null;
            U = (parts.length > 6) ? parts[6] : null;
            u = (parts.length > 7) ? parts[7] : null;
            L = (parts.length > 8) ? parts[8] : null;
            T = (parts.length > 9) ? parts[9] : null;
            G = (parts.length > 10) ? parts[10] : null;
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
