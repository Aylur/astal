namespace AstalBspc {
    public class ReportFormat  {
        public string M;
        public string m;
        public string O;
        public string o;
        public string F;
        public string f;
        public string U;
        public string u;
        public string L;
        public string T;
        public string G;

        public ReportFormat(string[] parts) {
            if (parts.length >= 11) {
                M = parts[0];
                m = parts[1];
                O = parts[2];
                o = parts[3];
                F = parts[4];
                f = parts[5];
                U = parts[6];
                u = parts[7];
                L = parts[8];
                T = parts[9];
                G = parts[10];
            } else {
                M = m = O = o = F = f = U = u = L = T = G = "Error";
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
