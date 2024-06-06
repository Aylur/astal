namespace AstalApps {
public class Application : Object {
    public DesktopAppInfo app { get; construct set; }
    public int frequency { get; set; default = 0; }
    public string name { get { return app.get_name(); } }
    public string entry { get { return app.get_id(); } }
    public string description { get { return app.get_description(); } }
    public string wm_class { get { return app.get_startup_wm_class(); } }
    public string executable { owned get { return app.get_string("Exec"); } }
    public string icon_name { owned get { return app.get_string("Icon"); } }

    internal Application(string id, int? frequency = 0) {
        Object(app: new DesktopAppInfo(id));
        this.frequency = frequency;
    }

    public string get_key(string key) {
        return app.get_string(key);
    }

    public bool launch() {
        try {
            var s = app.launch(null, null);
            ++frequency;
            return s;
        } catch (Error err) {
            critical(err.message);
            return false;
        }
    }

    public Score fuzzy_match(string term) {
        var score = Score();
        if (name != null)
            score.name = levenshtein(term, name);
        if (entry != null)
            score.entry = levenshtein(term, entry);
        if (executable != null)
            score.executable = levenshtein(term, executable);
        if (description != null)
            score.description = levenshtein(term, description);

        return score;
    }

    public Score exact_match(string term) {
        var score = Score();
        if (name != null)
            score.name = name.down().contains(term.down()) ? 1 : 0;
        if (entry != null)
            score.entry = entry.down().contains(term.down()) ? 1 : 0;
        if (executable != null)
            score.executable = executable.down().contains(term.down()) ? 1 : 0;
        if (description != null)
            score.description = description.down().contains(term.down()) ? 1 : 0;

        return score;
    }

    internal Json.Node to_json() {
        return new Json.Builder()
            .begin_object()
            .set_member_name("name").add_string_value(name)
            .set_member_name("entry").add_string_value(entry)
            .set_member_name("executable").add_string_value(executable)
            .set_member_name("description").add_string_value(description)
            .set_member_name("icon_name").add_string_value(icon_name)
            .set_member_name("frequency").add_int_value(frequency)
            .end_object()
            .get_root();
    }
}

int min3(int a, int b, int c) {
    return (a < b) ? ((a < c) ? a : c) : ((b < c) ? b : c);
}

double levenshtein(string s1, string s2) {
    int len1 = s1.length;
    int len2 = s2.length;

    int[, ] d = new int[len1 + 1, len2 + 1];

    for (int i = 0; i <= len1; i++) {
        d[i, 0] = i;
    }
    for (int j = 0; j <= len2; j++) {
        d[0, j] = j;
    }

    for (int i = 1; i <= len1; i++) {
        for (int j = 1; j <= len2; j++) {
            int cost = (s1[i - 1] == s2[j - 1]) ? 0 : 1;
            d[i, j] = min3(
                d[i - 1, j] + 1,       // deletion
                d[i, j - 1] + 1,       // insertion
                d[i - 1, j - 1] + cost // substitution
            );
        }
    }

    var distance = d[len1, len2];
    int max_len = len1 > len2 ? len1 : len2;

    if (max_len == 0) {
        return 1.0;
    }

    return 1.0 - ((double)distance / max_len);
}

public struct Score {
    double name;
    double entry;
    double executable;
    double description;
}
}
