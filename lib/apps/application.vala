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
            score.name = fuzzy_match_string(term, name);
        if (entry != null)
            score.entry = fuzzy_match_string(term, entry);
        if (executable != null)
            score.executable = fuzzy_match_string(term, executable);
        if (description != null)
            score.description = fuzzy_match_string(term, description);

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

public struct Score {
    int name;
    int entry;
    int executable;
    int description;
}
}
