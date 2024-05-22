namespace AstalApps {
public class Application : Object {
    public DesktopAppInfo app { get; construct set; }
    public uint frequency { get; set; }
    public string name { get { return app.get_name(); } }
    public string entry { get { return app.get_id(); } }
    public string description { get { return app.get_description(); } }
    public string wm_class { get { return app.get_startup_wm_class(); } }
    public string executable { owned get { return app.get_string("Exec"); } }
    public string icon_name { owned get { return app.get_string("Icon"); } }

    internal Application(string id, uint? frequency = 0) {
        Object(app: new DesktopAppInfo(id), frequency: frequency);
    }

    public string get_key(string key) {
        return app.get_string(key);
    }

    public void launch() {
        try {
            app.launch(null, null);
            ++frequency;
        } catch (Error err) {
            critical(err.message);
        }
    }

    public double match(string term) {
        int n = 0;
        double score = 0;
        if (name != null) {
            score += jaro_winkler_similarity(term, name);
            ++n;
        }
        if (entry != null) {
            score += jaro_winkler_similarity(term, entry);
            ++n;
        }
        if (executable != null) {
            score += jaro_winkler_similarity(term, executable);
            ++n;
        }
        if (description != null) {
            score += levenshtein_distance(term, description);
            ++n;
        }
        return n > 0 ? score / n : 0;
    }

    internal Json.Node to_json() {
        return new Json.Builder()
            .begin_object()
            .set_member_name("name").add_string_value(name)
            .set_member_name("entry").add_string_value(entry)
            .set_member_name("executable").add_string_value(executable)
            .set_member_name("description").add_string_value(description)
            .set_member_name("icon_name").add_string_value(icon_name)
            .end_object()
            .get_root();
    }
}
}
