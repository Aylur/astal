/**
 * Object representing an applications .desktop file.
 */
public class AstalApps.Application : Object {
    /**
     * The underlying DesktopAppInfo.
     */
    public DesktopAppInfo app { get; construct set; }

    /**
     * The number of times [method@AstalApps.Application.launch] was called on this Application.
     */
    public int frequency { get; set; default = 0; }

    /**
     * The name of this Application.
     */
    public string name { get { return app.get_name(); } }

    /**
     * Name of the .desktop of this Application.
     */
    public string entry { get { return app.get_id(); } }

    /**
     * Description of this Application.
     */
    public string description { get { return app.get_description(); } }

    /**
     * `StartupWMClass` field from the desktop file.
     * This represents the `WM_CLASS` property of the main window of the application.
     */
    public string wm_class { get { return app.get_startup_wm_class(); } }

    /**
     * `Exec` field from the desktop file.
     * Note that if you want to launch this Application you should use the [method@AstalApps.Application.launch] method.
     */
    public string executable { owned get { return app.get_string("Exec"); } }

    /**
     * `Icon` field from the desktop file.
     * This is usually a named icon or a path to a file.
     */
    public string icon_name { owned get { return app.get_string("Icon"); } }

    /**
     * `Keywords` field from the desktop file.
     */
    public string[] keywords { owned get { return app.get_keywords(); } }

    /**
     * `Categories` field from the desktop file.
     */
    public string[] categories {
        owned get {
            if (app.get_categories() == null)
                return {};

            var categories = app.get_categories();
            var arr = categories.split(";");
            return categories.has_suffix(";") ? arr[0:arr.length-1] : arr;
        }
    }

    internal Application(string id, int? frequency = 0) {
        Object(app: new DesktopAppInfo(id));
        this.frequency = frequency;
    }

    /**
     * Get a value from the .desktop file by its key.
     */
    public string get_key(string key) {
        return app.get_string(key);
    }

    /**
     * Launches this application.
     * The launched application inherits the environment of the launching process
     */
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

    /**
     * Calculate a score for an application using fuzzy matching algorithm.
     */
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
        foreach (var keyword in keywords) {
            var s = fuzzy_match_string(term, keyword);
            if (s > score.keywords) {
                score.keywords = s;
            }
        }
        foreach (var category in categories) {
            var s = fuzzy_match_string(term, category);
            if (s > score.categories) {
                score.categories = s;
            }
        }

        return score;
    }

    /**
     * Calculate a score using exact string algorithm.
     */
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
        foreach (var keyword in keywords) {
            if (score.keywords == 0) {
                score.keywords = keyword.down().contains(term.down()) ? 1 : 0;
            }
        }
        foreach (var category in categories) {
            if (score.categories == 0) {
                score.categories = category.down().contains(term.down()) ? 1 : 0;
            }
        }

        return score;
    }

    internal Json.Node to_json() {
        var keyword_arr = new Json.Builder().begin_array();
        foreach (string keyword in keywords) {
            keyword_arr.add_string_value(keyword);
        }

        var category_arr = new Json.Builder().begin_array();
        foreach (string category in categories) {
            category_arr.add_string_value(category);
        }

        return new Json.Builder()
            .begin_object()
            .set_member_name("name").add_string_value(name)
            .set_member_name("entry").add_string_value(entry)
            .set_member_name("executable").add_string_value(executable)
            .set_member_name("description").add_string_value(description)
            .set_member_name("icon_name").add_string_value(icon_name)
            .set_member_name("frequency").add_int_value(frequency)
            .set_member_name("keywords").add_value(keyword_arr.end_array().get_root())
            .set_member_name("categories").add_value(category_arr.end_array().get_root())
            .end_object()
            .get_root();
    }
}

public struct AstalApps.Score {
    int name;
    int entry;
    int executable;
    int description;
    int keywords;
    int categories;
}
