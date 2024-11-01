/**
 * This object can be used to query applications.
 * Multipliers can be set to customize [struct@AstalApps.Score] results
 * from queries which then are summed and sorted accordingly.
 */
public class AstalApps.Apps : Object {
    private string cache_directory;
    private string cache_file;
    private List<Application> _list;
    private HashTable<string, int> frequents { get; private set; }

    /**
     * Indicates wether hidden applications should included in queries.
     */
    public bool show_hidden { get; set; }

    /**
     * Full list of available applications.
     */
    public List<weak Application> list { owned get { return _list.copy(); } }

    /**
     * The minimum score the application has to meet in order to be included in queries.
     */
    public double min_score { get; set; default = 0; }

    /**
     * Extra multiplier to apply when matching the `name` of an application.
     * Defaults to `2`
     */
    public double name_multiplier { get; set; default = 2; }

    /**
     * Extra multiplier to apply when matching the entry of an application.
     * Defaults to `0`
     */
    public double entry_multiplier { get; set; default = 0; }

    /**
     * Extra multiplier to apply when matching the executable of an application.
     * Defaults to `0.5`
     */
    public double executable_multiplier { get; set; default = 0.5; }

    /**
     * Extra multiplier to apply when matching the description of an application.
     * Defaults to `0`
     */
    public double description_multiplier { get; set; default = 0; }

    /**
     * Extra multiplier to apply when matching the keywords of an application.
     * Defaults to `0.5`
     */
    public double keywords_multiplier { get; set; default = 0.5; }

    /**
     * Extra multiplier to apply when matching the categories of an application.
     * Defaults to `0`
     */
    public double categories_multiplier { get; set; default = 0; }

    construct {
        cache_directory = Environment.get_user_cache_dir() + "/astal";
        cache_file = cache_directory + "/apps-frequents.json";
        frequents = new HashTable<string, int>(str_hash, str_equal);

        AppInfoMonitor.get().changed.connect(() => {
            reload();
        });

        if (FileUtils.test(cache_file, FileTest.EXISTS)) {
            try {
                uint8[] content;
                File.new_for_path(cache_file).load_contents(null, out content, null);

                var parser = new Json.Parser();
                parser.load_from_data((string)content);
                var obj = parser.get_root().get_object();
                foreach (var member in obj.get_members()) {
                    var v = obj.get_member(member).get_value().get_int64();
                    frequents.set(member, (int)v);
                }
            } catch (Error err) {
                critical("cannot read cache: %s\n", err.message);
            }
        }

        reload();
    }

    private double score(string search, Application a, SearchAlgorithm alg) {
        var s = Score();
        double r = 0;

        if (alg == FUZZY) s = a.fuzzy_match(search);
        if (alg == EXACT) s = a.exact_match(search);

        r += s.name * name_multiplier;
        r += s.entry * entry_multiplier;
        r += s.executable * executable_multiplier;
        r += s.description * description_multiplier;
        r += s.keywords * keywords_multiplier;
        r += s.categories * categories_multiplier;

        return r;
    }

    /**
     * Calculate a score for an application using fuzzy matching algorithm.
     * Taking this Apps' include settings into consideration .
     */
    public double fuzzy_score(string search, Application a) {
        return score(search, a, FUZZY);
    }

    /**
     * Calculate a score for an application using exact string algorithm.
     * Taking this Apps' include settings into consideration .
     */
    public double exact_score(string search, Application a) {
        return score(search, a, EXACT);
    }

    internal List<weak Application> query(string? search = "", SearchAlgorithm alg = FUZZY) {
        if (search == null)
            search = "";

        var arr = list.copy();

        // empty search, sort by frequency
        if (search == "") {
            arr.sort_with_data((a, b) => {
                return (int)b.frequency - (int)a.frequency;
            });

            return arr;
        }

        // single character, sort by frequency and exact match
        if (search.length == 1) {
            foreach (var app in list) {
                if (score(search, app, alg) == 0)
                    arr.remove(app);
            }

            arr.sort_with_data((a, b) => {
                return (int)b.frequency - (int)a.frequency;
            });

            return arr;
        }

        // filter
        foreach (var app in list) {
            if (score(search, app, alg) < min_score)
                arr.remove(app);
        }

        // sort by score, frequency
        arr.sort_with_data((a, b) => {
            var s1 = score(search, a, alg);
            var s2 = score(search, b, alg);

            if (s1 == s2)
                return (int)b.frequency - (int)a.frequency;

            return s1 < s2 ? 1 : -1;
        });

        return arr;
    }

    /**
     * Query the `list` of applications with a fuzzy matching algorithm.
     */
    public List<weak Application> fuzzy_query(string? search = "") {
        return query(search, FUZZY);
    }

    /**
     * Query the `list` of applications with a simple string matching algorithm.
     */
    public List<weak Application> exact_query(string? search = "") {
        return query(search, EXACT);
    }

    /**
     * Reload the `list` of Applications.
     */
    public void reload() {
        var arr = AppInfo.get_all();

        _list = new List<Application>();
        foreach (var app in arr) {
            if (!show_hidden && !app.should_show())
                continue;

            var a = new Application(
                app.get_id(),
                frequents.get(app.get_id())
            );
            a.notify.connect((pspec) => {
                if (pspec.name != "frequency")
                    return;

                var f = frequents.get(app.get_id());
                frequents.set(app.get_id(), ++f);

                _list.sort((a, b) => {
                    return (int)a.frequency - (int)b.frequency;
                });
                cache();
            });
            _list.append(a);
        }

        cache();
    }

    private void cache() {
        var json = new Json.Builder().begin_object();
        foreach (string key in frequents.get_keys())
            json.set_member_name(key).add_int_value(frequents.get(key));

        try {
            if (!FileUtils.test(cache_directory, FileTest.EXISTS))
                File.new_for_path(cache_directory).make_directory_with_parents(null);

            var generator = new Json.Generator();
            generator.set_root(json.end_object().get_root());
            FileUtils.set_contents_full(cache_file, generator.to_data(null));
        } catch (Error err) {
            critical("cannot cache frequents: %s", err.message);
        }
    }
}

private enum AstalApps.SearchAlgorithm {
    EXACT,
    FUZZY,
}
