namespace AstalApps {
public class Apps : Object {
    private string cache_directory;
    private string cache_file;
    private List<Application> _list;
    private HashTable<string, int> frequents { get; private set; }

    public bool show_hidden { get; set; }
    public List<weak Application> list { owned get { return _list.copy(); } }

    public int min_score { get; set; default = 0; }

    public double name_multiplier { get; set; default = 2; }
    public double entry_multiplier { get; set; default = 1; }
    public double executable_multiplier { get; set; default = 1; }
    public double description_multiplier { get; set; default = 0.5; }

    public bool include_name { get; set; default = true; }
    public bool include_entry { get; set; default = false; }
    public bool include_executable { get; set; default = false; }
    public bool include_description { get; set; default = false; }

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

    private double score (string search, Application a, bool exact) {
        var am = exact ? a.exact_match(search) : a.fuzzy_match(search);
        double r = 0;

        if (include_name)
            r += am.name * name_multiplier;
        if (include_entry)
            r += am.entry * entry_multiplier;
        if (include_executable)
            r += am.executable * executable_multiplier;
        if (include_description)
            r += am.description * description_multiplier;

        return r;
    }

    public List<weak Application> query(string? search = "", bool exact = false) {
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
                if (score(search, app, true) == 0)
                    arr.remove(app);
            }

            arr.sort_with_data((a, b) => {
                return (int)b.frequency - (int)a.frequency;
            });

            return arr;
        }

        // filter
        foreach (var app in list) {
            if (score(search, app, exact) < min_score)
                arr.remove(app);
        }

        // sort by score, frequency
        arr.sort_with_data((a, b) => {
            var s1 = score(search, a, exact);
            var s2 = score(search, b, exact);

            if (s1 == s2)
                return (int)b.frequency - (int)a.frequency;

            return s1 < s2 ? 1 : -1;
        });

        return arr;
    }

    public List<weak Application> fuzzy_query(string? search = "") {
        return query(search, false);
    }

    public List<weak Application> exact_query(string? search = "") {
        return query(search, true);
    }

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
}
