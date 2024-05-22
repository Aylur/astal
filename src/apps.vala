namespace AstalApps {
public class Apps : Object {
    private string cache_directory;
    private string cache_file;
    private List<Application> _list;
    private HashTable<string, uint> frequents { get; private set; }

    public bool show_hidden { get; set; }
    public List<weak Application> list { owned get { return _list.copy(); } }

    construct {
        cache_directory = Environment.get_user_cache_dir() + "/astal";
        cache_file = cache_directory + "/apps-frequents.json";
        frequents = new HashTable<string, uint>(str_hash, str_equal);

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
                    frequents.set(member, (uint)v);
                }
            } catch (Error err) {
                critical("cannot read cache: %s\n", err.message);
            }
        }

        reload();
    }

    private int compare (string search, Application a, Application b) {
        var s1 = a.match(search) * 100;
        var s2 = b.match(search) * 100;
        return (int)s2 - (int)s1;
    }

    public List<weak Application> query(string? search = "") {
        if (search == null)
            return list.copy();

        var arr = list.copy();
        arr.sort_with_data((a, b) => {
            return compare(search, a, b);
        });
        return arr;
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
        foreach (string key in frequents.get_keys()) {
            uint v = frequents.get(key);
            json.set_member_name(key).add_int_value((int)v);
        }

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
