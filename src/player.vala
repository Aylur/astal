namespace AstalMpris {
public class Player : Object {
    private static string COVER_CACHE = Environment.get_user_cache_dir() + "/astal/mpris";

    private IPlayer proxy;

    public signal void appeared () { available = true; }
    public signal void closed () { available = false; }

    // identifiers
    public string bus_name { owned get; construct set; }
    public bool available { get; private set; }

    // periodically notify position
    private uint pollid;

    // mpris
    public void raise() {
        try { proxy.raise(); } catch (Error error) { critical(error.message); }
    }

    public void quit() {
        try { proxy.quit(); } catch (Error error) { critical(error.message); }
    }

    public bool can_quit { get; private set; }
    public bool fullscreen { get; private set; }
    public bool can_set_fullscreen { get; private set; }
    public bool can_raise { get; private set; }
    public bool has_track_list { get; private set; }
    public string identity { owned get; private set; }
    public string entry { owned get; private set; }
    public string[] supported_uri_schemas { owned get; private set; }
    public string[] supported_mime_types { owned get; private set; }

    public void toggle_fullscreen() {
        if (!can_set_fullscreen)
            critical("can not set fullscreen on " + bus_name);

        proxy.fullscreen = !fullscreen;
    }

    // player
    public void next() {
        try { proxy.next(); } catch (Error error) { critical(error.message); }
    }

    public void previous() {
        try { proxy.previous(); } catch (Error error) { critical(error.message); }
    }

    public void pause() {
        try { proxy.pause(); } catch (Error error) { critical(error.message); }
    }

    public void play_pause() {
        try { proxy.play_pause(); } catch (Error error) { critical(error.message); }
    }

    public void stop() {
        try { proxy.stop(); } catch (Error error) { critical(error.message); }
    }

    public void play() {
        try { proxy.play(); } catch (Error error) { critical(error.message); }
    }

    public void open_uri(string uri) {
        try { proxy.open_uri(uri); } catch (Error error) { critical(error.message); }
    }

    public void loop() {
        switch (loop_status) {
            case Loop.NONE:
                loop_status = Loop.TRACK;
                break;
            case Loop.TRACK:
                loop_status = Loop.PLAYLIST;
                break;
            case Loop.PLAYLIST:
                loop_status = Loop.NONE;
                break;
            default:
                break;
        }
    }

    public void shuffle() {
        shuffle_status = shuffle_status == Shuffle.ON
            ? Shuffle.OFF
            : Shuffle.ON;
    }

    public signal void seeked (int64 position);

    public double _get_position() {
        try {
            var reply = proxy.call_sync(
                "org.freedesktop.DBus.Properties.Get",
                new Variant("(ss)",
                    "org.mpris.MediaPlayer2.Player",
                    "Position"
                ),
                DBusCallFlags.NONE,
                -1,
                null
            );

            var body = reply.get_child_value(0);
            if (body.classify() == Variant.Class.STRING) {
                return -1; // Position not supported
            }

            return (double)body.get_variant().get_int64() / 1000000;
        } catch (Error err) {
            return -1;
        }
    }

    private void _set_position(double pos) {
        try {
            proxy.set_position((ObjectPath)trackid, (int64)(pos * 1000000));
        } catch (Error error) {
            critical(error.message);
        }
    }

    private Loop _loop_status = Loop.UNSUPPORTED;
    private double _rate;
    private Shuffle _shuffle_status = Shuffle.UNSUPPORTED;
    private double _volume = -1;

    public Loop loop_status {
        get { return _loop_status; }
        set { proxy.loop_status = value.to_string(); }
    }

    public double rate {
        get { return _rate; }
        set { proxy.rate = value; }
    }

    public Shuffle shuffle_status {
        get { return _shuffle_status; }
        set { proxy.shuffle = value == Shuffle.ON; }
    }

    public double volume {
        get { return _volume; }
        set { proxy.volume = value; }
    }

    public double position {
        get { return _get_position(); }
        set { _set_position(value); }
    }

    public PlaybackStatus playback_status { get; private set; }
    public double minimum_rate { get; private set; }
    public double maximum_rate { get; private set; }
    public bool can_go_next { get; private set; }
    public bool can_go_previous { get; private set; }
    public bool can_play { get; private set; }
    public bool can_pause { get; private set; }
    public bool can_seek { get; private set; }
    public bool can_control { get; private set; }

    // metadata
    [CCode (notify = false)]
    public HashTable<string,Variant> metadata { owned get; private set; }

    public string trackid { owned get; private set; }
    public double length { get; private set; }
    public string art_url { owned get; private set; }

    public string album { owned get; private set; }
    public string album_artist { owned get; private set; }
    public string artist { owned get; private set; }
    public string lyrics { owned get; private set; }
    public string title { owned get; private set; }
    public string composer { owned get; private set; }
    public string comments { owned get; private set; }

    // cached cover art
    public string cover_art { owned get; private set; }

    public Player(string name) {
        Object(bus_name: name.has_prefix("org.mpris.MediaPlayer2.")
            ? name : "org.mpris.MediaPlayer2." + name);
    }

    private void sync() {
        // mpris
        can_quit = proxy.can_quit;
        fullscreen = proxy.fullscreen;
        can_set_fullscreen = proxy.can_set_fullscreen;
        can_raise = proxy.can_raise;
        has_track_list = proxy.has_track_list;
        identity = proxy.identity;
        entry = proxy.desktop_entry;
        supported_uri_schemas = proxy.supported_uri_schemas;
        supported_mime_types = proxy.supported_mime_types;

        if (position >= 0)
            notify_property("position");

        // LoopStatus and Shuffle are optional props
        var props = proxy.get_all("org.mpris.MediaPlayer2.Player");

        // player
        if (props != null && props.get("LoopStatus") != null) {
            if (loop_status != Loop.from_string(proxy.loop_status)) {
                _loop_status = Loop.from_string(proxy.loop_status);
                notify_property("loop-status");
            }
        }

        if (rate != proxy.rate) {
            _rate = proxy.rate;
            notify_property("rate");
        }

        if (props != null && props.get("Shuffle") != null) {
            if (shuffle_status != Shuffle.from_bool(proxy.shuffle)) {
                _shuffle_status = Shuffle.from_bool(proxy.shuffle);
                notify_property("shuffle-status");
            }
        }

        if (volume != proxy.volume) {
            _volume = proxy.volume;
            notify_property("volume");
        }

        playback_status = PlaybackStatus.from_string(proxy.playback_status);
        minimum_rate = proxy.minimum_rate;
        maximum_rate = proxy.maximum_rate;
        can_go_next = proxy.can_go_next;
        can_go_previous = proxy.can_go_previous;
        can_play = proxy.can_play;
        can_pause = proxy.can_pause;
        can_seek = proxy.can_seek;
        can_control = proxy.can_control;

        // metadata
        metadata = proxy.metadata;
        if (metadata != null) {
            if (metadata.get("mpris:length") != null)
                length = (double)metadata.get("mpris:length").get_uint64() / 1000000;
            else
                length = -1;

            trackid = get_str("mpris:trackid");
            art_url = get_str("mpris:artUrl");
            album = get_str("xesam:album");
            lyrics = get_str("xesam:asText");
            title = get_str("xesam:title");
            album_artist = join_strv("xesam:albumArtist", ", ");
            artist = join_strv("xesam:artist", ", ");
            comments = join_strv("xesam:comments", "\n");
            composer = join_strv("xesam:composer", ", ");
            cache_cover.begin((_, res) => cache_cover.end(res));
            notify_property("metadata");
        }
    }

    private async void cache_cover() {
        if (art_url == null || art_url == "")
            return;

        var file = File.new_for_uri(art_url);
        if (file.get_path() != null) {
            cover_art = file.get_path();
            return;
        }

        var path = COVER_CACHE + "/" + Checksum.compute_for_string(ChecksumType.SHA1, art_url, -1);
        if (FileUtils.test(path, FileTest.EXISTS)) {
            cover_art = path;
            return;
        }

        try {
            if (!FileUtils.test(COVER_CACHE, FileTest.IS_DIR))
                File.new_for_path(COVER_CACHE).make_directory_with_parents(null);

            file.copy_async.begin(
                File.new_for_path(path),
                FileCopyFlags.OVERWRITE,
                Priority.DEFAULT,
                null,
                null,
                (_, res) => {
                    try {
                        file.copy_async.end(res);
                        cover_art = path;
                    } catch (Error err) {
                        critical("Failed to cache cover art with url \"%s\": %s", art_url, err.message);
                    }
                }
            );
        } catch (Error err) {
            critical(err.message);
        }
    }

    public Variant? get_meta(string key) {
        return metadata.lookup(key);
    }

    private string get_str(string key) {
        if (metadata.get(key) == null)
            return "";

        var str = metadata.get(key).get_string(null);
        return str == null ? "" : str;
    }

    private string? join_strv(string key, string sep) {
        if (metadata.get(key) == null)
            return null;

        var arr = metadata.get(key).get_strv();
        if (arr.length == 0)
            return null;

        var builder = new StringBuilder();
        for (var i = 0; i < arr.length; ++i) {
            builder.append(arr[i]);
            if (i + 1 < arr.length)
                builder.append(sep);
        }

        return builder.str;
    }

    construct {
        try {
            try_proxy();
            sync();
        } catch (Error error) {
            critical(error.message);
        }
    }

    public void try_proxy() throws Error {
        if (proxy != null)
            return;

        proxy = Bus.get_proxy_sync(
            BusType.SESSION,
            bus_name,
            "/org/mpris/MediaPlayer2"
        );

        if (proxy.g_name_owner != null)
            appeared();

        proxy.notify["g-name-owner"].connect(() => {
            if (proxy.g_name_owner != null)
                appeared();
            else
                closed();
        });

        proxy.g_properties_changed.connect(sync);

        pollid = Timeout.add_seconds(1, () => {
            if (!available)
                return Source.CONTINUE;

            if (position >= 0) {
                notify_property("position");
            }
            return Source.CONTINUE;
        }, Priority.DEFAULT);
    }

    ~Player() {
        Source.remove(pollid);
    }
}

public enum PlaybackStatus {
    PLAYING = 0,
    PAUSED,
    STOPPED;

    public static PlaybackStatus from_string(string? str) {
        switch (str) {
            case "Playing":
                return PLAYING;
            case "Paused":
                return PAUSED;
            case "Stopped":
            default:
                return STOPPED;
        }
    }

    public string to_string() {
        switch (this) {
            case PLAYING:
                return "Playing";
            case PAUSED:
                return "Paused";
            case STOPPED:
            default:
                return "Stopped";
        }
    }
}

public enum Loop {
    UNSUPPORTED = 0,
    NONE,
    TRACK,
    PLAYLIST;

    public static Loop from_string(string? str) {
        switch (str) {
            case "None":
                return NONE;
            case "Track":
                return TRACK;
            case "Playlist":
                return PLAYLIST;
            default:
                return UNSUPPORTED;
        }
    }

    public string? to_string() {
        switch (this) {
            case NONE:
                return "None";
            case TRACK:
                return "Track";
            case PLAYLIST:
                return "Playlist";
            default:
                return "Unsupported";
        }
    }
}

public enum Shuffle {
    UNSUPPORTED = 0,
    ON,
    OFF;

    public static Shuffle from_bool(bool b) {
        return b ? Shuffle.ON : Shuffle.OFF;
    }

    public string? to_string() {
        switch (this) {
            case OFF:
                return "Off";
            case ON:
                return "On";
            default:
                return "Unsupported";
        }
    }
}
}
