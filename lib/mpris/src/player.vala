/**
 * Object which tracks players through their mpris DBus interface.
 * The most simple way is to use [class@AstalMpris.Mpris] which tracks every player,
 * but [class@AstalMpris.Player] can be constructed for dedicated players too.
 */
public class AstalMpris.Player : Object {
    private static string COVER_CACHE = Environment.get_user_cache_dir() + "/astal/mpris";

    private string _busname = "";
    private PlayerProxy? proxy;
    private uint pollid = 0; // periodically notify position

    /**
     * Full dbus nama of this player.
     */
    public string bus_name {
        get { return _busname; }
        private set {
            _busname = value;
            init_proxy.begin(value, (_, res) => {
                try {
                    init_proxy.end(res);
                } catch (Error error) {
                    critical(error.message);
                }
            });
        }
    }

    /**
     * Indicates if [property@AstalMpris.Player:bus_name] is available on dbus.
     */
    public bool available { get; private set; default = false; }

    private bool check_available() {
        if (!available) {
            warning(@"$bus_name is not available \n");
            return true;
        }

        return false;
    }

    /**
     * Brings the player's user interface to the front using any appropriate mechanism available.
     * The media player may be unable to control how its user interface is displayed,
     * or it may not have a graphical user interface at all.
     * In this case, the [property@AstalMpris.Player:can_raise] is `false` and this method does nothing.
     */
    public void raise() {
        if (check_available()) return;
        proxy.raise.begin((_, res) => {
            try {
                proxy.raise.end(res);
            } catch (Error err) {
                critical(err.message);
            }
        });
    }

    /**
     * Causes the media player to stop running.
     * The media player may refuse to allow clients to shut it down.
     * In this case, the [property@AstalMpris.Player:can_quit] property is false and this method does nothing.
     */
    public void quit() {
        if (check_available()) return;
        proxy.quit.begin((_, res) => {
            try {
                proxy.quit.end(res);
            } catch (Error err) {
                critical(err.message);
            }
        });
    }

    /**
     * Indicates if [method@AstalMpris.Player.quit] has any effect.
     */
    public bool can_quit { get; private set; default = false; }

    /**
     * Indicates if the player is occupying the fullscreen. This is typically used for videos.
     * Use [method@AstalMpris.Player.toggle_fullscreen] to toggle fullscreen state.
     */
    public bool fullscreen { get; private set; default = false; }

    /**
     * Indicates if [method@AstalMpris.Player.toggle_fullscreen] has any effect.
     */
    public bool can_set_fullscreen { get; private set; default = false; }

    /**
     * Indicates if [method@AstalMpris.Player.raise] has any effect.
     */
    public bool can_raise { get; private set; default = false; }

    // TODO: Tracklist interface
    // public bool has_track_list { get; private set; }

    /**
     * A human friendly name to identify the player.
     */
    public string identity { get; private set; default = ""; }

    /**
     * The base name of a .desktop file
     */
    public string entry { get; private set; default = ""; }

    /**
     * The URI schemes supported by the media player.
     * This can be viewed as protocols supported by the player in almost all cases.
     * Almost every media player will include support for the "file" scheme.
     * Other common schemes are "http" and "rtsp".
     */
    public string[] supported_uri_schemes { get; private set; default = {}; }

    /**
     * The mime-types supported by the player.
     */
    public string[] supported_mime_types { get; private set; default = {}; }

    /**
     * Toggle [property@AstalMpris.Player:fullscreen] state.
     */
    public void toggle_fullscreen() {
        if (check_available()) return;
        if (!can_set_fullscreen) critical(@"can not set fullscreen on $bus_name");

        proxy.set.begin(
            MediaPlayerProxy.NAME,
            "Fullscreen",
            new Variant.boolean(!fullscreen),
            proxy_set_end
        );
    }

    /**
     * Skips to the next track in the tracklist.
     * If there is no next track (and endless playback and track repeat are both off), stop playback.
     * If [property@AstalMpris.Player:can_go_next] is `false` this method has no effect.
     */
    public void next() {
        if (check_available()) return;
        proxy.next.begin((_, res) => {
            try {
                proxy.next.end(res);
            } catch (Error error) {
                critical(error.message);
            }
        });
    }

    /**
     * Skips to the previous track in the tracklist.
     * If there is no previous track (and endless playback and track repeat are both off), stop playback.
     * If [property@AstalMpris.Player:can_go_previous] is `false` this method has no effect.
     */
    public void previous() {
        if (check_available()) return;
        proxy.previous.begin((_, res) => {
            try {
                proxy.previous.end(res);
            } catch (Error error) {
                critical(error.message);
            }
        });
    }

    /**
     * Pauses playback.
     * If playback is already paused, this has no effect.
     * If [property@AstalMpris.Player:can_pause] is `false` this method has no effect.
     */
    public void pause() {
        if (check_available()) return;
        proxy.pause.begin((_, res) => {
            try {
                proxy.pause.end(res);
            } catch (Error error) {
                critical(error.message);
            }
        });
    }

    /**
     * Pauses playback.
     * If playback is already paused, resumes playback.
     * If playback is stopped, starts playback.
     */
    public void play_pause() {
        if (check_available()) return;
        proxy.play_pause.begin((_, res) => {
            try {
                proxy.play_pause.end(res);
            } catch (Error error) {
                critical(error.message);
            }
        });
    }

    /**
     * Stops playback.
     * If playback is already stopped, this has no effect.
     * If [property@AstalMpris.Player:can_control] is `false` this method has no effect.
     */
    public void stop() {
        if (check_available()) return;
        proxy.stop.begin((_, res) => {
            try {
                proxy.stop.end(res);
            } catch (Error error) {
                critical(error.message);
            }
        });
    }

    /**
     * Starts or resumes playback.
     * If already playing, this has no effect.
     * If paused, playback resumes from the current position.
     * If [property@AstalMpris.Player:can_play] is `false` this method has no effect.
     */
    public void play() {
        if (check_available()) return;
        proxy.play.begin((_, res) => {
            try {
                proxy.play.end(res);
            } catch (Error error) {
                critical(error.message);
            }
        });
    }

    /**
     * uri scheme should be an element of [property@AstalMpris.Player:supported_uri_schemes]
     * and the mime-type should match one of the elements of [property@AstalMpris.Player:supported_mime_types].
     *
     * @param uri Uri of the track to load.
     */
    public void open_uri(string uri) {
        if (check_available()) return;
        proxy.open_uri.begin(uri, (_, res) => {
            try {
                proxy.open_uri.end(res);
            } catch (Error error) {
                critical(error.message);
            }
        });
    }

    /**
     * Change [property@AstalMpris.Player:loop_status] from none to track,
     * from track to playlist, from playlist to none.
     */
    public void loop() {
        if (check_available()) return;
        if (loop_status == Loop.UNSUPPORTED) {
            critical(@"loop is unsupported by $bus_name");
            return;
        }

        if (loop_status == Loop.NONE) {
            loop_status = Loop.TRACK;
            return;
        }
        if (loop_status == Loop.TRACK) {
            loop_status = Loop.PLAYLIST;
            return;
        }
        if (loop_status == Loop.PLAYLIST) {
            loop_status = Loop.NONE;
            return;
        }
    }

    /**
     * Toggle [property@AstalMpris.Player:shuffle_status].
     */
    public void shuffle() {
        if (check_available()) return;
        if (shuffle_status == Shuffle.UNSUPPORTED) {
            critical(@"shuffle is unsupported by $bus_name");
            return;
        }

        shuffle_status = (shuffle_status == Shuffle.ON)
            ? Shuffle.OFF
            : Shuffle.ON;
    }

    /**
     * The current loop/repeat status.
     */
    public Loop loop_status {
        get { return _loop_status; }
        set { _set_loop_status(value); }
    }

    private Loop _loop_status = Loop.UNSUPPORTED;
    private void _set_loop_status(Loop loop) {
        if (check_available()) return;
        if (loop != Loop.UNSUPPORTED) {
            proxy.set.begin(
                PlayerProxy.NAME,
                "LoopStatus",
                new Variant.string(loop.to_string()),
                proxy_set_end
            );
        }
    }

    /**
     * The current shuffle status.
     */
    public Shuffle shuffle_status {
        get { return _shuffle_status; }
        set { _set_shuffle_status(value); }
    }

    private Shuffle _shuffle_status = Shuffle.UNSUPPORTED;
    private void _set_shuffle_status(Shuffle status) {
        if (check_available()) return;
        proxy.set.begin(
            PlayerProxy.NAME,
            "Shuffle",
            new Variant.boolean(status == Shuffle.ON),
            proxy_set_end
        );
    }

    /**
     * The current playback rate.
     */
    public double rate {
        get { return _rate; }
        set { _set_rate(value); }
    }

    private double _rate = 0;
    private void _set_rate(double rate) {
        if (check_available()) return;
        proxy.set.begin(
            PlayerProxy.NAME,
            "Rate",
            new Variant.double(rate),
            proxy_set_end
        );
    }

    /**
     * The current volume level between 0 and 1 or -1 when it is unsupported.
     */
    public double volume {
        get { return _volume; }
        set { _set_volume(value); }
    }

    private double _volume = -1;
    private void _set_volume(double volume) {
        if (check_available()) return;
        proxy.set.begin(
            PlayerProxy.NAME,
            "Volume",
            new Variant.double(volume),
            proxy_set_end
        );
    }

    /**
     * The current position of the track in seconds or -1 when it is unsupported.
     * To get a progress percentage simply divide this with [property@AstalMpris.Player:length].
     */
    public double position {
        get { return _position; }
        set { _set_position(value); }
    }

    private double _position = -1;
    private void _set_position(double pos) {
        if (check_available()) return;
        proxy.set_position.begin((ObjectPath)trackid, (int64)(pos * 1000000), (_, res) => {
            try {
                proxy.set_position.end(res);
            } catch (Error error) {
                critical(error.message);
            }
        });
    }

    /**
     * The current playback status.
     */
    public PlaybackStatus playback_status { get; private set; }

    /**
     * The minimum value which the [property@AstalMpris.Player:rate] can take.
     */
    public double minimum_rate { get; private set; }

    /**
     * The maximum value which the [property@AstalMpris.Player:rate] can take.
     */
    public double maximum_rate { get; private set; }

    /**
     * Indicates if invoking [method@AstalMpris.Player.next] has effect.
     */
    public bool can_go_next { get; private set; }

    /**
     * Indicates if invoking [method@AstalMpris.Player.previous] has effect.
     */
    public bool can_go_previous { get; private set; }

    /**
     * Indicates if invoking [method@AstalMpris.Player.play] has effect.
     */
    public bool can_play { get; private set; }

    /**
     * Indicates if invoking [method@AstalMpris.Player.pause] has effect.
     */
    public bool can_pause { get; private set; }

    /**
     * Indicates if setting [property@AstalMpris.Player:position] has effect.
     */
    public bool can_seek { get; private set; }

    /**
     * Indicates if the player can be controlled with
     * methods such as [method@AstalMpris.Player.play_pause].
     */
    public bool can_control { get; private set; }

    /**
     * Metadata of this player.
     */
    public Variant metadata {
        get; private set;
        default = new Variant.array(new VariantType("{sv}"), {});
    }

    /**
     * Currently playing track's id.
     */
    public string trackid { get; private set; default = ""; }

    /**
     * Length of the currently playing track in seconds.
     */
    public double length { get; private set; }

    /**
     * The location of an image representing the track or album.
     * You might prefer using [property@AstalMpris.Player:cover_art].
     */
    public string art_url { get; private set; default = ""; }

    /**
     * Title of the currently playing album.
     */
    public string album { get; private set; default = ""; }

    /**
     * Artists of the currently playing album.
     */
    public string album_artist { get; private set; default = ""; }

    /**
     * Artists of the currently playing track.
     */
    public string artist { get; private set; default = ""; }

    /**
     * Lyrics of the currently playing track.
     */
    public string lyrics { get; private set; default = ""; }

    /**
     * Title of the currently playing track.
     */
    public string title { get; private set; default = ""; }

    /**
     * Composers of the currently playing track.
     */
    public string composer { get; private set; default = ""; }

    /**
     * Comments of the currently playing track.
     */
    public string comments { get; private set; default = ""; }

    /**
     * Path of the cached [property@AstalMpris.Player:art_url].
     */
    public string cover_art { get; private set; default = ""; }

    /**
     * Lookup a key from [property@AstalMpris.Player:metadata].
     * This method is useful for languages that fail to introspect hashtables.
     */
    public Variant? get_meta(string key) {
        var iter = metadata.iterator();

        Variant? val = null;
        string? _key = null;

        while (iter.next("{sv}", out _key, out val)) {
            if (_key == key) return val;
        }

        return null;
    }

    /**
     * Construct a Player that tracks a dbus name. For example "org.mpris.MediaPlayer2.spotify".
     * The "org.mpris.MediaPlayer2." prefix can be omitted so simply "spotify" would mean the same.
     * [property@AstalMpris.Player:available] indicates whether the player is actually running or not.
     *
     * @param name dbus name of the player.
     */
    public Player(string name) {
        bus_name = name.has_prefix("org.mpris.MediaPlayer2.")
            ? name : @"org.mpris.MediaPlayer2.$name";
    }

    internal async Player.async(string name) throws Error {
        _busname = name;
        yield init_proxy(name);
    }

    private void sync(Variant props) {
        var _can_quit = props.lookup_value("CanQuit", VariantType.BOOLEAN);
        if (_can_quit != null) can_quit = _can_quit.get_boolean();

        var _fullscreen = props.lookup_value("Fullscreen", VariantType.BOOLEAN);
        if (_fullscreen != null) fullscreen = _fullscreen.get_boolean();

        var _can_fullscreen = props.lookup_value("CanSetFullscreen", VariantType.BOOLEAN);
        if (_can_fullscreen != null) can_set_fullscreen = _can_fullscreen.get_boolean();

        var _can_raise = props.lookup_value("CanRaise", VariantType.BOOLEAN);
        if (_can_raise != null) can_raise = _can_raise.get_boolean();

        var _identity = props.lookup_value("Identity", VariantType.STRING);
        if (_identity != null) identity = _identity.get_string(null);

        var _entry = props.lookup_value("DesktopEntry", VariantType.STRING);
        if (_entry != null) entry = _entry.get_string(null);

        var _uri_schemes = props.lookup_value("SupportedUriSchemes", VariantType.STRING_ARRAY);
        if (_uri_schemes != null) supported_uri_schemes = _uri_schemes.get_strv();

        var _mime_types = props.lookup_value("SupportedMimeTypes", VariantType.STRING_ARRAY);
        if (_mime_types != null) supported_mime_types = _mime_types.get_strv();

        var rate = props.lookup_value("Rate", VariantType.DOUBLE);
        if (rate != null) {
            _rate = rate.get_double();
            notify_property("rate");
        }

        var volume = props.lookup_value("Volume", VariantType.DOUBLE);
        if (volume != null) {
            _volume = volume.get_double();
            notify_property("volume");
        }

        var loop = props.lookup_value("LoopStatus", VariantType.STRING);
        if (loop != null) {
            var s = Loop.from_string(loop.get_string(null));
            if (s != _loop_status) {
                _loop_status = s;
                notify_property("loop-status");
            }
        }

        var shuffle = props.lookup_value("Shuffle", VariantType.BOOLEAN);
        if (shuffle != null) {
            var s = Shuffle.from_bool(shuffle.get_boolean());
            if (s != _shuffle_status) {
                _shuffle_status = s;
                notify_property("shuffle-status");
            }
        }

        var status = props.lookup_value("PlaybackStatus", VariantType.STRING);
        if (status != null) playback_status = PlaybackStatus.from_string(status.get_string(null));

        var min_rate = props.lookup_value("MinimumRate", VariantType.DOUBLE);
        if (min_rate != null) minimum_rate = min_rate.get_double();

        var max_rate = props.lookup_value("MaximumRate", VariantType.DOUBLE);
        if (max_rate != null) maximum_rate = max_rate.get_double();

        var go_next = props.lookup_value("CanGoNext", VariantType.BOOLEAN);
        if (go_next != null) can_go_next = go_next.get_boolean();

        var go_previous = props.lookup_value("CanGoPrevious", VariantType.BOOLEAN);
        if (go_previous != null) can_go_previous = go_previous.get_boolean();

        var play = props.lookup_value("CanPlay", VariantType.BOOLEAN);
        if (play != null) can_play = play.get_boolean();

        var pause = props.lookup_value("CanPause", VariantType.BOOLEAN);
        if (pause != null) can_pause = pause.get_boolean();

        var seek = props.lookup_value("CanSeek", VariantType.BOOLEAN);
        if (seek != null) can_seek = seek.get_boolean();

        var control = props.lookup_value("CanControl", VariantType.BOOLEAN);
        if (control != null) can_control = control.get_boolean();

        var meta = props.lookup_value("Metadata", new VariantType("a{sv}"));
        if (meta != null) {
            var _id = meta.lookup_value("mpris:trackid", VariantType.STRING);
            trackid = (_id != null) ? _id.get_string(null) : "";

            var _art_url = meta.lookup_value("mpris:artUrl", VariantType.STRING);
            art_url = (_art_url != null) ? _art_url.get_string(null) : "";

            var length_x = meta.lookup_value("mpris:length", VariantType.INT64);
            var length_t = meta.lookup_value("mpris:length", VariantType.UINT64);
            if (length_x != null) length = (double)length_x.get_int64() / 1000000;
            if (length_t != null) length = (double)length_t.get_uint64() / 1000000;
            if ((length_t == null) && (length_x == null)) length = -1;

            var _title = meta.lookup_value("xesam:title", VariantType.STRING);
            title = (_title != null) ? _title.get_string(null) : "";

            var _album = meta.lookup_value("xesam:album", VariantType.STRING);
            album = (_album != null) ? _album.get_string(null) : "";

            var _text = meta.lookup_value("xesam:asText", VariantType.STRING);
            lyrics = (_text != null) ? _text.get_string(null) : "";

            var _album_artist = meta.lookup_value("xesam:albumArtist", VariantType.STRING_ARRAY);
            album_artist = (_album_artist != null) ? strv_join(_album_artist, ", ") : "";

            var _artist = meta.lookup_value("xesam:artist", VariantType.STRING_ARRAY);
            artist = (_artist != null) ? strv_join(_artist, ", ") : "";

            var _comments = meta.lookup_value("xesam:comments", VariantType.STRING_ARRAY);
            comments = (_comments != null) ? strv_join(_comments, "\n") : "";

            var _composer = meta.lookup_value("xesam:composer", VariantType.STRING_ARRAY);
            composer = (_composer != null) ? strv_join(_composer, ", ") : "";

            cache_cover.begin(null);
            metadata = meta;
        }
    }

    private string strv_join(Variant v, string sep) {
        var arr = v.get_strv();
        var builder = new StringBuilder();
        for (var i = 0; i < arr.length; ++i) {
            builder.append(arr[i]);
            if (i + 1 < arr.length) builder.append(sep);
        }
        return builder.str;
    }

    private async void cache_cover() {
        if ((art_url == null) || (art_url == "")) {
            cover_art = "";
            return;
        }

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

        if (!FileUtils.test(COVER_CACHE, FileTest.IS_DIR)) {
            try {
                File.new_for_path(COVER_CACHE).make_directory_with_parents(null);
            } catch (Error err) {
                critical(err.message);
                return;
            }
        }

        try {
            yield file.copy_async(
                File.new_for_path(path),
                FileCopyFlags.OVERWRITE,
                Priority.DEFAULT,
                null,
                null
            );

            cover_art = path;
        } catch (Error err) {
            critical("Failed to cache cover art with url \"%s\": %s", art_url, err.message);
        }
    }

    private void proxy_set_end(Object? _, AsyncResult res) {
        try {
            proxy.set.end(res);
        } catch (Error error) {
            critical(error.message);
        }
    }

    private async void check_position() {
        try {
            var posv = yield proxy.get(PlayerProxy.NAME, "Position");
            if (posv.classify() != Variant.Class.INT64) {
                return; // Position not supported
            }

            var pos = (double)posv.get_int64() / 1000000;
            if (pos != _position) {
                _position = pos;
                notify_property("position");
            }
        } catch (Error error) {
            warning(error.message);
        }
    }

    private void init_position_poll() {
        pollid = Timeout.add_seconds(1, () => {
            if (!available) {
                pollid = 0;
                return Source.REMOVE;
            }

            if (playback_status == PlaybackStatus.PLAYING) {
                check_position.begin(null);
            }

            return Source.CONTINUE;
        }, Priority.DEFAULT);
    }

    private async void on_appeared() {
        try {
            sync(yield proxy.get_all(MediaPlayerProxy.NAME));
            sync(yield proxy.get_all(PlayerProxy.NAME));
            yield check_position();
            yield cache_cover();
            available = true;
            init_position_poll();
        } catch (Error error) {
            critical(error.message);
        }
    }

    private async void init_proxy(string busname) throws Error {
        if (proxy != null) return;

        proxy = yield PlayerProxy.new(busname);

        if (proxy.g_name_owner != null) {
            yield on_appeared();
        }

        proxy.notify["g-name-owner"].connect(() => {
            if (proxy.g_name_owner != null) {
                on_appeared.begin(null);
            } else {
                available = false;
                if (pollid > 0) {
                    Source.remove(pollid);
                    pollid = 0;
                }
            }
        });

        proxy.g_properties_changed.connect(sync);

        proxy.seeked.connect((_, position) => {
            _position = (double)position / 1000000;
            notify_property("position");
        });
    }

    ~Player() {
        if (pollid > 0) {
            Source.remove(pollid);
            pollid = 0;
        }
    }
}
