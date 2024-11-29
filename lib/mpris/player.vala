/**
 * Object which tracks players through their mpris dbus interface.
 * The most simple way is to use [class@AstalMpris.Mpris] which tracks every player,
 * but [class@AstalMpris.Player] can be constructed for a dedicated players too.
 */
public class AstalMpris.Player : Object {
    private static string COVER_CACHE = Environment.get_user_cache_dir() + "/astal/mpris";

    private IPlayer proxy;
    private uint pollid; // periodically notify position

    internal signal void appeared() { available = true; }
    internal signal void closed() { available = false; }

    /**
     * Full dbus namae of this player.
     */
    public string bus_name { owned get; private set; }

    /**
     * Indicates if [property@AstalMpris.Player:bus_name] is available on dbus.
     */
    public bool available { get; private set; }

    // mpris

    /**
     * Brings the player's user interface to the front
     * using any appropriate mechanism available.
     *
     * The media player may be unable to control how its user interface is displayed,
     * or it may not have a graphical user interface at all.
     * In this case, the [property@AstalMpris.Player:can_raise] is `false` and this method does nothing.
     */
    public void raise() {
        try { proxy.raise(); } catch (Error err) { critical(err.message); }
    }

    /**
     * Causes the media player to stop running.
     *
     * The media player may refuse to allow clients to shut it down.
     * In this case, the [property@AstalMpris.Player:can_quit] property is false and this method does nothing.
     */
    public void quit() {
        try { proxy.quit(); } catch (Error err) { critical(err.message); }
    }

    /**
     * Indicates if [method@AstalMpris.Player.quit] has any effect.
     */
    public bool can_quit { get; private set; }

    /**
     * Indicates if the player is occupying the fullscreen. This is typically used for videos.
     * Use [method@AstalMpris.Player.toggle_fullscreen] to toggle fullscreen state.
     */
    public bool fullscreen { get; private set; }

    /**
     * Indicates if [method@AstalMpris.Player.toggle_fullscreen] has any effect.
     */
    public bool can_set_fullscreen { get; private set; }

    /**
     * Indicates if [method@AstalMpris.Player.raise] has any effect.
     */
    public bool can_raise { get; private set; }

    // TODO: Tracklist interface
    // public bool has_track_list { get; private set; }

    /**
     * A human friendly name to identify the player.
     */
    public string identity { owned get; private set; }

    /**
     * The base name of a .desktop file
     */
    public string entry { owned get; private set; }

    /**
     * The URI schemes supported by the media player.
     *
     * This can be viewed as protocols supported by the player in almost all cases.
     * Almost every media player will include support for the "file" scheme.
     * Other common schemes are "http" and "rtsp".
     */
    public string[] supported_uri_schemes { owned get; private set; }

    /**
     * The mime-types supported by the player.
     */
    public string[] supported_mime_types { owned get; private set; }

    /**
     * Toggle [property@AstalMpris.Player:fullscreen] state.
     */
    public void toggle_fullscreen() {
        if (!can_set_fullscreen)
            critical(@"can not set fullscreen on $bus_name");

        proxy.fullscreen = !fullscreen;
    }

    /**
     * Skips to the next track in the tracklist.
     * If there is no next track (and endless playback and track repeat are both off), stop playback.
     * If [property@AstalMpris.Player:can_go_next] is `false` this method has no effect.
     */
    public void next() {
        try { proxy.next(); } catch (Error error) { critical(error.message); }
    }

    /**
     * Skips to the previous track in the tracklist.
     * If there is no previous track (and endless playback and track repeat are both off), stop playback.
     * If [property@AstalMpris.Player:can_go_previous] is `false` this method has no effect.
     */
    public void previous() {
        try { proxy.previous(); } catch (Error error) { critical(error.message); }
    }

    /**
     * Pauses playback.
     * If playback is already paused, this has no effect.
     * If [property@AstalMpris.Player:can_pause] is `false` this method has no effect.
     */
    public void pause() {
        try { proxy.pause(); } catch (Error error) { critical(error.message); }
    }

    /**
     * Pauses playback.
     * If playback is already paused, resumes playback.
     * If playback is stopped, starts playback.
     */
    public void play_pause() {
        try { proxy.play_pause(); } catch (Error error) { critical(error.message); }
    }

    /**
     * Stops playback.
     * If playback is already stopped, this has no effect.
     * If [property@AstalMpris.Player:can_control] is `false` this method has no effect.
     */
    public void stop() {
        try { proxy.stop(); } catch (Error error) { critical(error.message); }
    }

    /**
     * Starts or resumes playback.
     * If already playing, this has no effect.
     * If paused, playback resumes from the current position.
     * If [property@AstalMpris.Player:can_play] is `false` this method has no effect.
     */
    public void play() {
        try { proxy.play(); } catch (Error error) { critical(error.message); }
    }

    /**
     * uri scheme should be an element of [property@AstalMpris.Player:supported_uri_schemes]
     * and the mime-type should match one of the elements of [property@AstalMpris.Player:supported_mime_types].
     *
     * @param uri Uri of the track to load.
     */
    public void open_uri(string uri) {
        try { proxy.open_uri(uri); } catch (Error error) { critical(error.message); }
    }

    /**
     * Change [property@AstalMpris.Player:loop_status] from none to track,
     * from track to playlist, from playlist to none.
     */
    public void loop() {
        if (loop_status == Loop.UNSUPPORTED) {
            critical(@"loop is unsupported by $bus_name");
            return;
        }

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

    /**
     * Toggle [property@AstalMpris.Player:shuffle_status].
     */
    public void shuffle() {
        if (shuffle_status == Shuffle.UNSUPPORTED) {
            critical(@"shuffle is unsupported by $bus_name");
            return;
        }

        shuffle_status = shuffle_status == Shuffle.ON
            ? Shuffle.OFF
            : Shuffle.ON;
    }

    private double _get_position() {
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

    /**
     * The current loop/repeat status.
     */
    public Loop loop_status {
        get { return _loop_status; }
        set { proxy.loop_status = value.to_string(); }
    }

    /**
     * The current playback rate.
     */
    public double rate {
        get { return _rate; }
        set { proxy.rate = value; }
    }

    /**
     * The current shuffle status.
     */
    public Shuffle shuffle_status {
        get { return _shuffle_status; }
        set { proxy.shuffle = value == Shuffle.ON; }
    }

    /**
     * The current volume level between 0 and 1.
     */
    public double volume {
        get { return _volume; }
        set { proxy.volume = value; }
    }

    /**
     * The current position of the track in seconds.
     * To get a progress percentage simply divide this with [property@AstalMpris.Player:length].
     */
    public double position {
        get { return _get_position(); }
        set { _set_position(value); }
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
     * Metadata hashtable of this player.
     * In languages that cannot introspect this
     * use [method@AstalMpris.Player.get_meta].
     */
    [CCode (notify = false)] // notified manually in sync
    public HashTable<string, Variant> metadata { owned get; private set; }

    /**
     * Currently playing track's id.
     */
    public string trackid { owned get; private set; }

    /**
     * Length of the currently playing track in seconds.
     */
    public double length { get; private set; }

    /**
     * The location of an image representing the track or album.
     * You should always prefer to use [property@AstalMpris.Player:cover_art].
     */
    public string art_url { owned get; private set; }

    /**
     * Title of the currently playing album.
     */
    public string album { owned get; private set; }

    /**
     * Artists of the currently playing album.
     */
    public string album_artist { owned get; private set; }

    /**
     * Artists of the currently playing track.
     */
    public string artist { owned get; private set; }

    /**
     * Lyrics of the currently playing track.
     */
    public string lyrics { owned get; private set; }

    /**
     * Title of the currently playing track.
     */
    public string title { owned get; private set; }

    /**
     * Composers of the currently playing track.
     */
    public string composer { owned get; private set; }

    /**
     * Comments of the currently playing track.
     */
    public string comments { owned get; private set; }

    /**
     * Path of the cached [property@AstalMpris.Player:art_url].
     */
    public string cover_art { owned get; private set; }

    /**
     * Lookup a key from [property@AstalMpris.Player:metadata].
     * This method is useful for languages that fail to introspect hashtables.
     */
    public Variant? get_meta(string key) {
        return metadata.lookup(key);
    }

    /**
     * Construct a Player that tracks a dbus name. For example "org.mpris.MediaPlayer2.spotify".
     * The "org.mpris.MediaPlayer2." prefix can be leftout so simply "spotify" would mean the same.
     * [property@AstalMpris.Player:available] indicates whether the player is actually running or not.
     *
     * @param name dbus name of the player.
     */
    public Player(string name) {
        bus_name = name.has_prefix("org.mpris.MediaPlayer2.")
            ? name : @"org.mpris.MediaPlayer2.$name";
    }

    private void sync() {
        // mpris
        can_quit = proxy.can_quit;
        fullscreen = proxy.fullscreen;
        can_set_fullscreen = proxy.can_set_fullscreen;
        can_raise = proxy.can_raise;
        // has_track_list = proxy.has_track_list;
        identity = proxy.identity;
        entry = proxy.desktop_entry;
        supported_uri_schemes = proxy.supported_uri_schemes;
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
            if (metadata.get("mpris:length") != null) {
                var v = metadata.get("mpris:length");
                if (v.get_type_string() == "x") {
                    length = (double)v.get_int64() / 1000000;
                } else if (v.get_type_string() == "t") {
                    length = (double)v.get_uint64() / 1000000;
                }
            } else {
                length = -1;
            }

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
        if (art_url == null || art_url == "") {
            cover_art = null;
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
        notify["bus-name"].connect(() => {
            try {
                setup_proxy();
                setup_position_poll();
                sync();
            } catch (Error error) {
                critical(error.message);
            }
        });
    }

    private void setup_position_poll() {
        var current_position = position;

        pollid = Timeout.add_seconds(1, () => {
            if (!available)
                return Source.CONTINUE;

            if (position >= 0 && current_position != position) {
                current_position = position;
                notify_property("position");
            }
            return Source.CONTINUE;
        }, Priority.DEFAULT);
    }

    private void setup_proxy() throws Error {
        if (proxy != null)
            return;

        proxy = Bus.get_proxy_sync(
            BusType.SESSION,
            bus_name,
            "/org/mpris/MediaPlayer2"
        );

        if (proxy.g_name_owner != null) {
            appeared();
        }

        proxy.notify["g-name-owner"].connect(() => {
            if (proxy.g_name_owner != null) {
                appeared();
            } else {
                closed();
            }
        });

        proxy.g_properties_changed.connect(sync);
    }

    ~Player() {
        Source.remove(pollid);
    }
}

public enum AstalMpris.PlaybackStatus {
    PLAYING,
    PAUSED,
    STOPPED;

    internal static PlaybackStatus from_string(string? str) {
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
}

public enum AstalMpris.Loop {
    UNSUPPORTED,
    /** The playback will stop when there are no more tracks to play. */
    NONE,
    /** The current track will start again from the begining once it has finished playing. */
    TRACK,
    /** The playback loops through a list of tracks. */
    PLAYLIST;

    internal static Loop from_string(string? str) {
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

    internal string? to_string() {
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

public enum AstalMpris.Shuffle {
    UNSUPPORTED,
    /** Playback is progressing through a playlist in some other order. */
    ON,
    /** Playback is progressing linearly through a playlist. */
    OFF;

    internal static Shuffle from_bool(bool b) {
        return b ? Shuffle.ON : Shuffle.OFF;
    }

    internal string? to_string() {
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
