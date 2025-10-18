[DBus(name = "org.freedesktop.DBus")]
interface AstalMpris.BusProxy : DBusProxy {
    public abstract async string[] list_names() throws GLib.Error;
    public signal void name_owner_changed(string name, string old_owner, string new_owner);

    public static async BusProxy new() throws Error {
        return yield Bus.get_proxy(
            BusType.SESSION,
            "org.freedesktop.DBus",
            "/org/freedesktop/DBus",
            DBusProxyFlags.DO_NOT_LOAD_PROPERTIES
        );
    }
}

[DBus(name = "org.freedesktop.DBus.Properties")]
interface AstalMpris.PropertiesProxy : DBusProxy {
    public signal void properties_changed(string iface, HashTable<string, Variant> changed, string[] invalidated);

    public abstract async HashTable<string, Variant> get_all(string iface) throws Error;
    public abstract async Variant get(string iface, string property) throws Error;
    public abstract async void set(string iface, string property, Variant value) throws Error;
}

[DBus(name = "org.mpris.MediaPlayer2")]
interface AstalMpris.MediaPlayerProxy : PropertiesProxy {
    public const string PREFIX = "org.mpris.MediaPlayer2.";
    public const string NAME = "org.mpris.MediaPlayer2";

    public abstract async void raise() throws GLib.Error;
    public abstract async void quit() throws GLib.Error;
}

[DBus(name = "org.mpris.MediaPlayer2.Player")]
interface AstalMpris.PlayerProxy : MediaPlayerProxy {
    public const string NAME = "org.mpris.MediaPlayer2.Player";
    public signal void seeked(int64 position);

    public abstract async void next() throws GLib.Error;
    public abstract async void previous() throws GLib.Error;
    public abstract async void pause() throws GLib.Error;
    public abstract async void play_pause() throws GLib.Error;
    public abstract async void stop() throws GLib.Error;
    public abstract async void play() throws GLib.Error;
    public abstract async void seek(int64 offset) throws GLib.Error;
    public abstract async void set_position(ObjectPath track_id, int64 position) throws GLib.Error;
    public abstract async void open_uri(string uri) throws GLib.Error;

    public static async PlayerProxy new(string name) throws Error {
        return yield Bus.get_proxy(
            BusType.SESSION,
            name,
            "/org/mpris/MediaPlayer2",
            DBusProxyFlags.DO_NOT_AUTO_START
        );
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

    internal string? to_string() {
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
