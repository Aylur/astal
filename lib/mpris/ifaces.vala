[DBus (name="org.freedesktop.DBus")]
private interface AstalMpris.DBusImpl : DBusProxy {
    public abstract string[] list_names() throws GLib.Error;
    public signal void name_owner_changed(string name, string old_owner, string new_owner);
}

[DBus (name="org.freedesktop.DBus.Properties")]
private interface AstalMpris.PropsIface : DBusProxy {
    public abstract HashTable<string, Variant> get_all(string iface);
}

[DBus (name="org.mpris.MediaPlayer2")]
private interface AstalMpris.IMpris : PropsIface {
    public abstract void raise() throws GLib.Error;
    public abstract void quit() throws GLib.Error;

    public abstract bool can_quit { get; }
    public abstract bool fullscreen { get; set; }
    public abstract bool can_set_fullscreen { get; }
    public abstract bool can_raise { get; }
    public abstract bool has_track_list { get; }
    public abstract string identity { owned get; }
    public abstract string desktop_entry { owned get; }
    public abstract string[] supported_uri_schemes { owned get; }
    public abstract string[] supported_mime_types { owned get; }
}

[DBus (name="org.mpris.MediaPlayer2.Player")]
private interface AstalMpris.IPlayer : IMpris {
    public abstract void next() throws GLib.Error;
    public abstract void previous() throws GLib.Error;
    public abstract void pause() throws GLib.Error;
    public abstract void play_pause() throws GLib.Error;
    public abstract void stop() throws GLib.Error;
    public abstract void play() throws GLib.Error;
    public abstract void seek(int64 offset) throws GLib.Error;
    public abstract void set_position(ObjectPath track_id, int64 position) throws GLib.Error;
    public abstract void open_uri(string uri) throws GLib.Error;

    public signal void seeked(int64 position);

    public abstract string playback_status { owned get; }
    public abstract string loop_status { owned get; set; }
    public abstract double rate { get; set; }
    public abstract bool shuffle { get; set; }
    public abstract HashTable<string,Variant> metadata { owned get; }
    public abstract double volume { get; set; }
    public abstract int64 position { get; }
    public abstract double minimum_rate { get; set; }
    public abstract double maximum_rate { get; set; }

    public abstract bool can_go_next { get; }
    public abstract bool can_go_previous { get; }
    public abstract bool can_play { get; }
    public abstract bool can_pause { get; }
    public abstract bool can_seek { get; }
    public abstract bool can_control { get; }
}
