namespace AstalClipboard {
[DBus(name = "io.astal.clipboard")]
internal interface DaemonProxy : DBusProxy {
    public signal void new_selection(int id);
    public abstract async string[] get_mime_types(int id) throws Error;
    public abstract async void get_fd(int id, string mime_type, out UnixInputStream fd) throws Error;
    public abstract async Variant get_history() throws Error;
}

internal class Proxy : Backend {
    private DaemonProxy proxy;

    construct {
        try {
            var bus = Bus.get_sync(BusType.SESSION, null);
            setup_proxy.begin();
        } catch (Error err) {
            critical(@"cannot get proxy: $(err.message)");
        }
    }

    private async Data get_data_for_mime(int id, string mime_type) {
        UnixInputStream str;
        yield proxy.get_fd(id, mime_type, out str);
        return new Data(mime_type, str.get_fd());
    }

    private async void selection_handler(int id) {
        if(id < 0) {
            this.selection = null;
            this.selected(null);
            return;
        }
        var offer = new DataOffer(id);
        var mimes = yield proxy.get_mime_types(id);
        foreach (var mime in mimes) {
            offer.add_data(yield get_data_for_mime(id, mime));
        }
        this.add_to_history(offer);
        this.selection = offer;
        this.selected(offer);
    }

    private async void setup_proxy() throws Error {
        proxy = Bus.get_proxy_sync(
            BusType.SESSION,
            "io.astal.clipboard",
            "/io/astal/clipboard"
        );

        proxy.new_selection.connect(selection_handler);
        var history = yield proxy.get_history();
        var iter = new VariantIter(history);
        Variant v;
        while (iter.next("v", out v)) {
            var id = v.get_child_value(0).get_int32();
            var data_offer = new DataOffer(id);
            var mimes = v.get_child_value(1).get_strv();
            foreach (var mime in mimes) {
                data_offer.add_data(yield get_data_for_mime(id, mime));
            }
            this.add_to_history(data_offer);
        }
    }
}
}
