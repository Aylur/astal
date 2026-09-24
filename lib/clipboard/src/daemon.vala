namespace AstalClipboard {
internal abstract class Backend : Object {
    internal List<weak DataOffer> _history;
    internal HashTable<int, DataOffer> offers;

    internal DataOffer? selection { get; set; }
    internal List<weak DataOffer> history { get { return this._history; } }

    internal signal void selected(DataOffer? offer);

    internal int max_history { get; set; default = 15; }

    internal void add_to_history(DataOffer data_offer) {
        this._history.prepend(data_offer);
        this.offers.insert(data_offer.id, data_offer);
        //TODO: iterates twice over _history per loop
        while (this._history.length() > this.max_history) {
            var rem_offer = this._history.last().data;
            this._history.remove(rem_offer);
            this.offers.remove(rem_offer.id);
        }
        this.notify_property("history");
    }

    construct {
        this._history = new List<weak DataOffer>();
        this.offers = new HashTable<int, DataOffer>(direct_hash, direct_equal);
    }
}

[DBus(name = "io.astal.clipboard")]
internal class Daemon : Backend {
    private AstalWl.Registry registry;
    private ZwlrDataControlManagerV1? data_manager;
    private ZwlrDataControlDeviceV1? data_device;
    private int id_counter = 0;

    public signal void new_selection(int id);

    public string[] get_mime_types(int id) throws Error {
        var offer = this.offers.get(id);
        if (offer == null) throw new DBusError.INVALID_ARGS("id does no exist");
        return offer.get_mime_types();
    }

    public void get_fd(int id, string mime_type, out UnixInputStream fd) throws Error {
        var offer = this.offers.get(id);
        if (offer == null) throw new DBusError.INVALID_ARGS("id does no exist");
        var data = offer.get_offer_data(mime_type);
        if (data == null) throw new DBusError.INVALID_ARGS("mime-type does no exist for given id");

        fd = new UnixInputStream(data.fd, false);
    }

    [DBus (name="GetHistory")]
    public Variant dbus_get_history() throws Error {
        var builder = new VariantBuilder(new VariantType.array(new VariantType("v")));
        foreach (var item in this._history) {
            builder.add("v", new Variant.tuple({
                new Variant.int32(item.id),
                new Variant.strv(item.get_mime_types())
            }));
        }
        return builder.end();
    }

    private void handle_data_offer(ZwlrDataControlDeviceV1 device, ZwlrDataControlOfferV1 offer) {
        var data_offer = new DataOffer.for_offer(offer, ++id_counter);
        offer.set_user_data(data_offer.ref());
    }

    private void handle_selection(ZwlrDataControlDeviceV1 device, ZwlrDataControlOfferV1? offer) {
        //TODO: detect duplicates
        if (offer == null) {
            this.selection = null;
            selected(null);
            new_selection(-1);
            return;
        }
        DataOffer data_offer = (DataOffer)offer.get_user_data();
        data_offer.data_ready.connect(() => {
            this.selection = data_offer;
            this.add_to_history(data_offer);
            selected(data_offer);
            new_selection(data_offer.id);
            data_offer.unref();
            offer.destroy();
        });
    }

    private void handle_primary_selection(ZwlrDataControlDeviceV1 device, ZwlrDataControlOfferV1? offer) {
        //TODO: support primary selection. Should it have its own history, 
        //stored in the same history with a flag in DataOffer, or no history at all?
        if (offer == null) return;
        DataOffer data_offer = (DataOffer)offer.get_user_data();
        data_offer.unref();
        offer.destroy();
    }

    private void handle_finished(ZwlrDataControlDeviceV1 device) {}

    private const ZwlrDataControlDeviceV1Listener device_listener = {
        handle_data_offer,
        handle_selection,
        handle_finished,
        handle_primary_selection
    };

    internal Daemon(DBusConnection conn) {
        try {
            conn.register_object("/io/astal/clipboard", this);
        } catch (Error err) {
            critical(err.message);
            return;
        }

        //TODO: support ext-data-control-v1.xml
        this.registry = AstalWl.Registry.get_default();
        AstalWl.Global? manager_global = this.registry.find_globals("zwlr_data_control_manager_v1").nth_data(0);
        if (manager_global == null) {
            critical("the compositor does not support the wlr-data-control-manager protocol");
            return;
        }
        this.data_manager = this.registry.get_registry().bind<ZwlrDataControlManagerV1>(manager_global.name, ref ZwlrDataControlManagerV1.iface, uint.min(manager_global.version, 2));
        AstalWl.Seat seat = this.registry.get_seats().nth_data(0);
        this.data_device = this.data_manager.get_data_device(seat.get_wl_seat());
        this.data_device.add_listener(device_listener, this);
    }
}
}
