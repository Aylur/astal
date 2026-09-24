namespace AstalClipboard {
public class DataOffer : Object {
    private unowned ZwlrDataControlOfferV1? wlr_offer;
    private HashTable<string, Data> _data;
    private int ready_counter = 0;

    public int id { get; construct; default = -1; }

    public signal void data_ready();

    public string[] get_mime_types() {
        return this._data.get_keys_as_array();
    }

    public Data get_offer_data(string mime_type) {
        return this._data.get(mime_type);
    }

    private void handle_offer(ZwlrDataControlOfferV1 offer, string mime_type) {
        //TODO: maybe ask the user if he's interested in this mime-type
        var data_offer = new Data.for_offer(mime_type, offer);
        this._data.insert(mime_type, data_offer);
        this.ready_counter++;
        data_offer.notify["data-ready"].connect(() => {
            if (--this.ready_counter == 0) this.data_ready();
        });
    }

    private const ZwlrDataControlOfferV1Listener offer_listener = {
        handle_offer
    };

    internal void add_data(Data data) {
        this._data.insert(data.mime_type, data);
    }

    internal DataOffer.for_offer(ZwlrDataControlOfferV1 offer, int id) {
        Object(id : id);
        this.wlr_offer = offer;
        this.wlr_offer.add_listener(offer_listener, this);
        this._data = new HashTable<string, Data>(str_hash, str_equal);
    }

    internal DataOffer(int id) {
        Object(id: id);
        this._data = new HashTable<string, Data>(str_hash, str_equal);
    }
}
}
