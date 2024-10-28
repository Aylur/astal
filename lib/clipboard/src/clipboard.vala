public class AstalClipboard.Clipboard : Object {
  
  private WlSource.WlSource source;
  private Wl.Registry registry;
  private Wl.Seat? seat;

  private DataControl.Manager? manager;
  private DataControl.Device? device;

  internal static Wl.RegistryListener registry_listener = Wl.RegistryListener() {
    global = registry_handle_global,
    global_remove = registry_handle_global_remove
  };

  internal static DataControl.DeviceListener device_listener = DataControl.DeviceListener() { 
    data_offer = device_handle_data_offer,
    selection = device_handle_selection,
    primary_selection = device_handle_primary_selection,
    finished = device_handle_finished
  };

  private static DataControl.OfferListener offer_listener;

  private void registry_handle_global (Wl.Registry wl_registry, uint32 name, string interface, uint32 version) {
    // if(interface == DataControl.Manager.interface.name) { //TODO: figure this out
    if(interface == "zwlr_data_control_manager_v1") {
      this.manager = wl_registry.bind<DataControl.Manager>(name, ref DataControl.Manager.interface, version);
    }
    else if(interface == "wl_seat") {
      this.seat = wl_registry.bind<Wl.Seat>(name, ref Wl.Seat.interface, version);
    }
  }

  private void registry_handle_global_remove (Wl.Registry wl_registry, uint32 name) {
    print("rem: %u\n", name);
  }

	private void device_handle_data_offer(DataControl.Device device, DataControl.Offer offer) {
    print("data offer\n");
    // var ol = new DataControl.OfferListener();
    // ol.offer = offer_handle_offer;
    // offer.add_listener(ol, this);
  }

	private void device_handle_selection (DataControl.Device device, DataControl.Offer? offer) {
    print("selection\n");
  }

	private void device_handle_primary_selection (DataControl.Device device, DataControl.Offer? offer) {
    print("primary selection\n");
  }

	private void device_handle_finished (DataControl.Device device) {
    print("finished\n");
  }
	
  private void offer_handle_offer (DataControl.Offer offer, string mime) {
    print("offer %s\n", mime);
  }

  construct {
    this.source = new WlSource.WlSource();

    this.registry = this.source.display.get_registry();
    this.registry.add_listener(registry_listener, this);

    this.source.display.roundtrip();
   
    assert(this.manager != null && this.seat != null);
    this.device = this.manager.get_data_device(this.seat);
    this.device.add_listener(device_listener, this);
    
    this.source.display.roundtrip();
  }

}
