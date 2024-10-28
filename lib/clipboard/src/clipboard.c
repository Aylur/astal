#include <gio/gio.h>
#include <time.h>
#include <wayland-client-protocol.h>
#include <wayland-client.h>
#include <wayland-glib.h>

#include "astal-clipboard.h"
#include "wlr-data-control-unstable-v1-client.h"

struct _AstalClipboardClipboard {
    GObject parent_instance;
};

typedef struct {
    GHashTable* signal_ids;
    struct wl_registry* wl_registry;
    struct wl_seat* seat;
    struct wl_display* display;
    WlSourceWlSource* wl_source;
    struct zwlr_data_control_manager_v1* manager;
    struct zwlr_data_control_device_v1* device;
} AstalClipboardClipboardPrivate;

G_DEFINE_TYPE_WITH_PRIVATE(AstalClipboardClipboard, astal_clipboard_clipboard, G_TYPE_OBJECT)

typedef enum {
    ASTAL_CLIPBOARD_CLIPBOARD_N_PROPERTIES = 1
} AstalCiplboardClipboardProperties;

typedef enum {
    ASTAL_CLIPBOARD_CLIPBOARD_N_SIGNALS
} AstalClipboardClipboardSignals;

static guint astal_clipboard_clipboard_signals[ASTAL_CLIPBOARD_CLIPBOARD_N_SIGNALS] = {
    0,
};
static GParamSpec* astal_clipboard_clipboard_properties[ASTAL_CLIPBOARD_CLIPBOARD_N_PROPERTIES] = {
    NULL,
};

static void astal_clipboard_clipboard_get_property(GObject* object, guint property_id, GValue* value,
                                           GParamSpec* pspec) {
    AstalClipboardClipboard* self = ASTAL_CLIPBOARD_CLIPBOARD(object);

    switch (property_id) {
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void noop() {}

static void global_registry_handler(void* data, struct wl_registry* registry, uint32_t id,
                                    const char* interface, uint32_t version) {
    AstalClipboardClipboard* self = ASTAL_CLIPBOARD_CLIPBOARD(data);
    AstalClipboardClipboardPrivate* priv = astal_clipboard_clipboard_get_instance_private(self);
    if (strcmp(interface, wl_seat_interface.name) == 0) {
        priv->seat = wl_registry_bind(registry, id, &wl_seat_interface, 4);
    } else if (strcmp(interface, zwlr_data_control_manager_v1_interface.name) == 0) {
        priv->manager = wl_registry_bind(registry, id, &zwlr_data_control_manager_v1_interface, 2);
    }
}

static void global_registry_remover(void* data, struct wl_registry* registry, uint32_t id) {
}

static void offer_handle_offer(void* data, struct zwlr_data_control_offer_v1 *offer, const char *mime_type) {
  g_print("offer %s\n", mime_type);
}

static const struct zwlr_data_control_offer_v1_listener offer_listener = {
  .offer = offer_handle_offer
};

static void device_handle_data_offer(void* data, struct zwlr_data_control_device_v1 *device, struct zwlr_data_control_offer_v1 *offer) {
  g_print("data offer\n");
  if(offer == NULL) return;
  
  zwlr_data_control_offer_v1_add_listener(offer, &offer_listener, data);

}

static void device_handle_selection(void* data, struct zwlr_data_control_device_v1 *device, struct zwlr_data_control_offer_v1 *offer) {
  g_print("selection\n");
}

static void device_handle_primary_selection(void* data, struct zwlr_data_control_device_v1 *device, struct zwlr_data_control_offer_v1 *offer) {
  g_print("primary selection\n");
}

static void device_handle_finished(void* data, struct zwlr_data_control_device_v1 *device) {
  g_print("finished\n");
}


static const struct wl_registry_listener registry_listener = {global_registry_handler,
                                                              global_registry_remover};

static const struct zwlr_data_control_device_v1_listener device_listener = {
  .finished = device_handle_finished,
  .selection = device_handle_selection,
  .primary_selection = device_handle_primary_selection,
  .data_offer = device_handle_data_offer
};

static void astal_clipboard_clipboard_init(AstalClipboardClipboard* self) {
    AstalClipboardClipboardPrivate* priv = astal_clipboard_clipboard_get_instance_private(self);

    priv->manager = NULL;
    priv->device = NULL;
    priv->seat = NULL;

    priv->wl_source = wl_source_wl_source_new();

    if (priv->wl_source == NULL) {
        g_critical("Can not connect to wayland display");
        return;
    }

    priv->display = priv->wl_source->display;

    priv->wl_registry = wl_display_get_registry(priv->display);
    wl_registry_add_listener(priv->wl_registry, &registry_listener, self);

    wl_display_roundtrip(priv->display);

    if (priv->manager == NULL) {
        g_critical("Can not connect to the wlr data control protocol");
        return;
    }

    priv->device = zwlr_data_control_manager_v1_get_data_device(priv->manager, priv->seat);
    zwlr_data_control_device_v1_add_listener(priv->device, &device_listener, self);

    wl_display_roundtrip(priv->display);

}

static void astal_clipboard_clipboard_finalize(GObject* object) {
    AstalClipboardClipboard* self = ASTAL_CLIPBOARD_CLIPBOARD(object);
    AstalClipboardClipboardPrivate* priv = astal_clipboard_clipboard_get_instance_private(self);

    if (priv->display != NULL) wl_display_roundtrip(priv->display);

    if (priv->wl_registry != NULL) wl_registry_destroy(priv->wl_registry);
    if (priv->manager != NULL)
        zwlr_data_control_manager_v1_destroy(priv->manager);
    if (priv->device != NULL) zwlr_data_control_device_v1_destroy(priv->device);
    if (priv->seat != NULL) wl_seat_destroy(priv->seat);
    if (priv->display != NULL) wl_display_flush(priv->display);

    if (priv->wl_source != NULL) g_source_unref((GSource*)(priv->wl_source));

    G_OBJECT_CLASS(astal_clipboard_clipboard_parent_class)->finalize(object);
}

static void astal_clipboard_clipboard_class_init(AstalClipboardClipboardClass* class) {
    GObjectClass* object_class = G_OBJECT_CLASS(class);
    object_class->get_property = astal_clipboard_clipboard_get_property;
    object_class->finalize = astal_clipboard_clipboard_finalize;

}
