#include <gio/gio.h>
#include <wayland-client-core.h>
#include <wayland-client-protocol.h>
#include <wayland-client.h>
#include <wayland-glib.h>

#include "astal-clipboard-private.h"
#include "astal-clipboard.h"
#include "astal-clipboard.h.in"
#include "glib-object.h"
#include "glib.h"
#include "wlr-data-control-unstable-v1-client.h"

struct _AstalClipboardClipboard {
    GObject parent_instance;

    AstalClipboardSelection* selection;
    AstalClipboardSelection* primary_selection;

    guint max_length;

    GList* selection_history;
    GList* primary_selection_history;
};

typedef struct {
    struct wl_registry* wl_registry;
    struct wl_seat* seat;
    struct wl_display* display;
    WlSourceWlSource* wl_source;
    struct zwlr_data_control_manager_v1* manager;
    struct zwlr_data_control_device_v1* device;

    GList* pending_offers;

} AstalClipboardClipboardPrivate;

G_DEFINE_TYPE_WITH_PRIVATE(AstalClipboardClipboard, astal_clipboard_clipboard, G_TYPE_OBJECT)

typedef enum {
    ASTAL_CLIPBOARD_CLIPBOARD_PROP_SELECTION = 1,
    ASTAL_CLIPBOARD_CLIPBOARD_PROP_PRIMARY_SELECTION,
    ASTAL_CLIPBOARD_CLIPBOARD_PROP_MAX_LENGTH,
    ASTAL_CLIPBOARD_CLIPBOARD_PROP_SELECTION_HISTORY,
    ASTAL_CLIPBOARD_CLIPBOARD_PROP_PRIMARY_SELECTION_HISTORY,
    ASTAL_CLIPBOARD_CLIPBOARD_N_PROPERTIES
} AstalClipboardClipboardProperties;

typedef enum { ASTAL_CLIPBOARD_CLIPBOARD_N_SIGNALS } AstalClipboardClipboardSignals;

static guint astal_clipboard_clipboard_signals[ASTAL_CLIPBOARD_CLIPBOARD_N_SIGNALS] = {
    0,
};
static GParamSpec* astal_clipboard_clipboard_properties[ASTAL_CLIPBOARD_CLIPBOARD_N_PROPERTIES] = {
    NULL,
};

static void limit_list_length(AstalClipboardClipboard* self, GList** list) {
    GList* last;
    while (g_list_length(*list) > self->max_length) {
        last = g_list_last(*list);
        g_object_unref(last->data);
        *list = g_list_delete_link(*list, last);
    }
}

static void prepend_list(AstalClipboardClipboard* self, GList** list, gpointer data) {
    *list = g_list_prepend(*list, data);
    limit_list_length(self, list);
}

/**
 * astal_clipboard_clipboard_get_selection:
 *
 * Returns: (transfer none):
 */
AstalClipboardSelection* astal_clipboard_clipboard_get_selection(AstalClipboardClipboard* self) {
    return self->selection;
}

/**
 * astal_clipboard_clipboard_get_primary_selection:
 *
 * Returns: (transfer none):
 */
AstalClipboardSelection* astal_clipboard_clipboard_get_primary_selection(
    AstalClipboardClipboard* self) {
    return self->primary_selection;
}

/**
 * astal_clipboard_clipboard_get_max_length:
 *
 * Returns:
 */
guint astal_clipboard_clipboard_get_max_length(AstalClipboardClipboard* self) {
    return self->max_length;
}

/**
 * astal_clipboard_clipboard_set_max_length:
 *
 * Returns:
 */
void astal_clipboard_clipboard_set_max_length(AstalClipboardClipboard* self, guint max_length) {
    self->max_length = max_length;
    limit_list_length(self, &self->selection_history);
    limit_list_length(self, &self->primary_selection_history);
}

/**
 * astal_clipboard_clipboard_get_selection_history:
 *
 * Returns: (transfer none) (element-type AstalClipboard.Selection):
 */
GList* astal_clipboard_clipboard_get_selection_history(AstalClipboardClipboard* self) {
    return self->selection_history;
}

/**
 * astal_clipboard_clipboard_get_primary_selection_history:
 *
 * Returns: (transfer none) (element-type AstalClipboard.Selection):
 */
GList* astal_clipboard_clipboard_get_primary_selection_history(AstalClipboardClipboard* self) {
    return self->primary_selection_history;
}

static void astal_clipboard_clipboard_get_property(GObject* object, guint property_id,
                                                   GValue* value, GParamSpec* pspec) {
    AstalClipboardClipboard* self = ASTAL_CLIPBOARD_CLIPBOARD(object);

    switch (property_id) {
        case ASTAL_CLIPBOARD_CLIPBOARD_PROP_SELECTION:
            g_value_set_object(value, self->selection);
            break;
        case ASTAL_CLIPBOARD_CLIPBOARD_PROP_PRIMARY_SELECTION:
            g_value_set_object(value, self->primary_selection);
            break;
        case ASTAL_CLIPBOARD_CLIPBOARD_PROP_MAX_LENGTH:
            g_value_set_uint(value, self->max_length);
            break;
        case ASTAL_CLIPBOARD_CLIPBOARD_PROP_SELECTION_HISTORY:
            g_value_set_pointer(value, self->selection_history);
            break;
        case ASTAL_CLIPBOARD_CLIPBOARD_PROP_PRIMARY_SELECTION_HISTORY:
            g_value_set_pointer(value, self->primary_selection_history);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_clipboard_clipboard_set_property(GObject* object, guint property_id,
                                                   const GValue* value, GParamSpec* pspec) {
    AstalClipboardClipboard* self = ASTAL_CLIPBOARD_CLIPBOARD(object);

    switch (property_id) {
        case ASTAL_CLIPBOARD_CLIPBOARD_PROP_MAX_LENGTH:
            astal_clipboard_clipboard_set_max_length(self, g_value_get_uint(value));
            break;
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

static void global_registry_remover(void* data, struct wl_registry* registry, uint32_t id) {}

static void device_handle_data_offer(void* data, struct zwlr_data_control_device_v1* device,
                                     struct zwlr_data_control_offer_v1* offer) {
    AstalClipboardClipboard* self = ASTAL_CLIPBOARD_CLIPBOARD(data);
    AstalClipboardClipboardPrivate* priv = astal_clipboard_clipboard_get_instance_private(self);

    if (offer == NULL) {
        return;
    }
    priv->pending_offers =
        g_list_append(priv->pending_offers, astal_clipboard_selection_new(offer));
}

static void primary_selection_cb(AstalClipboardSelection* selection,
                                 AstalClipboardClipboard* self) {
    AstalClipboardClipboardPrivate* priv = astal_clipboard_clipboard_get_instance_private(self);

    priv->pending_offers = g_list_remove(priv->pending_offers, selection);

    g_clear_object(&self->primary_selection);
    self->primary_selection = g_object_ref(selection);

    prepend_list(self, &self->primary_selection_history, self->primary_selection);

    g_object_notify(G_OBJECT(self), "primary-selection");
    g_object_notify(G_OBJECT(self), "primary-selection-history");
}

static void selection_cb(AstalClipboardSelection* selection, AstalClipboardClipboard* self) {
    AstalClipboardClipboardPrivate* priv = astal_clipboard_clipboard_get_instance_private(self);

    priv->pending_offers = g_list_remove(priv->pending_offers, selection);

    g_clear_object(&self->selection);
    self->selection = g_object_ref(selection);

    prepend_list(self, &self->selection_history, self->selection);

    g_object_notify(G_OBJECT(self), "selection");
    g_object_notify(G_OBJECT(self), "selection-history");
}

static gint offer_equal(gconstpointer a, gconstpointer b) {
    const AstalClipboardSelection* offer_a = a;
    const struct zwlr_data_control_offer_v1* offer_b = b;
    return astal_clipboard_selection_get_offer(offer_a) != offer_b;
}

static void device_handle_selection(void* data, struct zwlr_data_control_device_v1* device,
                                    struct zwlr_data_control_offer_v1* offer) {
    AstalClipboardClipboard* self = ASTAL_CLIPBOARD_CLIPBOARD(data);
    AstalClipboardClipboardPrivate* priv = astal_clipboard_clipboard_get_instance_private(self);

    if (offer != NULL) {
        GList* selected_offer = g_list_find_custom(priv->pending_offers, offer, offer_equal);
        if (selected_offer != NULL) {
            g_signal_connect(selected_offer->data, "ready", G_CALLBACK(selection_cb), self);
        }
    } else {
        g_clear_object(&self->selection);
        g_object_notify(G_OBJECT(self), "selection");
    }
}

static void device_handle_primary_selection(void* data, struct zwlr_data_control_device_v1* device,
                                            struct zwlr_data_control_offer_v1* offer) {
    AstalClipboardClipboard* self = ASTAL_CLIPBOARD_CLIPBOARD(data);
    AstalClipboardClipboardPrivate* priv = astal_clipboard_clipboard_get_instance_private(self);

    if (offer != NULL) {
        GList* selected_offer = g_list_find_custom(priv->pending_offers, offer, offer_equal);
        if (selected_offer != NULL) {
            g_signal_connect(selected_offer->data, "ready", G_CALLBACK(primary_selection_cb), self);
        }
    } else {
        g_clear_object(&self->primary_selection);
        g_object_notify(G_OBJECT(self), "primary_selection");
    }
}

static void device_handle_finished(void* data, struct zwlr_data_control_device_v1* device) {}

static const struct wl_registry_listener registry_listener = {global_registry_handler,
                                                              global_registry_remover};

static const struct zwlr_data_control_device_v1_listener device_listener = {
    .finished = device_handle_finished,
    .selection = device_handle_selection,
    .primary_selection = device_handle_primary_selection,
    .data_offer = device_handle_data_offer};

/**
 * astal_clipboard_clipboard_get_default
 *
 * gets the default clipboard object.
 *
 * Returns: (nullable) (transfer none): gets the default clipboard object.
 */
AstalClipboardClipboard* astal_clipboard_clipboard_get_default() {
    static AstalClipboardClipboard* self = NULL;

    if (self == NULL) self = g_object_new(ASTAL_CLIPBOARD_TYPE_CLIPBOARD, NULL);

    return self;
}

/**
 * astal_clipboard_get_default
 *
 * gets the default clipboard object.
 *
 * Returns: (nullable) (transfer none): gets the default clipboard object.
 */
AstalClipboardClipboard* astal_clipboard_get_default() {
    return astal_clipboard_clipboard_get_default();
}

static void astal_clipboard_clipboard_init(AstalClipboardClipboard* self) {
    AstalClipboardClipboardPrivate* priv = astal_clipboard_clipboard_get_instance_private(self);

    self->selection = NULL;
    self->primary_selection = NULL;
    self->selection_history = NULL;

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
    if (priv->manager != NULL) zwlr_data_control_manager_v1_destroy(priv->manager);
    if (priv->device != NULL) zwlr_data_control_device_v1_destroy(priv->device);
    if (priv->seat != NULL) wl_seat_destroy(priv->seat);
    if (priv->display != NULL) wl_display_flush(priv->display);

    if (priv->wl_source != NULL) g_source_unref((GSource*)(priv->wl_source));

    G_OBJECT_CLASS(astal_clipboard_clipboard_parent_class)->finalize(object);
}

static void astal_clipboard_clipboard_class_init(AstalClipboardClipboardClass* class) {
    GObjectClass* object_class = G_OBJECT_CLASS(class);
    object_class->get_property = astal_clipboard_clipboard_get_property;
    object_class->set_property = astal_clipboard_clipboard_set_property;
    object_class->finalize = astal_clipboard_clipboard_finalize;

    astal_clipboard_clipboard_properties[ASTAL_CLIPBOARD_CLIPBOARD_PROP_SELECTION] =
        g_param_spec_object("selection", "selection", "selection", ASTAL_CLIPBOARD_TYPE_SELECTION,
                            G_PARAM_READABLE);
    astal_clipboard_clipboard_properties[ASTAL_CLIPBOARD_CLIPBOARD_PROP_PRIMARY_SELECTION] =
        g_param_spec_object("primary-selection", "primary-selection", "primary-selection",
                            ASTAL_CLIPBOARD_TYPE_SELECTION, G_PARAM_READABLE);
    astal_clipboard_clipboard_properties[ASTAL_CLIPBOARD_CLIPBOARD_PROP_MAX_LENGTH] =
        g_param_spec_uint("max-length", "max-length", "max-length", 0, UINT_MAX, 10,
                          G_PARAM_READWRITE | G_PARAM_CONSTRUCT);
    /**
     * AstalClipboardClipboard:selection-history: (type GList(AstalClipboardSelection))
     */
    astal_clipboard_clipboard_properties[ASTAL_CLIPBOARD_CLIPBOARD_PROP_SELECTION_HISTORY] =
        g_param_spec_pointer("selection-history", "selection-history", "selection-history",
                             G_PARAM_READABLE);
    /**
     * AstalClipboardClipboard:primary-selection-history: (type GList(AstalClipboardSelection))
     */
    astal_clipboard_clipboard_properties[ASTAL_CLIPBOARD_CLIPBOARD_PROP_PRIMARY_SELECTION_HISTORY] =
        g_param_spec_pointer("primary-selection-history", "primary-selection-history",
                             "primary-selection-history", G_PARAM_READABLE);

    g_object_class_install_properties(object_class, ASTAL_CLIPBOARD_CLIPBOARD_N_PROPERTIES,
                                      astal_clipboard_clipboard_properties);
}
