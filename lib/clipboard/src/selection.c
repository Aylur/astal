#include <fcntl.h>
#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gio/gio.h>
#include <glib-unix.h>

#include "astal-clipboard-private.h"
#include "astal-clipboard.h"
#include "astal-clipboard.h.in"
#include "glib-object.h"
#include "glib.h"
#include "wlr-data-control-unstable-v1-client.h"

struct _AstalClipboardSelection {
    GObject parent_instance;
    GList* data;
};

typedef struct {
    struct zwlr_data_control_offer_v1* offer;
    gint offer_counter;
} AstalClipboardSelectionPrivate;

G_DEFINE_TYPE_WITH_PRIVATE(AstalClipboardSelection, astal_clipboard_selection, G_TYPE_OBJECT)

typedef enum {
    ASTAL_CLIPBOARD_SELECTION_PROP_DATA = 1,
    ASTAL_CLIPBOARD_SELECTION_N_PROPERTIES
} AstalCiplboardSelectionProperties;

typedef enum {
    ASTAL_CLIPBOARD_SELECTION_SIGNAL_READY,
    ASTAL_CLIPBOARD_SELECTION_N_SIGNALS
} AstalClipboardSelectionSignals;

static guint astal_clipboard_selection_signals[ASTAL_CLIPBOARD_SELECTION_N_SIGNALS] = {
    0,
};
static GParamSpec* astal_clipboard_selection_properties[ASTAL_CLIPBOARD_SELECTION_N_PROPERTIES] = {
    NULL,
};

void astal_clipboard_clipboard_data_free(void* data) {
    if (data == NULL) return;
    AstalClipboardClipboardData* self = (AstalClipboardClipboardData*)data;
    g_free(self->mime_type);
    g_bytes_unref(self->data);
    g_free(self);
}

/**
 * astal_clipboard_clipboard_data_to_string:
 *
 * returns a string representation of the data. Note that this method does not check
 * if the data actually contains text using the mime_type field.
 *
 * Returns: (transfer full) (nullable)
 */
gchar* astal_clipboard_clipboard_data_to_string(AstalClipboardClipboardData* data) {
    if (data == NULL || data->data == NULL) {
        return NULL;
    }

    gsize size;
    const gchar* bytes = g_bytes_get_data(data->data, &size);

    gchar* string = g_malloc(size + 1);
    memcpy(string, bytes, size);
    string[size] = '\0';

    return string;
}

/**
 * astal_clipboard_clipboard_data_to_pixbuf:
 *
 * converts the data to a pixbuf. Note that this method does not check
 * if the data actually contains an image using the mime_type field.
 *
 * Returns: (transfer full) (nullable)
 */
GdkPixbuf* astal_clipboard_clipboard_data_to_pixbuf(AstalClipboardClipboardData* data) {
    if (data == NULL || data->data == NULL) {
        return NULL;
    }
    GInputStream* stream = g_memory_input_stream_new_from_bytes(data->data);
    GError* error = NULL;
    GdkPixbuf* pixbuf = gdk_pixbuf_new_from_stream(stream, NULL, &error);

    if (error != NULL) {
        g_warning("could not create Pixbuf: %s", error->message);
    }

    g_object_unref(stream);

    return pixbuf;
}

/**
 * astal_clipboard_selection_get_data:
 *
 * Returns: (transfer none) (element-type AstalClipboardClipboardData):
 */
GList* astal_clipboard_selection_get_data(AstalClipboardSelection* self) { return self->data; }

struct zwlr_data_control_offer_v1* astal_clipboard_selection_get_offer(
    AstalClipboardSelection* self) {
    AstalClipboardSelectionPrivate* priv = astal_clipboard_selection_get_instance_private(self);
    return priv->offer;
}

static void astal_clipboard_selection_get_property(GObject* object, guint property_id,
                                                   GValue* value, GParamSpec* pspec) {
    AstalClipboardSelection* self = ASTAL_CLIPBOARD_SELECTION(object);

    switch (property_id) {
        case ASTAL_CLIPBOARD_SELECTION_PROP_DATA:
            g_value_set_pointer(value, self->data);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void noop() {}

struct read_callback_data {
    AstalClipboardSelection* self;
    AstalClipboardClipboardData* data;
    GByteArray* read_data;
};

static void read_callback_data_free(void* data) {
    if (data == NULL) return;
    struct read_callback_data* rcd = data;
    g_object_unref(rcd->self);
    if (rcd->read_data != NULL) g_byte_array_free(rcd->read_data, TRUE);
    g_free(rcd);
}

static gboolean read_callback(GIOChannel* source, GIOCondition condition, gpointer data) {
    gchar buffer[1024];
    gsize bytes_read;
    GError* error = NULL;

    struct read_callback_data* rcd = data;
    AstalClipboardSelectionPrivate* priv =
        astal_clipboard_selection_get_instance_private(rcd->self);

    GIOStatus status =
        g_io_channel_read_chars(source, buffer, sizeof(buffer) - 1, &bytes_read, &error);
    if (status == G_IO_STATUS_ERROR) {
        g_critical("Error reading: %s", error->message);
        g_clear_error(&error);
        return FALSE;
    }

    g_byte_array_append(rcd->read_data, buffer, bytes_read);

    if (status == G_IO_STATUS_EOF) {
        rcd->data->data = g_byte_array_free_to_bytes(rcd->read_data);
        rcd->read_data = NULL;
        rcd->self->data = g_list_append(rcd->self->data, rcd->data);
        g_object_notify(G_OBJECT(rcd->self), "data");
        if (--priv->offer_counter == 0) {
            g_signal_emit_by_name(G_OBJECT(rcd->self), "ready");
            zwlr_data_control_offer_v1_destroy(priv->offer);
            priv->offer = NULL;
        }
        return FALSE;
    }

    return TRUE;
}

static void offer_handle_offer(void* data, struct zwlr_data_control_offer_v1* offer,
                               const char* mime_type) {
    AstalClipboardSelection* self = ASTAL_CLIPBOARD_SELECTION(data);
    AstalClipboardSelectionPrivate* priv = astal_clipboard_selection_get_instance_private(self);

    int pipes[2];
    GError* error = NULL;
    gboolean success = g_unix_open_pipe(pipes, O_NONBLOCK, &error);
    if (!success) {
        g_error("Failed to open pipe: %s\n", error->message);
        zwlr_data_control_offer_v1_destroy(offer);
        g_error_free(error);
        return;
    }

    ++priv->offer_counter;

    AstalClipboardClipboardData* cl_data = g_new0(AstalClipboardClipboardData, 1);
    cl_data->mime_type = g_strdup(mime_type);

    struct read_callback_data* rcd = g_malloc(sizeof(struct read_callback_data));
    rcd->self = g_object_ref(self);
    rcd->data = cl_data;
    rcd->read_data = g_byte_array_new();

    GIOChannel* channel = g_io_channel_unix_new(pipes[0]);
    g_io_channel_set_encoding(channel, NULL, NULL);
    g_io_channel_set_flags(channel, G_IO_FLAG_NONBLOCK, NULL);
    g_io_channel_set_close_on_unref(channel, TRUE);
    g_io_add_watch_full(channel, 0, G_IO_IN | G_IO_HUP, read_callback, rcd,
                        read_callback_data_free);

    zwlr_data_control_offer_v1_receive(offer, mime_type, pipes[1]);
    close(pipes[1]);
    g_io_channel_unref(channel);
}

static const struct zwlr_data_control_offer_v1_listener offer_listener = {.offer =
                                                                              offer_handle_offer};

AstalClipboardSelection* astal_clipboard_selection_new(struct zwlr_data_control_offer_v1* offer) {
    AstalClipboardSelection* self = g_object_new(ASTAL_CLIPBOARD_TYPE_SELECTION, NULL);
    AstalClipboardSelectionPrivate* priv = astal_clipboard_selection_get_instance_private(self);

    self->data = NULL;
    priv->offer = offer;

    zwlr_data_control_offer_v1_add_listener(offer, &offer_listener, self);
    return self;
}

static void astal_clipboard_selection_init(AstalClipboardSelection* self) {
    AstalClipboardSelectionPrivate* priv = astal_clipboard_selection_get_instance_private(self);
    priv->offer_counter = 0;
}

static void astal_clipboard_selection_finalize(GObject* object) {
    AstalClipboardSelection* self = ASTAL_CLIPBOARD_SELECTION(object);
    AstalClipboardSelectionPrivate* priv = astal_clipboard_selection_get_instance_private(self);

    if (priv->offer != NULL) zwlr_data_control_offer_v1_destroy(priv->offer);
    g_clear_list(&self->data, astal_clipboard_clipboard_data_free);

    G_OBJECT_CLASS(astal_clipboard_selection_parent_class)->finalize(object);
}

static void astal_clipboard_selection_class_init(AstalClipboardSelectionClass* class) {
    GObjectClass* object_class = G_OBJECT_CLASS(class);
    object_class->get_property = astal_clipboard_selection_get_property;
    object_class->finalize = astal_clipboard_selection_finalize;

    /**
     * AstalClipboardSelection:data: (type GList(AstalClipboardClipboardData))
     *
     * A list of [record@AstalClipboardClipboardData] objects.
     */
    astal_clipboard_selection_properties[ASTAL_CLIPBOARD_SELECTION_PROP_DATA] =
        g_param_spec_pointer("data", "data", "a list of the mime types of this selection",
                             G_PARAM_READABLE);

    g_object_class_install_properties(object_class, ASTAL_CLIPBOARD_SELECTION_N_PROPERTIES,
                                      astal_clipboard_selection_properties);

    /**
     * AstalClipboardSelection::ready: (skip)
     */
    astal_clipboard_selection_signals[ASTAL_CLIPBOARD_SELECTION_SIGNAL_READY] = g_signal_new(
        "ready", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL, NULL, G_TYPE_NONE, 0);
}
