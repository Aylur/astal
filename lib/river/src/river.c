#include <gio/gio.h>
#include <json-glib/json-glib.h>
#include <wayland-client-protocol.h>
#include <wayland-client.h>

#include "river-control-unstable-v1-client.h"
#include "river-private.h"
#include "river-status-unstable-v1-client.h"
// #include "wayland-source.h"
#include <wayland-glib.h>

struct _AstalRiverRiver {
    GObject parent_instance;
    GList* outputs;
    gchar* focused_output;
    gchar* focused_view;
    gchar* mode;
};

typedef struct {
    GHashTable* signal_ids;
    gboolean init;
    struct wl_registry* wl_registry;
    struct wl_seat* seat;
    struct wl_display* display;
    WlGlibWlSource* wl_source;
    struct zriver_status_manager_v1* river_status_manager;
    struct zriver_control_v1* river_control;
    struct zriver_seat_status_v1* river_seat_status;
} AstalRiverRiverPrivate;

static JsonSerializableIface* serializable_iface = NULL;
static void astal_river_river_initable_iface_init(GInitableIface* iface);
static void astal_river_river_json_serializable_iface_init(JsonSerializableIface* g_iface);

G_DEFINE_TYPE_WITH_CODE(AstalRiverRiver, astal_river_river, G_TYPE_OBJECT,
                        G_ADD_PRIVATE(AstalRiverRiver) G_IMPLEMENT_INTERFACE(
                            G_TYPE_INITABLE, astal_river_river_initable_iface_init)
                            G_IMPLEMENT_INTERFACE(JSON_TYPE_SERIALIZABLE,
                                                  astal_river_river_json_serializable_iface_init))

typedef enum {
    ASTAL_RIVER_RIVER_PROP_FOCUSED_OUTPUT = 1,
    ASTAL_RIVER_RIVER_PROP_FOCUSED_VIEW,
    ASTAL_RIVER_RIVER_PROP_MODE,
    ASTAL_RIVER_RIVER_PROP_OUTPUTS,
    ASTAL_RIVER_RIVER_N_PROPERTIES
} AstalRiverRiverProperties;

typedef enum {
    ASTAL_RIVER_RIVER_SIGNAL_CHANGED,
    ASTAL_RIVER_RIVER_SIGNAL_OUTPUT_ADDED,
    ASTAL_RIVER_RIVER_SIGNAL_OUTPUT_REMOVED,
    ASTAL_RIVER_RIVER_N_SIGNALS
} AstalRiverRiverSignals;

static guint astal_river_river_signals[ASTAL_RIVER_RIVER_N_SIGNALS] = {
    0,
};
static GParamSpec* astal_river_river_properties[ASTAL_RIVER_RIVER_N_PROPERTIES] = {
    NULL,
};

static void reemit_changed(AstalRiverOutput* output, AstalRiverRiver* self) {
    g_signal_emit(self, astal_river_river_signals[ASTAL_RIVER_RIVER_SIGNAL_CHANGED], 0);
}

static AstalRiverOutput* find_output_by_id(AstalRiverRiver* self, uint32_t id) {
    GList* output = self->outputs;
    while (output != NULL) {
        AstalRiverOutput* river_output = output->data;
        if (astal_river_output_get_id(river_output) == id) return river_output;
        output = output->next;
    }
    return NULL;
}

static AstalRiverOutput* find_output_by_output(AstalRiverRiver* self, struct wl_output* wl_output) {
    GList* output = self->outputs;
    while (output != NULL) {
        AstalRiverOutput* river_output = output->data;
        if (astal_river_output_get_wl_output(river_output) == wl_output) return river_output;
        output = output->next;
    }
    return NULL;
}

static AstalRiverOutput* find_output_by_name(AstalRiverRiver* self, gchar* name) {
    GList* output = self->outputs;
    while (output != NULL) {
        AstalRiverOutput* river_output = output->data;
        if (strcmp(astal_river_output_get_name(river_output), name) == 0) return river_output;
        output = output->next;
    }
    return NULL;
}

/**
 *  AstalRiverRiver
 *
 *  This class creates a connection to the river compositor.
 */

/**
 * astal_river_river_get_outputs
 * @self: the AstalRiverRiver object
 *
 * returns a list of all outputs
 *
 * Returns: (transfer none) (element-type AstalRiver.Output): a list of all outputs
 *
 */
GList* astal_river_river_get_outputs(AstalRiverRiver* self) { return self->outputs; }

/**
 * astal_river_river_get_output
 * @self: the AstalRiverRiver object
 * @name: the name of the output
 *
 * returns the output with the given name or null
 *
 * Returns: (transfer none) (nullable): the output with the given name or null
 */
AstalRiverOutput* astal_river_river_get_output(AstalRiverRiver* self, gchar* name) {
    return find_output_by_name(self, name);
}

/**
 * astal_river_river_get_focused_view
 * @self: the AstalRiverOutput object
 *
 * returns the currently focused view
 *
 * Returns: (transfer none) (nullable): the currently focused view
 */
gchar* astal_river_river_get_focused_view(AstalRiverRiver* self) { return self->focused_view; }

/**
 * astal_river_river_get_focused_output
 * @self: the AstalRiverOutput object
 *
 * returns the name of the currently focused output
 *
 * Returns: (transfer none) (nullable): the name of the currently focused output
 */
gchar* astal_river_river_get_focused_output(AstalRiverRiver* self) { return self->focused_output; }

/**
 * astal_river_river_get_mode
 * @self: the AstalRiverOutput object
 *
 * returns the currently active mode
 *
 * Returns: (transfer none) (nullable): the currently active mode
 */
gchar* astal_river_river_get_mode(AstalRiverRiver* self) { return self->mode; }

static void astal_river_river_get_property(GObject* object, guint property_id, GValue* value,
                                           GParamSpec* pspec) {
    AstalRiverRiver* self = ASTAL_RIVER_RIVER(object);

    switch (property_id) {
        case ASTAL_RIVER_RIVER_PROP_MODE:
            g_value_set_string(value, self->mode);
            break;
        case ASTAL_RIVER_RIVER_PROP_FOCUSED_VIEW:
            g_value_set_string(value, self->focused_view);
            break;
        case ASTAL_RIVER_RIVER_PROP_FOCUSED_OUTPUT:
            g_value_set_string(value, self->focused_output);
            break;
        case ASTAL_RIVER_RIVER_PROP_OUTPUTS:
            g_value_set_pointer(value, self->outputs);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static JsonNode* astal_river_river_serialize_property(JsonSerializable* serializable,
                                                      const gchar* name, const GValue* value,
                                                      GParamSpec* pspec) {
    JsonNode* retval = NULL;
    if (strcmp(name, "outputs") == 0) {
        JsonArray* outputs = json_array_new();
        retval = json_node_new(JSON_NODE_ARRAY);
        GList* output = g_value_get_pointer(value);
        while (output != NULL) {
            AstalRiverOutput* river_output = output->data;
            json_array_add_element(outputs, json_gobject_serialize(G_OBJECT(river_output)));
            output = output->next;
        }
        json_node_take_array(retval, outputs);
    } else
        retval = serializable_iface->serialize_property(serializable, name, value, pspec);

    return retval;
}

static void noop() {}

static void river_seat_status_handle_focused_output(void* data,
                                                    struct zriver_seat_status_v1* seat_status,
                                                    struct wl_output* focused_output) {
    AstalRiverRiver* self = ASTAL_RIVER_RIVER(data);
    g_free(self->focused_output);
    AstalRiverOutput* output = find_output_by_output(self, focused_output);
    if (output == NULL) return;
    self->focused_output = g_strdup(astal_river_output_get_name(output));
    g_object_notify(G_OBJECT(self), "focused-output");
    g_signal_emit(self, astal_river_river_signals[ASTAL_RIVER_RIVER_SIGNAL_CHANGED], 0);
}

static void river_seat_status_handle_focused_view(void* data,
                                                  struct zriver_seat_status_v1* seat_status,
                                                  const char* focused_view) {
    AstalRiverRiver* self = ASTAL_RIVER_RIVER(data);
    g_free(self->focused_view);
    self->focused_view = g_strdup(focused_view);
    AstalRiverOutput* output = find_output_by_name(self, self->focused_output);
    astal_river_output_set_focused_view(output, focused_view);
    g_object_notify(G_OBJECT(self), "focused-view");
    g_signal_emit(self, astal_river_river_signals[ASTAL_RIVER_RIVER_SIGNAL_CHANGED], 0);
}

static void river_seat_status_handle_mode(void* data, struct zriver_seat_status_v1* seat_status,
                                          const char* mode) {
    AstalRiverRiver* self = ASTAL_RIVER_RIVER(data);
    g_free(self->mode);
    self->mode = g_strdup(mode);
    g_object_notify(G_OBJECT(self), "mode");
    g_signal_emit(self, astal_river_river_signals[ASTAL_RIVER_RIVER_SIGNAL_CHANGED], 0);
}

static const struct zriver_seat_status_v1_listener river_seat_status_listener = {
    .focused_output = river_seat_status_handle_focused_output,
    .unfocused_output = noop,
    .focused_view = river_seat_status_handle_focused_view,
    .mode = river_seat_status_handle_mode,
};

static void global_registry_handler(void* data, struct wl_registry* registry, uint32_t id,
                                    const char* interface, uint32_t version) {
    AstalRiverRiver* self = ASTAL_RIVER_RIVER(data);
    AstalRiverRiverPrivate* priv = astal_river_river_get_instance_private(self);
    if (strcmp(interface, wl_output_interface.name) == 0) {
        if (priv->river_status_manager == NULL) return;
        struct wl_output* wl_out = wl_registry_bind(registry, id, &wl_output_interface, 4);
        AstalRiverOutput* output = astal_river_output_new(
            id, wl_out, priv->river_status_manager, priv->river_control, priv->seat, priv->display);

        self->outputs = g_list_append(self->outputs, output);
        g_object_notify(G_OBJECT(self), "outputs");
        g_signal_emit(G_OBJECT(self),
                      astal_river_river_signals[ASTAL_RIVER_RIVER_SIGNAL_OUTPUT_ADDED], 0,
                      astal_river_output_get_name(output));
        g_signal_emit(self, astal_river_river_signals[ASTAL_RIVER_RIVER_SIGNAL_CHANGED], 0);

        guint signal_id = g_signal_connect(output, "changed", G_CALLBACK(reemit_changed), self);
        g_hash_table_insert(priv->signal_ids, GUINT_TO_POINTER(id), GUINT_TO_POINTER(signal_id));
    } else if (strcmp(interface, wl_seat_interface.name) == 0) {
        priv->seat = wl_registry_bind(registry, id, &wl_seat_interface, 4);
    } else if (strcmp(interface, zriver_status_manager_v1_interface.name) == 0) {
        priv->river_status_manager =
            wl_registry_bind(registry, id, &zriver_status_manager_v1_interface, 4);
    } else if (strcmp(interface, zriver_control_v1_interface.name) == 0) {
        priv->river_control = wl_registry_bind(registry, id, &zriver_control_v1_interface, 1);
    }
}

static void astal_river_river_callback_success(void* data, struct zriver_command_callback_v1* cb,
                                               const char* msg) {
    AstalRiverCommandCallback callback = (AstalRiverCommandCallback)(data);
    callback(TRUE, msg);
}

static void astal_river_river_callback_failure(void* data, struct zriver_command_callback_v1* cb,
                                               const char* msg) {
    AstalRiverCommandCallback callback = (AstalRiverCommandCallback)(data);
    callback(FALSE, msg);
}

const struct zriver_command_callback_v1_listener cb_listener = {
    .success = astal_river_river_callback_success, .failure = astal_river_river_callback_failure};

/**
 * astal_river_river_run_command_async:
 * @self: the AstalRiverRiver object
 * @length: the length of the cmd array
 * @cmd: (array length=length): the command to execute
 * @callback: (scope async) (nullable): the callback to invoke.
 *
 * Sends a given command to the compositor and calls the callback after it was executed.
 */
void astal_river_river_run_command_async(AstalRiverRiver* self, gint length, const gchar** cmd,
                                         AstalRiverCommandCallback callback) {
    AstalRiverRiverPrivate* priv = astal_river_river_get_instance_private(self);

    for (gint i = 0; i < length; ++i) {
        zriver_control_v1_add_argument(priv->river_control, cmd[i]);
    }

    struct zriver_command_callback_v1* cb =
        zriver_control_v1_run_command(priv->river_control, priv->seat);
    if (callback != NULL) zriver_command_callback_v1_add_listener(cb, &cb_listener, callback);
}

static void global_registry_remover(void* data, struct wl_registry* registry, uint32_t id) {
    AstalRiverRiver* self = ASTAL_RIVER_RIVER(data);
    AstalRiverRiverPrivate* priv = astal_river_river_get_instance_private(self);
    AstalRiverOutput* output = find_output_by_id(self, id);
    if (output != NULL) {
        guint signal_id =
            GPOINTER_TO_UINT(g_hash_table_lookup(priv->signal_ids, GUINT_TO_POINTER(id)));
        g_hash_table_remove(priv->signal_ids, GUINT_TO_POINTER(id));
        g_signal_handler_disconnect(output, signal_id);
        g_signal_emit(G_OBJECT(self),
                      astal_river_river_signals[ASTAL_RIVER_RIVER_SIGNAL_OUTPUT_REMOVED], 0,
                      astal_river_output_get_name(output));
        self->outputs = g_list_remove(self->outputs, output);
        g_object_notify(G_OBJECT(self), "outputs");
        g_object_unref(output);
        return;
    }
    g_signal_emit(self, astal_river_river_signals[ASTAL_RIVER_RIVER_SIGNAL_CHANGED], 0);
}

static const struct wl_registry_listener registry_listener = {global_registry_handler,
                                                              global_registry_remover};

static void astal_river_river_json_serializable_iface_init(JsonSerializableIface* iface) {
    iface->serialize_property = astal_river_river_serialize_property;
    serializable_iface = g_type_default_interface_peek(JSON_TYPE_SERIALIZABLE);
}

static gboolean astal_river_river_initable_init(GInitable* initable, GCancellable* cancellable,
                                                GError** error) {
    AstalRiverRiver* self = ASTAL_RIVER_RIVER(initable);
    AstalRiverRiverPrivate* priv = astal_river_river_get_instance_private(self);

    if (priv->init) return TRUE;

    priv->wl_source = wl_glib_wl_source_new();

    if (priv->wl_source == NULL) {
        g_set_error_literal(error, G_IO_ERROR, G_IO_ERROR_FAILED,
                            "Can not connect to wayland display");
        return FALSE;
    }

    priv->display = priv->wl_source->display;

    priv->wl_registry = wl_display_get_registry(priv->display);
    wl_registry_add_listener(priv->wl_registry, &registry_listener, self);

    wl_display_roundtrip(priv->display);

    if (priv->river_status_manager == NULL) {
        g_set_error_literal(error, G_IO_ERROR, G_IO_ERROR_FAILED,
                            "Can not connect river status protocol");
        return FALSE;
    }

    priv->river_seat_status =
        zriver_status_manager_v1_get_river_seat_status(priv->river_status_manager, priv->seat);
    zriver_seat_status_v1_add_listener(priv->river_seat_status, &river_seat_status_listener, self);

    wl_display_roundtrip(priv->display);

    priv->init = TRUE;
    return TRUE;
}

static void astal_river_river_constructed(GObject* object) {
    astal_river_river_initable_init(G_INITABLE(object), NULL, NULL);
}

static void astal_river_river_initable_iface_init(GInitableIface* iface) {
    iface->init = astal_river_river_initable_init;
}

static void astal_river_river_init(AstalRiverRiver* self) {
    AstalRiverRiverPrivate* priv = astal_river_river_get_instance_private(self);
    self->outputs = NULL;
    priv->init = FALSE;
    priv->seat = NULL;
    priv->display = NULL;
    priv->river_status_manager = NULL;
    priv->signal_ids = g_hash_table_new(g_direct_hash, g_direct_equal);
}

/**
 * astal_river_river_new
 *
 * creates a new River object. It is recommended to use the [func@AstalRiver.get_default] method
 * instead of this method.
 *
 * Returns: (nullable): a newly created connection to river
 */
AstalRiverRiver* astal_river_river_new() {
    return g_initable_new(ASTAL_RIVER_TYPE_RIVER, NULL, NULL, NULL);
}

static void disconnect_signal(gpointer key, gpointer value, gpointer user_data) {
    AstalRiverRiver* self = ASTAL_RIVER_RIVER(user_data);

    AstalRiverOutput* output = find_output_by_id(self, GPOINTER_TO_UINT(key));
    g_signal_handler_disconnect(output, GPOINTER_TO_UINT(value));
}

/**
 * astal_river_river_get_default
 *
 * returns the default River object.
 *
 * Returns: (nullable) (transfer none): gets the default River object.
 */
AstalRiverRiver* astal_river_river_get_default() {
    static AstalRiverRiver* self = NULL;

    if (self == NULL) self = astal_river_river_new();

    return self;
}

/**
 * astal_river_get_default
 *
 * Returns: (nullable) (transfer none): gets the default River object.
 */
AstalRiverRiver* astal_river_get_default() { return astal_river_river_get_default(); }

static void astal_river_river_finalize(GObject* object) {
    AstalRiverRiver* self = ASTAL_RIVER_RIVER(object);
    AstalRiverRiverPrivate* priv = astal_river_river_get_instance_private(self);

    g_hash_table_foreach(priv->signal_ids, disconnect_signal, self);
    g_hash_table_destroy(priv->signal_ids);

    if (priv->display != NULL) wl_display_roundtrip(priv->display);

    g_clear_list(&self->outputs, g_object_unref);
    self->outputs = NULL;

    if (priv->wl_registry != NULL) wl_registry_destroy(priv->wl_registry);
    if (priv->river_status_manager != NULL)
        zriver_status_manager_v1_destroy(priv->river_status_manager);
    if (priv->river_seat_status != NULL) zriver_seat_status_v1_destroy(priv->river_seat_status);
    if (priv->seat != NULL) wl_seat_destroy(priv->seat);
    if (priv->display != NULL) wl_display_flush(priv->display);

    // if (priv->wl_source != NULL) wl_source_free(priv->wl_source);
    if (priv->wl_source != NULL) g_source_unref((GSource*)(priv->wl_source));

    g_free(self->focused_view);
    g_free(self->focused_output);
    g_free(self->mode);

    G_OBJECT_CLASS(astal_river_river_parent_class)->finalize(object);
}

static void astal_river_river_class_init(AstalRiverRiverClass* class) {
    GObjectClass* object_class = G_OBJECT_CLASS(class);
    object_class->get_property = astal_river_river_get_property;
    object_class->finalize = astal_river_river_finalize;
    object_class->constructed = astal_river_river_constructed;

    /**
     * AstalRiverRiver:mode:
     *
     * The currently active mode
     */
    astal_river_river_properties[ASTAL_RIVER_RIVER_PROP_MODE] =
        g_param_spec_string("mode", "mode", "currently active mode", NULL, G_PARAM_READABLE);
    /**
     * AstalRiverRiver:focused-view:
     *
     * The name of the currently focused view
     */
    astal_river_river_properties[ASTAL_RIVER_RIVER_PROP_FOCUSED_VIEW] = g_param_spec_string(
        "focused-view", "focused-view", "currently focused view", NULL, G_PARAM_READABLE);
    /**
     * AstalRiverRiver:focused-output:
     *
     * The name of the currently focused output
     */
    astal_river_river_properties[ASTAL_RIVER_RIVER_PROP_FOCUSED_OUTPUT] = g_param_spec_string(
        "focused-output", "focused-output", "currently focused-output", NULL, G_PARAM_READABLE);
    /**
     * AstalRiverRiver:outputs: (type GList(AstalRiverOutput))
     *
     * A list of [class@AstalRiver.Output] objects
     */
    astal_river_river_properties[ASTAL_RIVER_RIVER_PROP_OUTPUTS] =
        g_param_spec_pointer("outputs", "outputs", "a list of all outputs", G_PARAM_READABLE);

    g_object_class_install_properties(object_class, ASTAL_RIVER_RIVER_N_PROPERTIES,
                                      astal_river_river_properties);
    /**
     * AstalRiverRiver::output-added:
     * @river: the object which received the signal.
     * @output: the name of the added output
     *
     * This signal is emitted when a new output was connected
     */
    astal_river_river_signals[ASTAL_RIVER_RIVER_SIGNAL_OUTPUT_ADDED] =
        g_signal_new("output-added", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, G_TYPE_STRING);
    /**
     * AstalRiverRiver::output-removed:
     * @river: the object which received the signal.
     * @output: the name of the removed output
     *
     * This signal is emitted when a new output was disconnected
     */
    astal_river_river_signals[ASTAL_RIVER_RIVER_SIGNAL_OUTPUT_REMOVED] =
        g_signal_new("output-removed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, G_TYPE_STRING);

    astal_river_river_signals[ASTAL_RIVER_RIVER_SIGNAL_CHANGED] =
        g_signal_new("changed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL, NULL,
                     G_TYPE_NONE, 0);
}
