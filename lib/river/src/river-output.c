#include <gio/gio.h>

#include "glib-object.h"
#include "glib.h"
#include "river-private.h"
#include "river-status-unstable-v1-client.h"

struct _AstalRiverOutput {
    GObject parent_instance;
    guint focused_tags;
    guint occupied_tags;
    guint urgent_tags;
    gchar* layout_name;
    gchar* focused_view;
    guint id;
    gchar* name;
};

typedef struct {
    struct zriver_status_manager_v1* river_status_manager;
    struct zriver_output_status_v1* river_output_status;
    struct zriver_control_v1* river_control;
    struct wl_seat* seat;
    struct wl_display* wl_display;
    struct wl_output* wl_output;
} AstalRiverOutputPrivate;

G_DEFINE_FINAL_TYPE_WITH_PRIVATE(AstalRiverOutput, astal_river_output, G_TYPE_OBJECT);

typedef enum {
    ASTAL_RIVER_OUTPUT_PROP_FOCUSED_TAGS = 1,
    ASTAL_RIVER_OUTPUT_PROP_OCCUPIED_TAGS,
    ASTAL_RIVER_OUTPUT_PROP_URGENT_TAGS,
    ASTAL_RIVER_OUTPUT_PROP_LAYOUT_NAME,
    ASTAL_RIVER_OUTPUT_PROP_NAME,
    ASTAL_RIVER_OUTPUT_PROP_FOCUSED_VIEW,
    ASTAL_RIVER_OUTPUT_PROP_ID,
    ASTAL_RIVER_OUTPUT_N_PROPERTIES
} AstalRiverOutputProperties;

typedef enum {
    ASTAL_RIVER_OUTPUT_SIGNAL_CHANGED,
    ASTAL_RIVER_OUTPUT_N_SIGNALS
} AstalRiverOutputSignals;

static guint astal_river_output_signals[ASTAL_RIVER_OUTPUT_N_SIGNALS] = {
    0,
};

static GParamSpec* astal_river_output_properties[ASTAL_RIVER_OUTPUT_N_PROPERTIES] = {
    NULL,
};

/**
 *  AstalRiverOutput
 *
 *  holds all the information associated with a monitor.
 */

/**
 * astal_river_output_get_id
 * @self: the AstalRiverOutput object
 *
 * the id of the underlying wl_output object
 *
 * Returns: the id of the underlying wl_output object
 */
guint astal_river_output_get_id(AstalRiverOutput* self) { return self->id; }

/**
 * astal_river_output_get_name
 * @self: the AstalRiverOutput object
 *
 * the name of the output
 *
 * Returns: (transfer none) (nullable): the name of the output
 */
gchar* astal_river_output_get_name(AstalRiverOutput* self) { return self->name; }

/**
 * astal_river_output_get_layout_name
 * @self: the AstalRiverOutput object
 *
 * the currently used layout name of the output
 *
 * Returns: (transfer none) (nullable): the currently used layout name of the output
 */
gchar* astal_river_output_get_layout_name(AstalRiverOutput* self) { return self->layout_name; }

/**
 * astal_river_output_get_focused_view
 * @self: the AstalRiverOutput object
 *
 * the focused view on the output
 *
 * Returns: (transfer none) (nullable): the focused view on the output
 */
gchar* astal_river_output_get_focused_view(AstalRiverOutput* self) { return self->focused_view; }

void astal_river_output_set_focused_view(AstalRiverOutput* self, const gchar* focused_view) {
    g_free(self->focused_view);
    self->focused_view = g_strdup(focused_view);
    g_object_notify(G_OBJECT(self), "focused-view");
    g_signal_emit(self, astal_river_output_signals[ASTAL_RIVER_OUTPUT_SIGNAL_CHANGED], 0);
}

/**
 * astal_river_output_set_focused_tags
 * @self: the AstalRiverOutput object
 * @tags: the tagmask to be focused
 *
 * sets the focused tags of the output
 *
 */
void astal_river_output_set_focused_tags(AstalRiverOutput* self, guint tags) { 
    AstalRiverOutputPrivate* priv = astal_river_output_get_instance_private(self);
    gchar *tagstring = g_strdup_printf("%i", tags);

    zriver_control_v1_add_argument(priv->river_control, "set-focused-tags");
    zriver_control_v1_add_argument(priv->river_control, tagstring);

    zriver_control_v1_run_command(priv->river_control, priv->seat);
    g_free(tagstring);
}

/**
 * astal_river_output_get_focused_tags
 * @self: the AstalRiverOutput object
 *
 * the focused tags of the output
 *
 * Returns: the focused tags of the output
 */
guint astal_river_output_get_focused_tags(AstalRiverOutput* self) { return self->focused_tags; }


/**
 * astal_river_output_get_urgent_tags
 * @self: the AstalRiverOutput object
 *
 * the urgent tags of the output
 *
 * Returns: the urgent tags of the output
 */
guint astal_river_output_get_urgent_tags(AstalRiverOutput* self) { return self->urgent_tags; }

/**
 * astal_river_output_get_occupied_tags
 * @self: the AstalRiverOutput object
 *
 * the occupied tags of the output
 *
 * Returns: the occupied tags of the output
 */
guint astal_river_output_get_occupied_tags(AstalRiverOutput* self) { return self->occupied_tags; }

struct wl_output* astal_river_output_get_wl_output(AstalRiverOutput* self) {
    AstalRiverOutputPrivate* priv = astal_river_output_get_instance_private(self);
    return priv->wl_output;
}

static void astal_river_output_get_property(GObject* object, guint property_id, GValue* value,
                                            GParamSpec* pspec) {
    AstalRiverOutput* self = ASTAL_RIVER_OUTPUT(object);

    switch (property_id) {
        case ASTAL_RIVER_OUTPUT_PROP_FOCUSED_TAGS:
            g_value_set_uint(value, self->focused_tags);
            break;
        case ASTAL_RIVER_OUTPUT_PROP_OCCUPIED_TAGS:
            g_value_set_uint(value, self->occupied_tags);
            break;
        case ASTAL_RIVER_OUTPUT_PROP_URGENT_TAGS:
            g_value_set_uint(value, self->urgent_tags);
            break;
        case ASTAL_RIVER_OUTPUT_PROP_ID:
            g_value_set_uint(value, self->id);
            break;
        case ASTAL_RIVER_OUTPUT_PROP_NAME:
            g_value_set_string(value, self->name);
            break;
        case ASTAL_RIVER_OUTPUT_PROP_LAYOUT_NAME:
            g_value_set_string(value, self->layout_name);
            break;
        case ASTAL_RIVER_OUTPUT_PROP_FOCUSED_VIEW:
            g_value_set_string(value, self->focused_view);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_river_output_set_property(GObject* object, guint property_id, const GValue* value,
                                            GParamSpec* pspec) {
    AstalRiverOutput* self = ASTAL_RIVER_OUTPUT(object);

    switch (property_id) {
        case ASTAL_RIVER_OUTPUT_PROP_ID:
            self->id = g_value_get_uint(value);
            g_object_notify(G_OBJECT(self), "id");
            break;
        case ASTAL_RIVER_OUTPUT_PROP_FOCUSED_TAGS:
            astal_river_output_set_focused_tags(self, g_value_get_uint(value));
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
    g_signal_emit(self, astal_river_output_signals[ASTAL_RIVER_OUTPUT_SIGNAL_CHANGED], 0);
}

static void noop() {}

static void astal_river_handle_focused_tags(void* data, struct zriver_output_status_v1* status,
                                            uint32_t tags) {
    AstalRiverOutput* self = ASTAL_RIVER_OUTPUT(data);
    self->focused_tags = tags;
    g_object_notify(G_OBJECT(self), "focused-tags");
    g_signal_emit(self, astal_river_output_signals[ASTAL_RIVER_OUTPUT_SIGNAL_CHANGED], 0);
}

static void astal_river_handle_urgent_tags(void* data, struct zriver_output_status_v1* status,
                                           uint32_t tags) {
    AstalRiverOutput* self = ASTAL_RIVER_OUTPUT(data);
    self->urgent_tags = tags;
    g_object_notify(G_OBJECT(self), "urgent-tags");
    g_signal_emit(self, astal_river_output_signals[ASTAL_RIVER_OUTPUT_SIGNAL_CHANGED], 0);
}

static void astal_river_handle_occupied_tags(void* data, struct zriver_output_status_v1* status,
                                             struct wl_array* view_tags) {
    AstalRiverOutput* self = ASTAL_RIVER_OUTPUT(data);
    guint tags = 0;
    guint* view;
    wl_array_for_each(view, view_tags) { tags |= *view; }

    self->occupied_tags = tags;
    g_object_notify(G_OBJECT(self), "occupied-tags");
    g_signal_emit(self, astal_river_output_signals[ASTAL_RIVER_OUTPUT_SIGNAL_CHANGED], 0);
}

static void astal_river_handle_layout_name(void* data, struct zriver_output_status_v1* status,
                                           const char* name) {
    AstalRiverOutput* self = ASTAL_RIVER_OUTPUT(data);
    g_free(self->layout_name);
    self->layout_name = g_strdup(name);
    g_object_notify(G_OBJECT(self), "layout-name");
    g_signal_emit(self, astal_river_output_signals[ASTAL_RIVER_OUTPUT_SIGNAL_CHANGED], 0);
}

static void astal_river_handle_layout_name_clear(void* data,
                                                 struct zriver_output_status_v1* status) {
    AstalRiverOutput* self = ASTAL_RIVER_OUTPUT(data);
    g_free(self->layout_name);
    self->layout_name = NULL;
    g_object_notify(G_OBJECT(self), "layout-name");
    g_signal_emit(self, astal_river_output_signals[ASTAL_RIVER_OUTPUT_SIGNAL_CHANGED], 0);
}

static void astal_river_wl_output_handle_name(void* data, struct wl_output* output,
                                              const char* name) {
    AstalRiverOutput* self = ASTAL_RIVER_OUTPUT(data);
    g_free(self->name);
    self->name = g_strdup(name);
    g_object_notify(G_OBJECT(self), "name");
    g_signal_emit(self, astal_river_output_signals[ASTAL_RIVER_OUTPUT_SIGNAL_CHANGED], 0);
}

static const struct zriver_output_status_v1_listener output_status_listener = {
    .focused_tags = astal_river_handle_focused_tags,
    .view_tags = astal_river_handle_occupied_tags,
    .urgent_tags = astal_river_handle_urgent_tags,
    .layout_name = astal_river_handle_layout_name,
    .layout_name_clear = astal_river_handle_layout_name_clear,
};

static const struct wl_output_listener wl_output_listener = {
    .name = astal_river_wl_output_handle_name,
    .geometry = noop,
    .mode = noop,
    .scale = noop,
    .description = noop,
    .done = noop,
};

static void astal_river_output_finalize(GObject* object) {
    AstalRiverOutput* self = ASTAL_RIVER_OUTPUT(object);
    AstalRiverOutputPrivate* priv = astal_river_output_get_instance_private(self);

    zriver_output_status_v1_destroy(priv->river_output_status);
    wl_output_destroy(priv->wl_output);

    wl_display_roundtrip(priv->wl_display);

    g_free(self->layout_name);
    g_free(self->name);

    G_OBJECT_CLASS(astal_river_output_parent_class)->finalize(object);
}

static void astal_river_output_init(AstalRiverOutput* self) {}

AstalRiverOutput* astal_river_output_new(guint id, struct wl_output* wl_output,
                                         struct zriver_status_manager_v1* status_manager,
                                         struct zriver_control_v1* river_control,
                                         struct wl_seat* seat,
                                         struct wl_display* wl_display) {
    AstalRiverOutput* self = g_object_new(ASTAL_RIVER_TYPE_OUTPUT, NULL);
    AstalRiverOutputPrivate* priv = astal_river_output_get_instance_private(self);

    self->id = id;
    priv->wl_display = wl_display;
    priv->wl_output = wl_output;
    priv->river_status_manager = status_manager;
    priv->river_control = river_control;
    priv->seat = seat;

    priv->river_output_status =
        zriver_status_manager_v1_get_river_output_status(priv->river_status_manager, wl_output);

    zriver_output_status_v1_add_listener(priv->river_output_status, &output_status_listener, self);

    wl_output_add_listener(wl_output, &wl_output_listener, self);

    wl_display_roundtrip(priv->wl_display);

    return self;
}

static void astal_river_output_class_init(AstalRiverOutputClass* class) {
    GObjectClass* object_class = G_OBJECT_CLASS(class);
    object_class->get_property = astal_river_output_get_property;
    object_class->set_property = astal_river_output_set_property;
    object_class->finalize = astal_river_output_finalize;
    /**
     * AstalRiverOutput:focused-tags:
     *
     * The currently focused tags
     */
    astal_river_output_properties[ASTAL_RIVER_OUTPUT_PROP_FOCUSED_TAGS] = g_param_spec_uint(
        "focused-tags", "focused-tags", "currently focused tags", 0, INT_MAX, 0, G_PARAM_READWRITE);
    /**
     * AstalRiverOutput:occupied-tags:
     *
     * The currently occupied tags
     */
    astal_river_output_properties[ASTAL_RIVER_OUTPUT_PROP_OCCUPIED_TAGS] =
        g_param_spec_uint("occupied-tags", "occupied-tags", "currently occupied tags", 0, INT_MAX,
                          0, G_PARAM_READABLE);
    /**
     * AstalRiverOutput:urgent-tags:
     *
     * The currently tags marked as urgent
     */
    astal_river_output_properties[ASTAL_RIVER_OUTPUT_PROP_URGENT_TAGS] = g_param_spec_uint(
        "urgent-tags", "urgent-tags", "currently urgent tags", 0, INT_MAX, 0, G_PARAM_READABLE);
    /**
     * AstalRiverOutput:id:
     *
     * The id of the underlying wl_output object
     */
    astal_river_output_properties[ASTAL_RIVER_OUTPUT_PROP_ID] =
        g_param_spec_uint("id", "id", "id of the output object", 0, INT_MAX, 0, G_PARAM_READABLE);
    /**
     * AstalRiverOutput:layout-name:
     *
     * The name of active layout
     */
    astal_river_output_properties[ASTAL_RIVER_OUTPUT_PROP_LAYOUT_NAME] = g_param_spec_string(
        "layout-name", "layout-name", "name of the current layout", NULL, G_PARAM_READABLE);
    /**
     * AstalRiverOutput:name:
     *
     * The name of this output
     */
    astal_river_output_properties[ASTAL_RIVER_OUTPUT_PROP_NAME] =
        g_param_spec_string("name", "name", "name of the output", NULL, G_PARAM_READABLE);
    /**
     * AstalRiverOutput:focused-view:
     *
     * The name of currently focused view
     */
    astal_river_output_properties[ASTAL_RIVER_OUTPUT_PROP_FOCUSED_VIEW] =
        g_param_spec_string("focused-view", "focused-view",
                            "name of last focused view on this output", NULL, G_PARAM_READABLE);

    g_object_class_install_properties(object_class, ASTAL_RIVER_OUTPUT_N_PROPERTIES,
                                      astal_river_output_properties);

    astal_river_output_signals[ASTAL_RIVER_OUTPUT_SIGNAL_CHANGED] =
        g_signal_new("changed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL, NULL,
                     G_TYPE_NONE, 0);
}
