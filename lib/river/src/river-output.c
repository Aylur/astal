#include <gio/gio.h>
#include <stdint.h>
#include <wayland-client-protocol.h>

#include "astal-river.h.in"
#include "glib-object.h"
#include "glib.h"
#include "river-private.h"
#include "river-status-unstable-v1-client.h"

G_DEFINE_ENUM_TYPE(
    AstalRiverTransform, astal_river_transform,
    G_DEFINE_ENUM_VALUE(ASTAL_RIVER_TRANSFORM_NORMAL, "normal"),
    G_DEFINE_ENUM_VALUE(ASTAL_RIVER_TRANSFORM_ROTATE_90_DEG, "rotate-90"),
    G_DEFINE_ENUM_VALUE(ASTAL_RIVER_TRANSFORM_ROTATE_180_DEG, "rotate-180"),
    G_DEFINE_ENUM_VALUE(ASTAL_RIVER_TRANSFORM_ROTATE_270_DEG, "rotate-270"),
    G_DEFINE_ENUM_VALUE(ASTAL_RIVER_TRANSFORM_FLIPPED, "flipped"),
    G_DEFINE_ENUM_VALUE(ASTAL_RIVER_TRANSFORM_FLIPPED_ROTATE_90_DEG, "flipped-rotate-90"),
    G_DEFINE_ENUM_VALUE(ASTAL_RIVER_TRANSFORM_FLIPPED_ROTATE_180_DEG, "flipped-rotate-180"),
    G_DEFINE_ENUM_VALUE(ASTAL_RIVER_TRANSFORM_FLIPPED_ROTATE_270_DEG, "flipped-rotate-270"));

struct _AstalRiverOutput {
    GObject parent_instance;
    guint focused_tags;
    guint occupied_tags;
    guint urgent_tags;
    gchar* layout_name;
    gchar* focused_view;
    guint id;

    gchar* name;
    gchar* description;
    gchar* make;
    gchar* model;

    gint scale_factor;
    gint x;
    gint y;
    gint width;
    gint height;
    gint physical_width;
    gint physical_height;
    gdouble refresh_rate;

    AstalRiverTransform transform;
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
    ASTAL_RIVER_OUTPUT_PROP_DESCRIPTION,
    ASTAL_RIVER_OUTPUT_PROP_MAKE,
    ASTAL_RIVER_OUTPUT_PROP_MODEL,
    ASTAL_RIVER_OUTPUT_PROP_X,
    ASTAL_RIVER_OUTPUT_PROP_Y,
    ASTAL_RIVER_OUTPUT_PROP_WIDTH,
    ASTAL_RIVER_OUTPUT_PROP_HEIGHT,
    ASTAL_RIVER_OUTPUT_PROP_PHYSICAL_WIDTH,
    ASTAL_RIVER_OUTPUT_PROP_PHYSICAL_HEIGHT,
    ASTAL_RIVER_OUTPUT_PROP_TRANSFORM,
    ASTAL_RIVER_OUTPUT_PROP_REFRESH_RATE,
    ASTAL_RIVER_OUTPUT_PROP_SCALE_FACTOR,
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
    gchar* tagstring = g_strdup_printf("%i", tags);

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

/**
 * astal_river_output_get_description
 * @self: the AstalRiverOutput object
 *
 * the description of the output
 */
const gchar* astal_river_output_get_description(AstalRiverOutput* self) {
    return self->description;
}

/**
 * astal_river_output_get_make
 * @self: the AstalRiverOutput object
 *
 * the make of the output
 */
const gchar* astal_river_output_get_make(AstalRiverOutput* self) { return self->make; }

/**
 * astal_river_output_get_model
 * @self: the AstalRiverOutput object
 *
 * the model of the output
 */
const gchar* astal_river_output_get_model(AstalRiverOutput* self) { return self->model; }

/**
 * astal_river_output_get_x
 * @self: the AstalRiverOutput object
 *
 * the x coordinate of the outputs position
 */
gint astal_river_output_get_x(AstalRiverOutput* self) { return self->x; }

/**
 * astal_river_output_get_y
 * @self: the AstalRiverOutput object
 *
 * the y coordinate of the outputs position
 */
gint astal_river_output_get_y(AstalRiverOutput* self) { return self->y; }

/**
 * astal_river_output_get_width
 * @self: the AstalRiverOutput object
 *
 * the width of the output
 */
gint astal_river_output_get_width(AstalRiverOutput* self) { return self->width; }

/**
 * astal_river_output_get_height
 * @self: the AstalRiverOutput object
 *
 * the height of the output
 */
gint astal_river_output_get_height(AstalRiverOutput* self) { return self->height; }

/**
 * astal_river_output_get_physical_width
 * @self: the AstalRiverOutput object
 *
 * the physical width of the output
 */
gint astal_river_output_get_physical_width(AstalRiverOutput* self) { return self->physical_width; }

/**
 * astal_river_output_get_physical_height
 * @self: the AstalRiverOutput object
 *
 * the physical height of the output
 */
gint astal_river_output_get_physical_height(AstalRiverOutput* self) {
    return self->physical_height;
}

/**
 * astal_river_output_get_scale_factor
 * @self: the AstalRiverOutput object
 *
 * the scale factor of the output
 */
gint astal_river_output_get_scale_factor(AstalRiverOutput* self) { return self->scale_factor; }

/**
 * astal_river_output_get_refresh_rate
 * @self: the AstalRiverOutput object
 *
 * the refresh rate of the output
 */
gdouble astal_river_output_get_refresh_rate(AstalRiverOutput* self) { return self->refresh_rate; }

/**
 * astal_river_output_get_transform
 * @self: the AstalRiverOutput object
 *
 * the transform of the output
 */
AstalRiverTransform astal_river_output_get_transform(AstalRiverOutput* self) {
    return self->transform;
}

struct wl_output* astal_river_output_get_wl_output(AstalRiverOutput* self) {
    AstalRiverOutputPrivate* priv = astal_river_output_get_instance_private(self);
    return priv->wl_output;
}

static void astal_river_output_get_property(GObject* object, guint property_id, GValue* value,
                                            GParamSpec* pspec) {
    AstalRiverOutput* self = ASTAL_RIVER_OUTPUT(object);

    switch (property_id) {
        case ASTAL_RIVER_OUTPUT_PROP_FOCUSED_TAGS:
            g_value_set_uint(value, astal_river_output_get_focused_tags(self));
            break;
        case ASTAL_RIVER_OUTPUT_PROP_OCCUPIED_TAGS:
            g_value_set_uint(value, astal_river_output_get_occupied_tags(self));
            break;
        case ASTAL_RIVER_OUTPUT_PROP_URGENT_TAGS:
            g_value_set_uint(value, astal_river_output_get_urgent_tags(self));
            break;
        case ASTAL_RIVER_OUTPUT_PROP_ID:
            g_value_set_uint(value, astal_river_output_get_id(self));
            break;
        case ASTAL_RIVER_OUTPUT_PROP_NAME:
            g_value_set_string(value, astal_river_output_get_name(self));
            break;
        case ASTAL_RIVER_OUTPUT_PROP_LAYOUT_NAME:
            g_value_set_string(value, astal_river_output_get_layout_name(self));
            break;
        case ASTAL_RIVER_OUTPUT_PROP_FOCUSED_VIEW:
            g_value_set_string(value, astal_river_output_get_focused_view(self));
            break;
        case ASTAL_RIVER_OUTPUT_PROP_DESCRIPTION:
            g_value_set_string(value, astal_river_output_get_description(self));
            break;
        case ASTAL_RIVER_OUTPUT_PROP_MAKE:
            g_value_set_string(value, astal_river_output_get_make(self));
            break;
        case ASTAL_RIVER_OUTPUT_PROP_MODEL:
            g_value_set_string(value, astal_river_output_get_model(self));
            break;
        case ASTAL_RIVER_OUTPUT_PROP_X:
            g_value_set_int(value, astal_river_output_get_x(self));
            break;
        case ASTAL_RIVER_OUTPUT_PROP_Y:
            g_value_set_int(value, astal_river_output_get_y(self));
            break;
        case ASTAL_RIVER_OUTPUT_PROP_WIDTH:
            g_value_set_int(value, astal_river_output_get_width(self));
            break;
        case ASTAL_RIVER_OUTPUT_PROP_HEIGHT:
            g_value_set_int(value, astal_river_output_get_height(self));
            break;
        case ASTAL_RIVER_OUTPUT_PROP_PHYSICAL_WIDTH:
            g_value_set_int(value, astal_river_output_get_physical_width(self));
            break;
        case ASTAL_RIVER_OUTPUT_PROP_PHYSICAL_HEIGHT:
            g_value_set_int(value, astal_river_output_get_physical_height(self));
            break;
        case ASTAL_RIVER_OUTPUT_PROP_SCALE_FACTOR:
            g_value_set_int(value, astal_river_output_get_scale_factor(self));
            break;
        case ASTAL_RIVER_OUTPUT_PROP_REFRESH_RATE:
            g_value_set_double(value, astal_river_output_get_refresh_rate(self));
            break;
        case ASTAL_RIVER_OUTPUT_PROP_TRANSFORM:
            g_value_set_enum(value, astal_river_output_get_transform(self));
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

static void astal_river_wl_output_handle_geometry(void* data, struct wl_output* output, int32_t x,
                                                  int32_t y, int32_t physical_width,
                                                  int32_t physical_height, int32_t subpixel,
                                                  const char* make, const char* model,
                                                  int32_t transform) {
    AstalRiverOutput* self = ASTAL_RIVER_OUTPUT(data);
    self->x = x;
    self->y = y;
    self->physical_height = physical_height;
    self->physical_width = physical_width;
    self->transform = transform;
    g_free(self->make);
    self->make = g_strdup(make);
    g_free(self->model);
    self->model = g_strdup(model);
    g_object_notify(G_OBJECT(self), "x");
    g_object_notify(G_OBJECT(self), "y");
    g_object_notify(G_OBJECT(self), "physical-width");
    g_object_notify(G_OBJECT(self), "physical-height");
    g_object_notify(G_OBJECT(self), "transform");
    g_object_notify(G_OBJECT(self), "make");
    g_object_notify(G_OBJECT(self), "model");
    g_signal_emit_by_name(self, "changed", 0);
}

static void astal_river_wl_output_handle_mode(void* data, struct wl_output* output, uint32_t mode,
                                              int32_t width, int32_t height, int32_t refresh) {
    AstalRiverOutput* self = ASTAL_RIVER_OUTPUT(data);

    if (mode != WL_OUTPUT_MODE_CURRENT) return;

    self->height = height;
    self->width = width;
    self->refresh_rate = refresh / 1000.0;
    g_object_notify(G_OBJECT(self), "height");
    g_object_notify(G_OBJECT(self), "width");
    g_object_notify(G_OBJECT(self), "refresh-rate");
    g_signal_emit_by_name(self, "changed", 0);
}

static void astal_river_wl_output_handle_scale(void* data, struct wl_output* output,
                                               int32_t scale) {
    AstalRiverOutput* self = ASTAL_RIVER_OUTPUT(data);

    self->scale_factor = scale;
    g_object_notify(G_OBJECT(self), "scale-factor");
    g_signal_emit_by_name(self, "changed", 0);
}

static void astal_river_wl_output_handle_description(void* data, struct wl_output* output,
                                                     const char* description) {
    AstalRiverOutput* self = ASTAL_RIVER_OUTPUT(data);
    g_free(self->description);
    self->description = g_strdup(description);
    g_object_notify(G_OBJECT(self), "description");
    g_signal_emit_by_name(self, "changed", 0);
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
    .geometry = astal_river_wl_output_handle_geometry,
    .mode = astal_river_wl_output_handle_mode,
    .scale = astal_river_wl_output_handle_scale,
    .description = astal_river_wl_output_handle_description,
    .done = noop,
};

static void astal_river_output_finalize(GObject* object) {
    AstalRiverOutput* self = ASTAL_RIVER_OUTPUT(object);
    AstalRiverOutputPrivate* priv = astal_river_output_get_instance_private(self);

    zriver_output_status_v1_destroy(priv->river_output_status);
    wl_output_destroy(priv->wl_output);

    wl_display_roundtrip(priv->wl_display);

    g_free(self->layout_name);
    g_free(self->focused_view);
    g_free(self->name);
    g_free(self->description);
    g_free(self->make);
    g_free(self->model);

    G_OBJECT_CLASS(astal_river_output_parent_class)->finalize(object);
}

static void astal_river_output_init(AstalRiverOutput* self) {}

AstalRiverOutput* astal_river_output_new(guint id, struct wl_output* wl_output,
                                         struct zriver_status_manager_v1* status_manager,
                                         struct zriver_control_v1* river_control,
                                         struct wl_seat* seat, struct wl_display* wl_display) {
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

    astal_river_output_properties[ASTAL_RIVER_OUTPUT_PROP_DESCRIPTION] = g_param_spec_string(
        "description", "description", "description of the output", NULL, G_PARAM_READABLE);
    astal_river_output_properties[ASTAL_RIVER_OUTPUT_PROP_MAKE] =
        g_param_spec_string("make", "make", "make of the output", NULL, G_PARAM_READABLE);
    astal_river_output_properties[ASTAL_RIVER_OUTPUT_PROP_MODEL] =
        g_param_spec_string("model", "model", "model of the output", NULL, G_PARAM_READABLE);
    astal_river_output_properties[ASTAL_RIVER_OUTPUT_PROP_X] = g_param_spec_int(
        "x", "x", "x coordinate of the output", INT_MIN, INT_MAX, 0, G_PARAM_READABLE);
    astal_river_output_properties[ASTAL_RIVER_OUTPUT_PROP_Y] = g_param_spec_int(
        "y", "y", "y coordinate of the output", INT_MIN, INT_MAX, 0, G_PARAM_READABLE);
    astal_river_output_properties[ASTAL_RIVER_OUTPUT_PROP_WIDTH] =
        g_param_spec_int("width", "width", "width of the output", 0, INT_MAX, 0, G_PARAM_READABLE);
    astal_river_output_properties[ASTAL_RIVER_OUTPUT_PROP_HEIGHT] = g_param_spec_int(
        "height", "height", "height of the output", 0, INT_MAX, 0, G_PARAM_READABLE);
    astal_river_output_properties[ASTAL_RIVER_OUTPUT_PROP_PHYSICAL_WIDTH] =
        g_param_spec_int("physical-width", "physical-width", "physical width of the output", 0,
                         INT_MAX, 0, G_PARAM_READABLE);
    astal_river_output_properties[ASTAL_RIVER_OUTPUT_PROP_PHYSICAL_HEIGHT] =
        g_param_spec_int("physical-height", "physical-height", "physical height of the output", 0,
                         INT_MAX, 0, G_PARAM_READABLE);
    astal_river_output_properties[ASTAL_RIVER_OUTPUT_PROP_SCALE_FACTOR] =
        g_param_spec_int("scale-factor", "scale-factor", "scale factor of the output", 0, INT_MAX,
                         0, G_PARAM_READABLE);
    astal_river_output_properties[ASTAL_RIVER_OUTPUT_PROP_REFRESH_RATE] =
        g_param_spec_double("refresh-rate", "refresh-rate", "refresh rate of the output", 0,
                            INT_MAX, 0, G_PARAM_READABLE);
    /**
     * AstalRiverOutput:transform: (type AstalRiverTransform)
     *
     */
    astal_river_output_properties[ASTAL_RIVER_OUTPUT_PROP_TRANSFORM] = g_param_spec_enum(
        "transform", "transform", "the transfrom of the output", ASTAL_RIVER_TYPE_TRANSFORM,
        ASTAL_RIVER_TRANSFORM_NORMAL, G_PARAM_READABLE);

    g_object_class_install_properties(object_class, ASTAL_RIVER_OUTPUT_N_PROPERTIES,
                                      astal_river_output_properties);

    astal_river_output_signals[ASTAL_RIVER_OUTPUT_SIGNAL_CHANGED] =
        g_signal_new("changed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL, NULL,
                     G_TYPE_NONE, 0);
}
