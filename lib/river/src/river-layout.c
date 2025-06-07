#include <gio/gio.h>
#include <stdint.h>
#include <stdio.h>
#include <wayland-client-protocol.h>

#include "astal-river.h"
#include "glib-object.h"
#include "glib.h"
#include "river-layout-v3-client.h"
#include "river-private.h"

G_DEFINE_BOXED_TYPE(AstalRiverGeometry, astal_river_geometry, astal_river_geometry_copy,
                    astal_river_geometry_free)

/**
 * astal_river_geometry_copy
 * @geometry: the AstalRiverGeometry to copy
 *
 * Creates a copy of the given AstalRiverGeometry.
 *
 */
AstalRiverGeometry* astal_river_geometry_copy(AstalRiverGeometry* geometry) {
    AstalRiverGeometry* new_geometry = g_new(AstalRiverGeometry, 1);
    new_geometry->x = geometry->x;
    new_geometry->y = geometry->y;
    new_geometry->width = geometry->width;
    new_geometry->height = geometry->height;
    return new_geometry;
}

/**
 * astal_river_geometry_free
 * @geometry: the AstalRiverGeometry to free
 *
 * Frees the given AstalRiverGeometry.
 *
 */
void astal_river_geometry_free(AstalRiverGeometry* geometry) { g_free(geometry); }

struct _AstalRiverLayout {
    GObject parent_instance;

    gchar* namespace;
};

typedef struct {
    AstalRiverRiver* river;

    GHashTable* layouts;

    struct river_layout_manager_v3* layout_manager;
    struct wl_display* wl_display;

    AstalRiverLayoutDemandCallback layout_demand_callback;
    GDestroyNotify layout_demand_destroy;
    void* layout_demand_user_data;

} AstalRiverLayoutPrivate;

G_DEFINE_FINAL_TYPE_WITH_PRIVATE(AstalRiverLayout, astal_river_layout, G_TYPE_OBJECT);

typedef enum {
    ASTAL_RIVER_LAYOUT_PROP_NAMESPACE = 1,
    ASTAL_RIVER_LAYOUT_N_PROPERTIES
} AstalRiverLayoutProperties;

typedef enum {
    ASTAL_RIVER_LAYOUT_SIGNAL_NAMESPACE_IN_USE,
    ASTAL_RIVER_LAYOUT_SIGNAL_USER_COMMAND,
    ASTAL_RIVER_LAYOUT_N_SIGNALS
} AstalRiverLayoutSignals;

static guint astal_river_layout_signals[ASTAL_RIVER_LAYOUT_N_SIGNALS] = {
    0,
};

static GParamSpec* astal_river_layout_properties[ASTAL_RIVER_LAYOUT_N_PROPERTIES] = {
    NULL,
};

struct river_layout_data {
    AstalRiverLayout* layout;
    AstalRiverOutput* output;
    struct river_layout_v3* river_layout;
};

void astal_river_layout_data_free(struct river_layout_data* data) {
    if (data->layout) {
        river_layout_v3_destroy(data->river_layout);
    }
    g_free(data);
}

/**
 *  AstalRiverLayout
 *
 *  handles the layout of windows.
 */

/**
 * astal_river_layout_get_namespace
 * @self: the AstalRiverLayout object
 *
 * the namespace of the layout
 *
 * Returns: (transfer none) (nullable): the namespace of the layout
 */
const gchar* astal_river_layout_get_namespace(AstalRiverLayout* self) {
    g_return_val_if_fail(ASTAL_RIVER_IS_LAYOUT(self), NULL);
    return self->namespace;
}

/**
 * astal_river_layout_set_layout_demand_callback
 * @self: the AstalRiverLayout object
 * @callback: (scope notified): the callback to be called when a layout demand is made
 * @user_data: user data to be passed to the callback
 * @destroy_notify: a function to destroy the user data when it is no longer needed
 *
 * Sets the callback to be called when a layout demand is made.
 */
void astal_river_layout_set_layout_demand_callback(AstalRiverLayout* self,
                                                   AstalRiverLayoutDemandCallback callback,
                                                   gpointer user_data,
                                                   GDestroyNotify destroy_notify) {
    g_return_if_fail(ASTAL_RIVER_IS_LAYOUT(self));

    AstalRiverLayoutPrivate* priv = astal_river_layout_get_instance_private(self);
    if (priv->layout_demand_destroy) {
        priv->layout_demand_destroy(priv->layout_demand_user_data);
    }
    priv->layout_demand_callback = callback;
    priv->layout_demand_user_data = user_data;
    priv->layout_demand_destroy = destroy_notify;
}

static void astal_river_layout_set_property(GObject* object, guint property_id, const GValue* value,
                                            GParamSpec* pspec) {
    AstalRiverLayout* self = ASTAL_RIVER_LAYOUT(object);

    switch (property_id) {
        case ASTAL_RIVER_LAYOUT_PROP_NAMESPACE:
            g_free(self->namespace);
            self->namespace = g_value_dup_string(value);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_river_layout_get_property(GObject* object, guint property_id, GValue* value,
                                            GParamSpec* pspec) {
    AstalRiverLayout* self = ASTAL_RIVER_LAYOUT(object);

    switch (property_id) {
        case ASTAL_RIVER_LAYOUT_PROP_NAMESPACE:
            g_value_set_string(value, self->namespace);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_river_layout_handle_namespace_in_use(void* data, struct river_layout_v3* layout) {
    struct river_layout_data* layout_data = data;
    g_signal_emit_by_name(layout_data->layout, "namespace-in-use", layout_data->output);
}

static void astal_river_layout_handle_layout_demand(void* data, struct river_layout_v3* layout,
                                                    uint view_count, uint usable_width,
                                                    uint usable_height, uint tags, uint serial) {
    struct river_layout_data* layout_data = data;
    AstalRiverLayoutPrivate* priv = astal_river_layout_get_instance_private(layout_data->layout);

    if (priv->layout_demand_callback == NULL) {
        return;
    }

    GList* geometries = NULL;
    gchar* layout_name = NULL;

    priv->layout_demand_callback(layout_data->layout, layout_data->output, view_count, usable_width,
                                 usable_height, &layout_name, &geometries,
                                 priv->layout_demand_user_data);

    if (g_list_length(geometries) != view_count) {
        g_critical("Invalid layout demand result: received %u geometries but expected %u.",
                   g_list_length(geometries), view_count);
        goto exit;
    }
    if (layout_name == NULL) {
        g_critical("Invalid layout demand result: received NULL layout name.");
        goto exit;
    }
    for (GList* l = geometries; l != NULL; l = l->next) {
        AstalRiverGeometry* geometry = (AstalRiverGeometry*)(l->data);
        river_layout_v3_push_view_dimensions(layout_data->river_layout, geometry->x, geometry->y,
                                             geometry->width, geometry->height, serial);
    }

    river_layout_v3_commit(layout_data->river_layout, layout_name, serial);

exit:
    g_list_free_full(geometries, (GDestroyNotify)astal_river_geometry_free);
    g_free(layout_name);
}

static void astal_river_layout_handle_user_command(void* data, struct river_layout_v3* layout,
                                                   const char* command) {
    struct river_layout_data* layout_data = data;

    g_signal_emit_by_name(layout_data->layout, "user-command", command, layout_data->output);
}

static void noop() {}

static const struct river_layout_v3_listener river_layout_listener = {
    .namespace_in_use = astal_river_layout_handle_namespace_in_use,
    .layout_demand = astal_river_layout_handle_layout_demand,
    .user_command = astal_river_layout_handle_user_command,
    .user_command_tags = noop};

void astal_river_layout_on_output_added(AstalRiverLayout* self, gchar* output_name) {
    AstalRiverLayoutPrivate* priv = astal_river_layout_get_instance_private(self);
    struct river_layout_data* data = g_new0(struct river_layout_data, 1);
    AstalRiverOutput* output = astal_river_river_get_output(priv->river, output_name);
    if (output == NULL) return;

    data->layout = self;
    data->output = output;

    data->river_layout = river_layout_manager_v3_get_layout(
        priv->layout_manager, astal_river_output_get_wl_output(output), self->namespace);
    river_layout_v3_add_listener(data->river_layout, &river_layout_listener, data);

    g_hash_table_insert(priv->layouts, g_strdup(output_name), data);
}

void astal_river_layout_on_output_removed(AstalRiverLayout* self, gchar* output_name) {
    AstalRiverLayoutPrivate* priv = astal_river_layout_get_instance_private(self);
    g_hash_table_remove(priv->layouts, output_name);
}

AstalRiverLayout* astal_river_layout_new(AstalRiverRiver* river,
                                         struct river_layout_manager_v3* layout_manager,
                                         struct wl_display* wl_display, const gchar* namespace) {
    AstalRiverLayout* self = g_object_new(ASTAL_RIVER_TYPE_LAYOUT, "namespace", namespace, NULL);
    AstalRiverLayoutPrivate* priv = astal_river_layout_get_instance_private(self);

    priv->river = g_object_ref(river);
    priv->layout_manager = layout_manager;
    priv->wl_display = wl_display;
    priv->layouts = g_hash_table_new_full(g_str_hash, g_str_equal, g_free,
                                          (GDestroyNotify)astal_river_layout_data_free);

    g_signal_connect_swapped(priv->river, "output-added",
                             G_CALLBACK(astal_river_layout_on_output_added), self);
    g_signal_connect_swapped(priv->river, "output-removed",
                             G_CALLBACK(astal_river_layout_on_output_removed), self);

    GList* outputs = astal_river_river_get_outputs(river);
    for (GList* l = outputs; l != NULL; l = l->next) {
        AstalRiverOutput* output = ASTAL_RIVER_OUTPUT(l->data);
        astal_river_layout_on_output_added(self, astal_river_output_get_name(output));
    }

    return self;
}

static void astal_river_layout_dispose(GObject* object) {
    AstalRiverLayout* self = ASTAL_RIVER_LAYOUT(object);
    AstalRiverLayoutPrivate* priv = astal_river_layout_get_instance_private(self);

    g_hash_table_unref(priv->layouts);
    g_clear_object(&priv->river);
}

static void astal_river_layout_finalize(GObject* object) {
    AstalRiverLayout* self = ASTAL_RIVER_LAYOUT(object);
    AstalRiverLayoutPrivate* priv = astal_river_layout_get_instance_private(self);

    wl_display_roundtrip(priv->wl_display);

    g_free(self->namespace);

    G_OBJECT_CLASS(astal_river_layout_parent_class)->finalize(object);
}

static void astal_river_layout_init(AstalRiverLayout* self) {}

static void astal_river_layout_class_init(AstalRiverLayoutClass* class) {
    GObjectClass* object_class = G_OBJECT_CLASS(class);
    object_class->get_property = astal_river_layout_get_property;
    object_class->set_property = astal_river_layout_set_property;
    object_class->dispose = astal_river_layout_dispose;
    object_class->finalize = astal_river_layout_finalize;
    /**
     * AstalRiverLayout:namespace:
     *
     * The namespace of this layout
     */
    astal_river_layout_properties[ASTAL_RIVER_LAYOUT_PROP_NAMESPACE] =
        g_param_spec_string("namespace", "namespace", "the namespace of this layout", NULL,
                            G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);

    g_object_class_install_properties(object_class, ASTAL_RIVER_LAYOUT_N_PROPERTIES,
                                      astal_river_layout_properties);

    /**
     * AstalRiverLayout::namespace-in-use:
     * @self: the AstalRiverLayout object
     * @output: the AstalRiverOutput object that is using the namespace
     *
     * Emitted when the namespace of this layout is already in use on an output.
     */
    astal_river_layout_signals[ASTAL_RIVER_LAYOUT_SIGNAL_NAMESPACE_IN_USE] =
        g_signal_new("namespace-in-use", ASTAL_RIVER_TYPE_LAYOUT, G_SIGNAL_RUN_LAST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_RIVER_TYPE_OUTPUT);

    /**
     * AstalRiverLayout::user-command:
     * @self: the AstalRiverLayout object
     * @command: the command send by the user
     * @output: the currently focused output
     *
     * Emitted when a user command is requested for this layout.
     */
    astal_river_layout_signals[ASTAL_RIVER_LAYOUT_SIGNAL_USER_COMMAND] =
        g_signal_new("user_command", ASTAL_RIVER_TYPE_LAYOUT, G_SIGNAL_RUN_LAST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 2, G_TYPE_STRING, ASTAL_RIVER_TYPE_OUTPUT);
}
