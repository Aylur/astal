#include "wireplumber.h"

#include <wp/wp.h>

#include "endpoint-private.h"
#include "glib-object.h"
#include "wp.h"

struct _AstalWpWp {
    GObject parent_instance;
};

typedef struct {
    WpCore *core;
    WpObjectManager *obj_manager;

    WpPlugin *mixer;
    WpPlugin *defaults;
    gint pending_plugins;

    GHashTable *endpoints;
} AstalWpWpPrivate;

G_DEFINE_FINAL_TYPE_WITH_PRIVATE(AstalWpWp, astal_wp_wp, G_TYPE_OBJECT);

typedef enum {
    ASTAL_WP_WP_SIGNAL_CHANGED,
    ASTAL_WP_WP_SIGNAL_ENDPOINT_ADDED,
    ASTAL_WP_WP_SIGNAL_ENDPOINT_REMOVED,
    ASTAL_WP_WP_N_SIGNALS
} AstalWpWpSignals;

static guint astal_wp_wp_signals[ASTAL_WP_WP_N_SIGNALS] = {
    0,
};

/**
 * astal_wp_wp_get_endpoint:
 * @self: the AstalWpWp object
 * @id: the id of the endpoint
 *
 * Returns: (transfer none) (nullable): the endpoint with the given id
 */
AstalWpEndpoint *astal_wp_wp_get_endpoint(AstalWpWp *self, guint id) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    AstalWpEndpoint *endpoint = g_hash_table_lookup(priv->endpoints, GUINT_TO_POINTER(id));
    return endpoint;
}

static void astal_wp_wp_default_changed(AstalWpWp *self) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    guint defaultSinkId;
    guint defaultSourceId;

    g_signal_emit_by_name(priv->defaults, "get-default-node", "Audio/Sink", &defaultSinkId);
    g_signal_emit_by_name(priv->defaults, "get-default-node", "Audio/Source", &defaultSourceId);

    g_print("default nodes: sink: %d, source: %d\n", defaultSinkId, defaultSourceId);

    g_signal_emit_by_name(self, "changed");
}

static void astal_wp_wp_object_added(AstalWpWp *self, gpointer object) {
    // print pipewire properties
    // WpIterator *iter = wp_pipewire_object_new_properties_iterator(WP_PIPEWIRE_OBJECT(object));
    // GValue item = G_VALUE_INIT;
    // const gchar *key, *value;
    //
    // g_print("\n\n");
    // while (wp_iterator_next (iter, &item)) {
    //     WpPropertiesItem *pi = g_value_get_boxed (&item);
    //     key = wp_properties_item_get_key (pi);
    //     value = wp_properties_item_get_value (pi);
    //     g_print("%s: %s\n", key, value);
    // }

    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    WpNode *node = WP_NODE(object);
    AstalWpEndpoint *endpoint = astal_wp_endpoint_create(node, priv->mixer);

    g_hash_table_insert(priv->endpoints, GUINT_TO_POINTER(wp_proxy_get_bound_id(WP_PROXY(node))),
                        endpoint);

    g_signal_emit_by_name(self, "endpoint-added", endpoint);
    g_signal_emit_by_name(self, "changed");
}

static void astal_wp_wp_object_removed(AstalWpWp *self, gpointer object) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    WpNode *node = WP_NODE(object);

    guint id = wp_proxy_get_bound_id(WP_PROXY(node));

    AstalWpEndpoint *endpoint = g_hash_table_lookup(priv->endpoints, GUINT_TO_POINTER(id));

    g_hash_table_remove(priv->endpoints, GUINT_TO_POINTER(id));

    g_signal_emit_by_name(self, "endpoint-removed", endpoint);
    g_signal_emit_by_name(self, "changed");
}

static void astal_wp_wp_mixer_changed(AstalWpWp *self, guint node_id) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    AstalWpEndpoint *endpoint = g_hash_table_lookup(priv->endpoints, GUINT_TO_POINTER(node_id));

    if (endpoint == NULL) return;

    astal_wp_endpoint_update_volume(endpoint);

    g_signal_emit_by_name(self, "changed");
}

static void astal_wp_wp_objm_installed(AstalWpWp *self) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);
}

static void astal_wp_wp_plugin_activated(WpObject *obj, GAsyncResult *result, AstalWpWp *self) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    GError *error = NULL;
    wp_object_activate_finish(obj, result, &error);
    if (error) {
        g_critical("Failed to activate component: %s\n", error->message);
        return;
    }

    if (--priv->pending_plugins == 0) {
        priv->defaults = wp_plugin_find(priv->core, "default-nodes-api");
        priv->mixer = wp_plugin_find(priv->core, "mixer-api");

        g_signal_connect_swapped(priv->mixer, "changed", (GCallback)astal_wp_wp_mixer_changed,
                                 self);
        g_signal_connect_swapped(priv->defaults, "changed", (GCallback)astal_wp_wp_default_changed,
                                 self);

        g_signal_connect_swapped(priv->obj_manager, "object-added",
                                 G_CALLBACK(astal_wp_wp_object_added), self);
        g_signal_connect_swapped(priv->obj_manager, "object-removed",
                                 G_CALLBACK(astal_wp_wp_object_removed), self);
        wp_core_install_object_manager(priv->core, priv->obj_manager);
    }
}

static void astal_wp_wp_plugin_loaded(WpObject *obj, GAsyncResult *result, AstalWpWp *self) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    GError *error = NULL;
    wp_core_load_component_finish(priv->core, result, &error);
    if (error) {
        g_critical("Failed to load component: %s\n", error->message);
        return;
    }

    wp_object_activate(obj, WP_PLUGIN_FEATURE_ENABLED, NULL,
                       (GAsyncReadyCallback)astal_wp_wp_plugin_activated, self);
}

/**
 * astal_wp_wp_get_default
 *
 * Returns: (nullable) (transfer none): gets the default wireplumber object.
 */
AstalWpWp *astal_wp_wp_get_default() {
    static AstalWpWp *self = NULL;

    if (self == NULL) self = g_object_new(ASTAL_WP_TYPE_WP, NULL);

    return self;
}

/**
 * astal_wp_get_default_wp
 *
 * Returns: (nullable) (transfer none): gets the default wireplumber object.
 */
AstalWpWp *astal_wp_get_default_wp() { return astal_wp_wp_get_default(); }

static void astal_wp_wp_dispose(GObject *object) {
    AstalWpWp *self = ASTAL_WP_WP(object);
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    wp_core_disconnect(priv->core);
    g_clear_object(&priv->mixer);
    g_clear_object(&priv->defaults);
    g_clear_object(&priv->obj_manager);
    g_clear_object(&priv->core);

    if (priv->endpoints != NULL) {
        g_hash_table_destroy(priv->endpoints);
        priv->endpoints = NULL;
    }
}

static void astal_wp_wp_finalize(GObject *object) {
    AstalWpWp *self = ASTAL_WP_WP(object);
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);
}

static void astal_wp_wp_init(AstalWpWp *self) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    priv->endpoints = g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL, g_object_unref);

    wp_init(WP_INIT_ALL);
    priv->core = wp_core_new(NULL, NULL, NULL);

    if (!wp_core_connect(priv->core)) {
        g_critical("could not connect to PipeWire\n");
    }

    priv->obj_manager = wp_object_manager_new();
    wp_object_manager_add_interest(priv->obj_manager, WP_TYPE_NODE, WP_CONSTRAINT_TYPE_PW_PROPERTY,
                                   "media.class", "=s", "Audio/Sink", NULL);
    wp_object_manager_add_interest(priv->obj_manager, WP_TYPE_NODE, WP_CONSTRAINT_TYPE_PW_PROPERTY,
                                   "media.class", "=s", "Audio/Source", NULL);
    wp_object_manager_add_interest(priv->obj_manager, WP_TYPE_NODE,
                                   WP_CONSTRAINT_TYPE_PW_GLOBAL_PROPERTY, "media.class", "=s",
                                   "Stream/Output/Audio", NULL);
    wp_object_manager_add_interest(priv->obj_manager, WP_TYPE_NODE, WP_CONSTRAINT_TYPE_PW_PROPERTY,
                                   "media.class", "=s", "Stream/Input/Audio", NULL);

    g_signal_connect_swapped(priv->obj_manager, "installed", (GCallback)astal_wp_wp_objm_installed,
                             self);

    priv->pending_plugins = 2;
    wp_core_load_component(priv->core, "libwireplumber-module-default-nodes-api", "module", NULL,
                           "default-nodes-api", NULL,
                           (GAsyncReadyCallback)astal_wp_wp_plugin_loaded, self);
    wp_core_load_component(priv->core, "libwireplumber-module-mixer-api", "module", NULL,
                           "mixer-api", NULL, (GAsyncReadyCallback)astal_wp_wp_plugin_loaded, self);
}

static void astal_wp_wp_class_init(AstalWpWpClass *class) {
    GObjectClass *object_class = G_OBJECT_CLASS(class);
    object_class->finalize = astal_wp_wp_finalize;
    object_class->dispose = astal_wp_wp_dispose;

    astal_wp_wp_signals[ASTAL_WP_WP_SIGNAL_ENDPOINT_ADDED] =
        g_signal_new("endpoint-added", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_ENDPOINT);
    astal_wp_wp_signals[ASTAL_WP_WP_SIGNAL_ENDPOINT_REMOVED] =
        g_signal_new("endpoint-removed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL,
                     NULL, NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_ENDPOINT);
    astal_wp_wp_signals[ASTAL_WP_WP_SIGNAL_CHANGED] =
        g_signal_new("changed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL, NULL,
                     G_TYPE_NONE, 0);
}
