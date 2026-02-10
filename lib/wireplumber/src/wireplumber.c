#include <wp/wp.h>

#include "astal-wp-enum-types.h"
#include "audio.h"
#include "device.h"
#include "gio/gio.h"
#include "glib-object.h"
#include "glib.h"
#include "node-private.h"
#include "node.h"
#include "video.h"
#include "wp-private.h"
#include "wp.h"
#include "wp/core.h"

struct _AstalWpWp {
    GObject parent_instance;

    AstalWpEndpoint *default_speaker;
    AstalWpEndpoint *default_microphone;

    AstalWpAudio *audio;
    AstalWpVideo *video;

    AstalWpScale scale;
    gboolean connected;
};

typedef struct {
    WpCore *core;
    WpObjectManager *obj_manager;
    WpObjectManager *metadata_manager;

    WpPlugin *mixer;
    WpPlugin *defaults;
    gint pending_plugins;

    WpMetadata *metadata;
    GHashTable *nodes;
    GHashTable *devices;
    GHashTable *delayed_endpoints;

    guint metadata_handler_id;
} AstalWpWpPrivate;

G_DEFINE_FINAL_TYPE_WITH_PRIVATE(AstalWpWp, astal_wp_wp, G_TYPE_OBJECT);

typedef enum {
    ASTAL_WP_WP_SIGNAL_NODE_ADDED,
    ASTAL_WP_WP_SIGNAL_NODE_REMOVED,
    ASTAL_WP_WP_SIGNAL_DEVICE_ADDED,
    ASTAL_WP_WP_SIGNAL_DEVICE_REMOVED,
    ASTAL_WP_WP_SIGNAL_READY,
    ASTAL_WP_WP_N_SIGNALS
} AstalWpWpSignals;

static guint astal_wp_wp_signals[ASTAL_WP_WP_N_SIGNALS] = {
    0,
};

typedef enum {
    ASTAL_WP_WP_PROP_AUDIO = 1,
    ASTAL_WP_WP_PROP_VIDEO,
    ASTAL_WP_WP_PROP_NODES,
    ASTAL_WP_WP_PROP_DEVICES,
    ASTAL_WP_WP_PROP_DEFAULT_SPEAKER,
    ASTAL_WP_WP_PROP_DEFAULT_MICROPHONE,
    ASTAL_WP_WP_PROP_SCALE,
    ASTAL_WP_WP_PROP_CONNECTED,
    ASTAL_WP_WP_N_PROPERTIES,
} AstalWpWpProperties;

static GParamSpec *astal_wp_wp_properties[ASTAL_WP_WP_N_PROPERTIES] = {
    NULL,
};

/**
 * AstalWpWp
 *
 * manages the connection to wireplumber. Usually you don't want to use this class directly, but use
 * the [class@AstalWp.Audio] or [class@AstalWp.Video] instead.
 *
 */

/**
 * astal_wp_wp_get_node:
 * @self: the AstalWpWp object
 * @id: the id of the node
 *
 * the node with the given id
 *
 * Returns: (transfer none) (nullable): the node with the given id
 */
AstalWpNode *astal_wp_wp_get_node(AstalWpWp *self, guint id) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    AstalWpNode *node = g_hash_table_lookup(priv->nodes, GUINT_TO_POINTER(id));
    return node;
}

/**
 * astal_wp_wp_get_nodes:
 * @self: the AstalWpWp object
 *
 * a GList containing all nodes
 *
 * Returns: (transfer container) (nullable) (type GList(AstalWpNode)): a GList containing the
 * nodes
 */
GList *astal_wp_wp_get_nodes(AstalWpWp *self) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);
    return g_hash_table_get_values(priv->nodes);
}

/**
 * astal_wp_wp_get_device:
 * @self: the AstalWpWp object
 * @id: the id of the device
 *
 * the device with the given id
 *
 * Returns: (transfer none) (nullable): the device with the given id
 */
AstalWpDevice *astal_wp_wp_get_device(AstalWpWp *self, guint id) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    AstalWpDevice *device = g_hash_table_lookup(priv->devices, GUINT_TO_POINTER(id));
    return device;
}

/**
 * astal_wp_wp_get_devices:
 * @self: the AstalWpWp object
 *
 * the GList containing the devices
 *
 * Returns: (transfer container) (nullable) (type GList(AstalWpDevice)): a GList containing the
 * devices
 */
GList *astal_wp_wp_get_devices(AstalWpWp *self) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);
    return g_hash_table_get_values(priv->devices);
}

/**
 * astal_wp_wp_get_audio
 *
 * gets the [class@AstalWp.Audio] object
 *
 * Returns: (transfer none): gets the audio object
 */
AstalWpAudio *astal_wp_wp_get_audio(AstalWpWp *self) { return self->audio; }

/**
 * astal_wp_wp_get_video
 *
 * gets the video object
 *
 * Returns: (transfer none): gets the video object
 */
AstalWpVideo *astal_wp_wp_get_video(AstalWpWp *self) { return self->video; }

/**
 * astal_wp_wp_get_default_speaker
 *
 * gets the default speaker object
 *
 * Returns: (transfer none): gets the default speaker object
 */
AstalWpEndpoint *astal_wp_wp_get_default_speaker(AstalWpWp *self) { return self->default_speaker; }

/**
 * astal_wp_wp_get_default_microphone
 *
 * gets the default microphone object
 *
 * Returns: (transfer none): gets the default microphone object
 */
AstalWpEndpoint *astal_wp_wp_get_default_microphone(AstalWpWp *self) {
    return self->default_microphone;
}

AstalWpScale astal_wp_wp_get_scale(AstalWpWp *self) { return self->scale; }

void astal_wp_wp_set_scale(AstalWpWp *self, AstalWpScale scale) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);
    self->scale = scale;

    if (priv->mixer == NULL) return;

    g_object_set(priv->mixer, "scale", self->scale, NULL);

    GHashTableIter iter;
    gpointer key, value;

    g_hash_table_iter_init(&iter, priv->nodes);
    while (g_hash_table_iter_next(&iter, &key, &value)) {
        AstalWpNode *ep = ASTAL_WP_NODE(value);
        astal_wp_node_update_volume(ep);
    }

    astal_wp_node_update_volume(ASTAL_WP_NODE(self->default_speaker));
    astal_wp_node_update_volume(ASTAL_WP_NODE(self->default_microphone));
}

static gboolean astal_wp_wp_node_compare_serial(gpointer key, gpointer value, gpointer user_data) {
    return astal_wp_node_get_serial(ASTAL_WP_NODE(value)) == GPOINTER_TO_INT(user_data);
}

/**
 * astal_wp_wp_get_node_by_serial
 *
 * finds the AstalWpNode with the give serial.
 *
 * Returns: (transfer none) (nullable)
 */
AstalWpNode *astal_wp_wp_get_node_by_serial(AstalWpWp *self, gint serial) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);
    return g_hash_table_find(priv->nodes, astal_wp_wp_node_compare_serial, GINT_TO_POINTER(serial));
}

static void astal_wp_wp_get_property(GObject *object, guint property_id, GValue *value,
                                     GParamSpec *pspec) {
    AstalWpWp *self = ASTAL_WP_WP(object);
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    switch (property_id) {
        case ASTAL_WP_WP_PROP_AUDIO:
            g_value_set_object(value, astal_wp_wp_get_audio(self));
            break;
        case ASTAL_WP_WP_PROP_VIDEO:
            g_value_set_object(value, astal_wp_wp_get_video(self));
            break;
        case ASTAL_WP_WP_PROP_NODES:
            g_value_set_pointer(value, g_hash_table_get_values(priv->nodes));
            break;
        case ASTAL_WP_WP_PROP_DEVICES:
            g_value_set_pointer(value, g_hash_table_get_values(priv->devices));
            break;
        case ASTAL_WP_WP_PROP_DEFAULT_SPEAKER:
            g_value_set_object(value, self->default_speaker);
            break;
        case ASTAL_WP_WP_PROP_DEFAULT_MICROPHONE:
            g_value_set_object(value, self->default_microphone);
            break;
        case ASTAL_WP_WP_PROP_SCALE:
            g_value_set_enum(value, self->scale);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_wp_wp_set_property(GObject *object, guint property_id, const GValue *value,
                                     GParamSpec *pspec) {
    AstalWpWp *self = ASTAL_WP_WP(object);

    switch (property_id) {
        case ASTAL_WP_WP_PROP_SCALE:
            astal_wp_wp_set_scale(self, g_value_get_enum(value));
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_wp_wp_check_delayed_endpoints(AstalWpWp *self, guint device_id) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);
    GList *eps = g_hash_table_get_values(priv->delayed_endpoints);

    for (GList *l = eps; l != NULL; l = l->next) {
        const gchar *dev =
            wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(l->data), "device.id");
        if (dev != NULL) {
            if (device_id == g_ascii_strtoull(dev, NULL, 10)) {
                AstalWpEndpoint *endpoint =
                    astal_wp_endpoint_new(l->data, priv->mixer, priv->defaults, self);

                g_hash_table_insert(priv->nodes,
                                    GUINT_TO_POINTER(wp_proxy_get_bound_id(WP_PROXY(l->data))),
                                    endpoint);
                g_hash_table_remove(priv->delayed_endpoints,
                                    GUINT_TO_POINTER(wp_proxy_get_bound_id(WP_PROXY(l->data))));
                astal_wp_wp_update_metadata(self, astal_wp_node_get_id(ASTAL_WP_NODE(endpoint)));
                g_signal_emit_by_name(self, "node-added", endpoint);
                g_object_notify(G_OBJECT(self), "nodes");
            }
        }
    }
    g_list_free(eps);
}

void astal_wp_wp_set_matadata(AstalWpWp *self, guint subject, const gchar *key, const gchar *type,
                              const gchar *value) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);
    wp_metadata_set(priv->metadata, subject, key, type, value);
}

static void astal_wp_wp_metadata_changed(WpMetadata *metadata, guint subject, const gchar *key,
                                         const gchar *type, const gchar *value,
                                         gpointer user_data) {
    AstalWpWp *self = ASTAL_WP_WP(user_data);
    AstalWpNode *node = astal_wp_wp_get_node(self, subject);

    if (node == NULL) return;

    astal_wp_node_metadata_changed(node, key, type, value);
}

void astal_wp_wp_update_metadata(AstalWpWp *self, guint subject) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    if (priv->metadata == NULL) return;

    WpIterator *iter = wp_metadata_new_iterator(priv->metadata, subject);
    GValue value = G_VALUE_INIT;
    while (wp_iterator_next(iter, &value)) {
        WpMetadataItem *item = g_value_get_boxed(&value);

        astal_wp_wp_metadata_changed(
            priv->metadata, wp_metadata_item_get_subject(item), wp_metadata_item_get_key(item),
            wp_metadata_item_get_value_type(item), wp_metadata_item_get_value(item), self);
        g_value_unset(&value);
    }
    wp_iterator_unref(iter);
}

static void astal_wp_wp_metadata_added(AstalWpWp *self, gpointer object) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);
    if (!WP_IS_METADATA(object)) return;
    WpMetadata *metadata = WP_METADATA(object);
    if (priv->metadata != NULL) {
        g_signal_handler_disconnect(priv->metadata, priv->metadata_handler_id);
        g_clear_object(&priv->metadata);
    }
    priv->metadata = g_object_ref(metadata);
    priv->metadata_handler_id =
        g_signal_connect(priv->metadata, "changed", G_CALLBACK(astal_wp_wp_metadata_changed), self);

    astal_wp_wp_update_metadata(self, -1);
}

static void astal_wp_wp_object_added(AstalWpWp *self, gpointer object) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    if (WP_IS_NODE(object)) {
        WpNode *node = WP_NODE(object);
        const gchar *dev = wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(node), "device.id");
        if (dev != NULL) {
            guint device_id = g_ascii_strtoull(dev, NULL, 10);
            if (!g_hash_table_contains(priv->devices, GUINT_TO_POINTER(device_id))) {
                g_hash_table_insert(priv->delayed_endpoints,
                                    GUINT_TO_POINTER(wp_proxy_get_bound_id(WP_PROXY(node))), node);
                return;
            }
        }

        AstalWpNode *astal_node;
        const gchar *media_class =
            wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(node), "media.class");
        if (g_str_has_prefix(media_class, "Stream")) {
            astal_node = ASTAL_WP_NODE(astal_wp_stream_new(node, priv->mixer, self));
        } else {
            astal_node =
                ASTAL_WP_NODE(astal_wp_endpoint_new(node, priv->mixer, priv->defaults, self));
        }
        g_hash_table_insert(priv->nodes, GUINT_TO_POINTER(wp_proxy_get_bound_id(WP_PROXY(node))),
                            astal_node);
        astal_wp_wp_update_metadata(self, astal_wp_node_get_id(astal_node));
        g_signal_emit_by_name(self, "node-added", astal_node);
        g_object_notify(G_OBJECT(self), "nodes");
    } else if (WP_IS_DEVICE(object)) {
        WpDevice *node = WP_DEVICE(object);
        AstalWpDevice *device = g_object_new(ASTAL_WP_TYPE_DEVICE, "device", node, NULL);
        g_hash_table_insert(priv->devices, GUINT_TO_POINTER(wp_proxy_get_bound_id(WP_PROXY(node))),
                            device);
        g_signal_emit_by_name(self, "device-added", device);
        g_object_notify(G_OBJECT(self), "devices");
        astal_wp_wp_check_delayed_endpoints(self, wp_proxy_get_bound_id(WP_PROXY(node)));
    }
}

static void astal_wp_wp_object_removed(AstalWpWp *self, gpointer object) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    if (WP_IS_NODE(object)) {
        guint id = wp_proxy_get_bound_id(WP_PROXY(object));
        AstalWpNode *node = g_object_ref(g_hash_table_lookup(priv->nodes, GUINT_TO_POINTER(id)));

        g_hash_table_remove(priv->nodes, GUINT_TO_POINTER(id));

        g_signal_emit_by_name(self, "node-removed", node);
        g_object_notify(G_OBJECT(self), "nodes");
        g_object_unref(node);
    } else if (WP_IS_DEVICE(object)) {
        guint id = wp_proxy_get_bound_id(WP_PROXY(object));
        AstalWpDevice *device =
            g_object_ref(g_hash_table_lookup(priv->devices, GUINT_TO_POINTER(id)));
        g_hash_table_remove(priv->devices, GUINT_TO_POINTER(id));

        g_signal_emit_by_name(self, "device-removed", device);
        g_object_notify(G_OBJECT(self), "devices");
        g_object_unref(device);
    }
}

static void astal_wp_wp_roundtrip_cb(WpCore *core, GAsyncResult *result, AstalWpWp *self) {
    g_signal_emit_by_name(self, "ready");
}

static void astal_wp_wp_objm_installed(AstalWpWp *self) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    astal_wp_endpoint_init_as_default(self->default_speaker, priv->mixer, priv->defaults,
                                      ASTAL_WP_MEDIA_CLASS_AUDIO_SPEAKER);
    astal_wp_endpoint_init_as_default(self->default_microphone, priv->mixer, priv->defaults,
                                      ASTAL_WP_MEDIA_CLASS_AUDIO_MICROPHONE);
    wp_core_sync(priv->core, NULL, (GAsyncReadyCallback)astal_wp_wp_roundtrip_cb, self);
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
        g_object_set(priv->mixer, "scale", self->scale, NULL);

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
 * gets the default wireplumber object.
 *
 * Returns: (transfer none): gets the default wireplumber object.
 */
AstalWpWp *astal_wp_wp_get_default() {
    static AstalWpWp *self = NULL;

    if (self == NULL) self = g_object_new(ASTAL_WP_TYPE_WP, NULL);

    return self;
}

/**
 * astal_wp_get_default
 *
 * gets the default wireplumber object.
 *
 * Returns: (transfer none): gets the default wireplumber object.
 */
AstalWpWp *astal_wp_get_default() { return astal_wp_wp_get_default(); }

static gboolean astal_wp_wp_try_reconnect(AstalWpWp *self) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);
    g_debug("Trying to connect to pipewire.");
    if (!wp_core_connect(priv->core)) {
        g_timeout_add(1000, (GSourceFunc)astal_wp_wp_try_reconnect, self);
    }
    return G_SOURCE_REMOVE;
}

static void astal_wp_wp_core_connected(AstalWpWp *self) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    g_debug("Sucessfully connected to pipewire.");

    priv->metadata_manager = wp_object_manager_new();
    wp_object_manager_request_object_features(priv->metadata_manager, WP_TYPE_GLOBAL_PROXY,
                                              WP_OBJECT_FEATURES_ALL);

    wp_object_manager_add_interest(priv->metadata_manager, WP_TYPE_METADATA,
                                   WP_CONSTRAINT_TYPE_PW_GLOBAL_PROPERTY, "metadata.name", "=s",
                                   "default", NULL);
    g_signal_connect_swapped(priv->metadata_manager, "object-added",
                             G_CALLBACK(astal_wp_wp_metadata_added), self);
    wp_core_install_object_manager(priv->core, priv->metadata_manager);

    priv->obj_manager = wp_object_manager_new();
    wp_object_manager_request_object_features(priv->obj_manager, WP_TYPE_NODE,
                                              WP_OBJECT_FEATURES_ALL);
    wp_object_manager_request_object_features(priv->obj_manager, WP_TYPE_GLOBAL_PROXY,
                                              WP_OBJECT_FEATURES_ALL);

    wp_object_manager_add_interest(priv->obj_manager, WP_TYPE_NODE, WP_CONSTRAINT_TYPE_PW_PROPERTY,
                                   "media.class", "c(sssss)", "Audio/Sink", "Audio/Source",
                                   "Audio/Source/Virtual", "Stream/Output/Audio", "Stream/Input/Audio", NULL);
    wp_object_manager_add_interest(priv->obj_manager, WP_TYPE_DEVICE,
                                   WP_CONSTRAINT_TYPE_PW_GLOBAL_PROPERTY, "media.class", "=s",
                                   "Audio/Device", NULL);

    wp_object_manager_add_interest(priv->obj_manager, WP_TYPE_NODE, WP_CONSTRAINT_TYPE_PW_PROPERTY,
                                   "media.class", "c(ssss)", "Video/Sink", "Video/Source",
                                   "Stream/Output/Video", "Stream/Input/Video", NULL);
    wp_object_manager_add_interest(priv->obj_manager, WP_TYPE_DEVICE,
                                   WP_CONSTRAINT_TYPE_PW_GLOBAL_PROPERTY, "media.class", "=s",
                                   "Video/Device", NULL);
    // wp_object_manager_add_interest(priv->obj_manager, WP_TYPE_CLIENT, NULL);

    g_signal_connect_swapped(priv->obj_manager, "installed", (GCallback)astal_wp_wp_objm_installed,
                             self);

    priv->pending_plugins = 2;
    wp_core_load_component(priv->core, "libwireplumber-module-default-nodes-api", "module", NULL,
                           "default-nodes-api", NULL,
                           (GAsyncReadyCallback)astal_wp_wp_plugin_loaded, self);
    wp_core_load_component(priv->core, "libwireplumber-module-mixer-api", "module", NULL,
                           "mixer-api", NULL, (GAsyncReadyCallback)astal_wp_wp_plugin_loaded, self);

    self->connected = TRUE;
    g_object_notify(G_OBJECT(self), "connected");
}

static void astal_wp_wp_core_disconnected(AstalWpWp *self) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    g_debug("Connection to pipewire lost.");

    if (priv->metadata) {
        g_signal_handler_disconnect(priv->metadata, priv->metadata_handler_id);
    }

    g_clear_object(&priv->mixer);
    g_clear_object(&priv->defaults);
    g_clear_object(&priv->metadata_manager);
    g_clear_object(&priv->metadata);
    g_clear_object(&priv->obj_manager);
    g_clear_object(&priv->metadata);

    if (priv->nodes != NULL) {
        g_hash_table_remove_all(priv->nodes);
    }

    if (priv->devices != NULL) {
        g_hash_table_remove_all(priv->devices);
    }

    self->connected = FALSE;
    g_object_notify(G_OBJECT(self), "connected");

    astal_wp_wp_try_reconnect(self);
}

static void astal_wp_wp_init(AstalWpWp *self) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    wp_init(7);

    self->connected = FALSE;

    priv->core = wp_core_new(NULL, NULL, NULL);

    g_signal_connect_swapped(priv->core, "connected", (GCallback)astal_wp_wp_core_connected, self);
    g_signal_connect_swapped(priv->core, "disconnected", (GCallback)astal_wp_wp_core_disconnected,
                             self);

    priv->nodes = g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL, g_object_unref);
    priv->delayed_endpoints = g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL, NULL);
    priv->devices = g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL, g_object_unref);

    self->default_speaker = astal_wp_endpoint_new_default(self);
    self->default_microphone = astal_wp_endpoint_new_default(self);

    self->audio = astal_wp_audio_new(self);
    self->video = astal_wp_video_new(self);

    astal_wp_wp_try_reconnect(self);
}

static void astal_wp_wp_dispose(GObject *object) {
    AstalWpWp *self = ASTAL_WP_WP(object);
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    if (priv->metadata) {
        g_signal_handler_disconnect(priv->metadata, priv->metadata_handler_id);
    }

    g_clear_object(&self->video);
    g_clear_object(&self->audio);

    wp_core_disconnect(priv->core);
    g_clear_object(&self->default_speaker);
    g_clear_object(&self->default_microphone);
    g_clear_object(&priv->mixer);
    g_clear_object(&priv->defaults);
    g_clear_object(&priv->metadata_manager);
    g_clear_object(&priv->metadata);
    g_clear_object(&priv->obj_manager);
    g_clear_object(&priv->core);
    g_clear_object(&priv->metadata);

    if (priv->nodes != NULL) {
        g_hash_table_destroy(priv->nodes);
        priv->nodes = NULL;
    }

    if (priv->devices != NULL) {
        g_hash_table_destroy(priv->devices);
        priv->devices = NULL;
    }

    G_OBJECT_CLASS(astal_wp_wp_parent_class)->dispose(object);
}

static void astal_wp_wp_class_init(AstalWpWpClass *class) {
    GObjectClass *object_class = G_OBJECT_CLASS(class);
    object_class->dispose = astal_wp_wp_dispose;
    object_class->get_property = astal_wp_wp_get_property;
    object_class->set_property = astal_wp_wp_set_property;

    astal_wp_wp_properties[ASTAL_WP_WP_PROP_AUDIO] =
        g_param_spec_object("audio", "audio", "audio", ASTAL_WP_TYPE_AUDIO, G_PARAM_READABLE);
    astal_wp_wp_properties[ASTAL_WP_WP_PROP_VIDEO] =
        g_param_spec_object("video", "video", "video", ASTAL_WP_TYPE_VIDEO, G_PARAM_READABLE);
    /**
     * AstalWpWp:scale: (type AstalWpScale)
     *
     * The scale used for the volume
     */
    astal_wp_wp_properties[ASTAL_WP_WP_PROP_SCALE] =
        g_param_spec_enum("scale", "scale", "scale", ASTAL_WP_TYPE_SCALE, ASTAL_WP_SCALE_CUBIC,
                          G_PARAM_READWRITE | G_PARAM_CONSTRUCT);

    /**
     * AstalWpWp:connected:
     *
     * The connection status to the pipewire daemon
     */
    astal_wp_wp_properties[ASTAL_WP_WP_PROP_CONNECTED] =
        g_param_spec_boolean("connected", "connected", "connected", FALSE, G_PARAM_READABLE);

    /**
     * AstalWpWp:nodes: (type GList(AstalWpNode)) (transfer container)
     *
     * A list of [class@AstalWp.Node] objects
     */
    astal_wp_wp_properties[ASTAL_WP_WP_PROP_NODES] =
        g_param_spec_pointer("nodes", "nodes", "nodes", G_PARAM_READABLE);
    /**
     * AstalWpWp:devices: (type GList(AstalWpDevice)) (transfer container)
     *
     * A list of [class@AstalWp.Device] objects
     */
    astal_wp_wp_properties[ASTAL_WP_WP_PROP_DEVICES] =
        g_param_spec_pointer("devices", "devices", "devices", G_PARAM_READABLE);
    /**
     * AstalWpWp:default-speaker:
     *
     * The [class@AstalWp.Endpoint] representing the default speaker
     */
    astal_wp_wp_properties[ASTAL_WP_WP_PROP_DEFAULT_SPEAKER] =
        g_param_spec_object("default-speaker", "default-speaker", "default-speaker",
                            ASTAL_WP_TYPE_ENDPOINT, G_PARAM_READABLE);
    /**
     * AstalWpWp:default-microphone:
     *
     * The [class@AstalWp.Endpoint] representing the default micophone
     */
    astal_wp_wp_properties[ASTAL_WP_WP_PROP_DEFAULT_MICROPHONE] =
        g_param_spec_object("default-microphone", "default-microphone", "default-microphone",
                            ASTAL_WP_TYPE_ENDPOINT, G_PARAM_READABLE);

    g_object_class_install_properties(object_class, ASTAL_WP_WP_N_PROPERTIES,
                                      astal_wp_wp_properties);

    astal_wp_wp_signals[ASTAL_WP_WP_SIGNAL_NODE_ADDED] =
        g_signal_new("node-added", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_NODE);
    astal_wp_wp_signals[ASTAL_WP_WP_SIGNAL_NODE_REMOVED] =
        g_signal_new("node-removed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_NODE);
    astal_wp_wp_signals[ASTAL_WP_WP_SIGNAL_DEVICE_ADDED] =
        g_signal_new("device-added", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_DEVICE);
    astal_wp_wp_signals[ASTAL_WP_WP_SIGNAL_DEVICE_REMOVED] =
        g_signal_new("device-removed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_DEVICE);
    astal_wp_wp_signals[ASTAL_WP_WP_SIGNAL_READY] = g_signal_new(
        "ready", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL, NULL, G_TYPE_NONE, 0);
}
