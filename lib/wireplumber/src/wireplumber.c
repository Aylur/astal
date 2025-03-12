#include <wp/wp.h>

#include "audio.h"
#include "device-private.h"
#include "endpoint-private.h"
#include "glib-object.h"
#include "glib.h"
#include "video.h"
#include "wp.h"

struct _AstalWpWp {
    GObject parent_instance;

    AstalWpEndpoint *default_speaker;
    AstalWpEndpoint *default_microphone;

    AstalWpAudio *audio;
    AstalWpVideo *video;

    AstalWpScale scale;
};

typedef struct {
    WpCore *core;
    WpObjectManager *obj_manager;

    WpPlugin *mixer;
    WpPlugin *defaults;
    gint pending_plugins;

    GHashTable *endpoints;
    GHashTable *devices;
    GHashTable *delayed_endpoints;
} AstalWpWpPrivate;

G_DEFINE_FINAL_TYPE_WITH_PRIVATE(AstalWpWp, astal_wp_wp, G_TYPE_OBJECT);

G_DEFINE_ENUM_TYPE(AstalWpScale, astal_wp_scale,
                   G_DEFINE_ENUM_VALUE(ASTAL_WP_SCALE_LINEAR, "linear"),
                   G_DEFINE_ENUM_VALUE(ASTAL_WP_SCALE_CUBIC, "cubic"));

typedef enum {
    ASTAL_WP_WP_SIGNAL_ENDPOINT_ADDED,
    ASTAL_WP_WP_SIGNAL_ENDPOINT_REMOVED,
    ASTAL_WP_WP_SIGNAL_DEVICE_ADDED,
    ASTAL_WP_WP_SIGNAL_DEVICE_REMOVED,
    ASTAL_WP_WP_N_SIGNALS
} AstalWpWpSignals;

static guint astal_wp_wp_signals[ASTAL_WP_WP_N_SIGNALS] = {
    0,
};

typedef enum {
    ASTAL_WP_WP_PROP_AUDIO = 1,
    ASTAL_WP_WP_PROP_VIDEO,
    ASTAL_WP_WP_PROP_ENDPOINTS,
    ASTAL_WP_WP_PROP_DEVICES,
    ASTAL_WP_WP_PROP_DEFAULT_SPEAKER,
    ASTAL_WP_WP_PROP_DEFAULT_MICROPHONE,
    ASTAL_WP_WP_PROP_SCALE,
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
 * astal_wp_wp_get_endpoint:
 * @self: the AstalWpWp object
 * @id: the id of the endpoint
 *
 * the endpoint with the given id
 *
 * Returns: (transfer none) (nullable): the endpoint with the given id
 */
AstalWpEndpoint *astal_wp_wp_get_endpoint(AstalWpWp *self, guint id) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    AstalWpEndpoint *endpoint = g_hash_table_lookup(priv->endpoints, GUINT_TO_POINTER(id));
    return endpoint;
}

/**
 * astal_wp_wp_get_endpoints:
 * @self: the AstalWpWp object
 *
 * a GList containing all endpoints
 *
 * Returns: (transfer container) (nullable) (type GList(AstalWpEndpoint)): a GList containing the
 * endpoints
 */
GList *astal_wp_wp_get_endpoints(AstalWpWp *self) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);
    return g_hash_table_get_values(priv->endpoints);
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
 * Returns: (nullable) (transfer none): gets the audio object
 */
AstalWpAudio *astal_wp_wp_get_audio(AstalWpWp *self) { return self->audio; }

/**
 * astal_wp_wp_get_video
 *
 * gets the video object
 *
 * Returns: (nullable) (transfer none): gets the video object
 */
AstalWpVideo *astal_wp_wp_get_video(AstalWpWp *self) { return self->video; }

/**
 * astal_wp_wp_get_default_speaker
 *
 * gets the default speaker object
 *
 * Returns: (nullable) (transfer none): gets the default speaker object
 */
AstalWpEndpoint *astal_wp_wp_get_default_speaker(AstalWpWp *self) { return self->default_speaker; }

/**
 * astal_wp_wp_get_default_microphone
 *
 * gets the default microphone object
 *
 * Returns: (nullable) (transfer none): gets the default microphone object
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

    g_hash_table_iter_init(&iter, priv->endpoints);
    while (g_hash_table_iter_next(&iter, &key, &value)) {
        AstalWpEndpoint *ep = ASTAL_WP_ENDPOINT(value);
        astal_wp_endpoint_update_volume(ep);
    }

    astal_wp_endpoint_update_volume(self->default_speaker);
    astal_wp_endpoint_update_volume(self->default_microphone);
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
        case ASTAL_WP_WP_PROP_ENDPOINTS:
            g_value_set_pointer(value, g_hash_table_get_values(priv->endpoints));
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
                    astal_wp_endpoint_create(l->data, priv->mixer, priv->defaults, self);

                g_hash_table_insert(priv->endpoints,
                                    GUINT_TO_POINTER(wp_proxy_get_bound_id(WP_PROXY(l->data))),
                                    endpoint);
                g_hash_table_remove(priv->delayed_endpoints,
                                    GUINT_TO_POINTER(wp_proxy_get_bound_id(WP_PROXY(l->data))));
                g_signal_emit_by_name(self, "endpoint-added", endpoint);
                g_object_notify(G_OBJECT(self), "endpoints");
            }
        }
    }
    g_list_free(eps);
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

        AstalWpEndpoint *endpoint =
            astal_wp_endpoint_create(node, priv->mixer, priv->defaults, self);

        g_hash_table_insert(priv->endpoints,
                            GUINT_TO_POINTER(wp_proxy_get_bound_id(WP_PROXY(node))), endpoint);

        g_signal_emit_by_name(self, "endpoint-added", endpoint);
        g_object_notify(G_OBJECT(self), "endpoints");
    } else if (WP_IS_DEVICE(object)) {
        WpDevice *node = WP_DEVICE(object);
        AstalWpDevice *device = astal_wp_device_create(node);
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
        AstalWpEndpoint *endpoint =
            g_object_ref(g_hash_table_lookup(priv->endpoints, GUINT_TO_POINTER(id)));

        g_hash_table_remove(priv->endpoints, GUINT_TO_POINTER(id));

        g_signal_emit_by_name(self, "endpoint-removed", endpoint);
        g_object_notify(G_OBJECT(self), "endpoints");
        g_object_unref(endpoint);
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

static void astal_wp_wp_objm_installed(AstalWpWp *self) {
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    astal_wp_endpoint_init_as_default(self->default_speaker, priv->mixer, priv->defaults,
                                      ASTAL_WP_MEDIA_CLASS_AUDIO_SPEAKER, self);
    astal_wp_endpoint_init_as_default(self->default_microphone, priv->mixer, priv->defaults,
                                      ASTAL_WP_MEDIA_CLASS_AUDIO_MICROPHONE, self);
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
 * Returns: (nullable) (transfer none): gets the default wireplumber object.
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
 * Returns: (nullable) (transfer none): gets the default wireplumber object.
 */
AstalWpWp *astal_wp_get_default() { return astal_wp_wp_get_default(); }

static void astal_wp_wp_dispose(GObject *object) {
    AstalWpWp *self = ASTAL_WP_WP(object);
    AstalWpWpPrivate *priv = astal_wp_wp_get_instance_private(self);

    g_clear_object(&self->video);
    g_clear_object(&self->audio);

    wp_core_disconnect(priv->core);
    g_clear_object(&self->default_speaker);
    g_clear_object(&self->default_microphone);
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
    priv->delayed_endpoints = g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL, NULL);
    priv->devices = g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL, g_object_unref);

    wp_init(7);
    priv->core = wp_core_new(NULL, NULL, NULL);

    if (!wp_core_connect(priv->core)) {
        g_critical("could not connect to PipeWire\n");
        return;
    }

    priv->obj_manager = wp_object_manager_new();
    wp_object_manager_request_object_features(priv->obj_manager, WP_TYPE_NODE,
                                              WP_OBJECT_FEATURES_ALL);
    wp_object_manager_request_object_features(priv->obj_manager, WP_TYPE_GLOBAL_PROXY,
                                              WP_OBJECT_FEATURES_ALL);

    wp_object_manager_add_interest(priv->obj_manager, WP_TYPE_NODE, WP_CONSTRAINT_TYPE_PW_PROPERTY,
                                   "media.class", "=s", "Audio/Sink", NULL);
    wp_object_manager_add_interest(priv->obj_manager, WP_TYPE_NODE, WP_CONSTRAINT_TYPE_PW_PROPERTY,
                                   "media.class", "=s", "Audio/Source", NULL);
    wp_object_manager_add_interest(priv->obj_manager, WP_TYPE_NODE, WP_CONSTRAINT_TYPE_PW_PROPERTY,
                                   "media.class", "=s", "Stream/Output/Audio", NULL);
    wp_object_manager_add_interest(priv->obj_manager, WP_TYPE_NODE, WP_CONSTRAINT_TYPE_PW_PROPERTY,
                                   "media.class", "=s", "Stream/Input/Audio", NULL);
    wp_object_manager_add_interest(priv->obj_manager, WP_TYPE_DEVICE,
                                   WP_CONSTRAINT_TYPE_PW_GLOBAL_PROPERTY, "media.class", "=s",
                                   "Audio/Device", NULL);

    wp_object_manager_add_interest(priv->obj_manager, WP_TYPE_NODE, WP_CONSTRAINT_TYPE_PW_PROPERTY,
                                   "media.class", "=s", "Video/Sink", NULL);
    wp_object_manager_add_interest(priv->obj_manager, WP_TYPE_NODE, WP_CONSTRAINT_TYPE_PW_PROPERTY,
                                   "media.class", "=s", "Video/Source", NULL);
    wp_object_manager_add_interest(priv->obj_manager, WP_TYPE_NODE, WP_CONSTRAINT_TYPE_PW_PROPERTY,
                                   "media.class", "=s", "Stream/Output/Video", NULL);
    wp_object_manager_add_interest(priv->obj_manager, WP_TYPE_NODE, WP_CONSTRAINT_TYPE_PW_PROPERTY,
                                   "media.class", "=s", "Stream/Input/Video", NULL);
    wp_object_manager_add_interest(priv->obj_manager, WP_TYPE_DEVICE,
                                   WP_CONSTRAINT_TYPE_PW_GLOBAL_PROPERTY, "media.class", "=s",
                                   "Video/Device", NULL);
    // wp_object_manager_add_interest(priv->obj_manager, WP_TYPE_CLIENT, NULL);

    g_signal_connect_swapped(priv->obj_manager, "installed", (GCallback)astal_wp_wp_objm_installed,
                             self);

    self->default_speaker = g_object_new(ASTAL_WP_TYPE_ENDPOINT, NULL);
    self->default_microphone = g_object_new(ASTAL_WP_TYPE_ENDPOINT, NULL);

    self->audio = astal_wp_audio_new(self);
    self->video = astal_wp_video_new(self);

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
     * AstalWpWp:endpoints: (type GList(AstalWpEndpoint)) (transfer container)
     *
     * A list of [class@AstalWp.Endpoint] objects
     */
    astal_wp_wp_properties[ASTAL_WP_WP_PROP_ENDPOINTS] =
        g_param_spec_pointer("endpoints", "endpoints", "endpoints", G_PARAM_READABLE);
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
     * The [class@AstalWp.Endpoint] representing the default speaker
     */
    astal_wp_wp_properties[ASTAL_WP_WP_PROP_DEFAULT_MICROPHONE] =
        g_param_spec_object("default-microphone", "default-microphone", "default-microphone",
                            ASTAL_WP_TYPE_ENDPOINT, G_PARAM_READABLE);

    g_object_class_install_properties(object_class, ASTAL_WP_WP_N_PROPERTIES,
                                      astal_wp_wp_properties);

    astal_wp_wp_signals[ASTAL_WP_WP_SIGNAL_ENDPOINT_ADDED] =
        g_signal_new("endpoint-added", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_ENDPOINT);
    astal_wp_wp_signals[ASTAL_WP_WP_SIGNAL_ENDPOINT_REMOVED] =
        g_signal_new("endpoint-removed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL,
                     NULL, NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_ENDPOINT);
    astal_wp_wp_signals[ASTAL_WP_WP_SIGNAL_DEVICE_ADDED] =
        g_signal_new("device-added", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_DEVICE);
    astal_wp_wp_signals[ASTAL_WP_WP_SIGNAL_DEVICE_REMOVED] =
        g_signal_new("device-removed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_DEVICE);
}
