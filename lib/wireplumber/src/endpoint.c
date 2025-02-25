#include "endpoint.h"

#include <wp/wp.h>

#include "astal-wp-enum-types.h"
#include "device.h"
#include "glib-object.h"
#include "glib.h"
#include "node-private.h"
#include "node.h"
#include "wp.h"

struct _AstalWpEndpoint {
    AstalWpNode parent_instance;

    guint device_id;
    gboolean is_default;
};

typedef struct {
    gulong default_node_handler_signal_id;
    WpPlugin *default_plugin;
    gboolean is_default_node;

} AstalWpEndpointPrivate;

G_DEFINE_FINAL_TYPE_WITH_PRIVATE(AstalWpEndpoint, astal_wp_endpoint, ASTAL_WP_TYPE_NODE);

typedef enum {
    ASTAL_WP_ENDPOINT_PROP_DEVICE_ID = 1,
    ASTAL_WP_ENDPOINT_PROP_DEVICE,
    ASTAL_WP_ENDPOINT_PROP_DEFAULT,
    ASTAL_WP_ENDPOINT_PROP_IS_DEFAULT_NODE,
    ASTAL_WP_ENDPOINT_PROP_DEFAULT_PLUGIN,
    ASTAL_WP_ENDPOINT_N_PROPERTIES,
} AstalWpEndpointProperties;

static GParamSpec *astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_N_PROPERTIES] = {
    NULL,
};

/**
 * astal_wp_endpoint_get_device_id:
 *
 * gets the id of the device associated with this endpoint
 */
guint astal_wp_endpoint_get_device_id(AstalWpEndpoint *self) { return self->device_id; }

/**
 * astal_wp_endpoint_get_device:
 *
 * gets the device associated with this endpoint
 *
 * Returns: (transfer none) (nullable)
 */
AstalWpDevice *astal_wp_endpoint_get_device(AstalWpEndpoint *self) {
    AstalWpWp *wp;
    g_object_get(self, "wp", &wp, NULL);
    return astal_wp_wp_get_device(wp, self->device_id);
}

gboolean astal_wp_endpoint_get_is_default(AstalWpEndpoint *self) { return self->is_default; }

void astal_wp_endpoint_set_is_default(AstalWpEndpoint *self, gboolean is_default) {
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);
    WpNode *node;

    g_object_get(self, "node", &node, NULL);

    if (!is_default) return;
    gboolean ret;
    const gchar *name = wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(node), "node.name");
    const gchar *media_class =
        wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(node), "media.class");
    g_signal_emit_by_name(priv->default_plugin, "set-default-configured-node-name", media_class,
                          name, &ret);
}

static void astal_wp_endpoint_get_property(GObject *object, guint property_id, GValue *value,
                                           GParamSpec *pspec) {
    AstalWpEndpoint *self = ASTAL_WP_ENDPOINT(object);

    switch (property_id) {
        case ASTAL_WP_ENDPOINT_PROP_DEVICE_ID:
            g_value_set_uint(value, self->device_id);
            break;
        case ASTAL_WP_ENDPOINT_PROP_DEVICE:
            g_value_set_object(value, astal_wp_endpoint_get_device(self));
            break;
        case ASTAL_WP_ENDPOINT_PROP_DEFAULT:
            g_value_set_boolean(value, self->is_default);
            break;

        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_wp_endpoint_set_property(GObject *object, guint property_id, const GValue *value,
                                           GParamSpec *pspec) {
    AstalWpEndpoint *self = ASTAL_WP_ENDPOINT(object);
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);

    WpPlugin *plugin;

    switch (property_id) {
        case ASTAL_WP_ENDPOINT_PROP_DEFAULT_PLUGIN:
            plugin = g_value_get_object(value);
            if (plugin != NULL && WP_IS_PLUGIN(plugin)) {
                g_clear_object(&priv->default_plugin);
                priv->default_plugin = g_object_ref(plugin);
            }
            break;
        case ASTAL_WP_ENDPOINT_PROP_IS_DEFAULT_NODE:
            priv->is_default_node = g_value_get_boolean(value);
            break;
        case ASTAL_WP_ENDPOINT_PROP_DEFAULT:
            astal_wp_endpoint_set_is_default(self, g_value_get_boolean(value));
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_wp_endpoint_properties_changed(AstalWpEndpoint *self) {
    WpNode *node;
    AstalWpMediaClass media_class;
    g_object_get(G_OBJECT(self), "node", &node, "media-class", &media_class, NULL);

    if(node == NULL) return;

    WpPipewireObject *pwo = WP_PIPEWIRE_OBJECT(node);

    const gchar *value;

    value = wp_pipewire_object_get_property(pwo, "device.id");
    guint id = g_ascii_strtoull(value, NULL, 10);
    if (self->device_id != id) {
        self->device_id = id;
        g_object_notify(G_OBJECT(self), "device-id");
        g_object_notify(G_OBJECT(self), "device");
    }

    AstalWpDevice *device = astal_wp_endpoint_get_device(self);
    if (device != NULL) {
        value = astal_wp_device_get_icon(device);
    }
    if (value == NULL) {
        value = media_class == ASTAL_WP_MEDIA_CLASS_AUDIO_SPEAKER
                    ? "audio-card-symbolic"
                    : "audio-input-microphone-symbolic";
    }
    astal_wp_node_set_icon(ASTAL_WP_NODE(self), value);
}

void astal_wp_endpoint_real_params_changed(AstalWpNode *node, const gchar *id) {
    AstalWpEndpoint *self = ASTAL_WP_ENDPOINT(node);

    g_object_freeze_notify(G_OBJECT(self));

    if (!g_strcmp0(id, "Props")) astal_wp_endpoint_properties_changed(self);

    ASTAL_WP_NODE_CLASS(astal_wp_endpoint_parent_class)->params_changed(node, id);
    g_object_thaw_notify(G_OBJECT(self));
}

static void astal_wp_endpoint_default_changed_as_default(AstalWpEndpoint *self) {
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);

    WpNode *node;
    guint id;
    AstalWpWp *wp;
    AstalWpMediaClass media_class;
    g_object_get(self, "node", &node, "id", &id, "wp", &wp, "media-class", &media_class, NULL);

    GEnumClass *enum_class = g_type_class_ref(ASTAL_WP_TYPE_MEDIA_CLASS);
    const gchar *media_class_nick = g_enum_get_value(enum_class, media_class)->value_nick;
    guint defaultId;
    g_signal_emit_by_name(priv->default_plugin, "get-default-node", media_class_nick, &defaultId);
    g_type_class_unref(enum_class);

    if (defaultId != id) {
        AstalWpNode *default_node = astal_wp_wp_get_node(wp, defaultId);
        if (default_node != NULL && astal_wp_node_get_media_class(default_node) == media_class) {
            WpNode *default_wp_node;
            g_object_get(default_node, "node", &default_wp_node, NULL);
            astal_wp_node_set_node(ASTAL_WP_NODE(self), default_wp_node);
        }
    }
}

static void astal_wp_endpoint_default_changed(AstalWpEndpoint *self) {
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);

    if (priv->is_default_node) {
        astal_wp_endpoint_default_changed_as_default(self);
        return;
    }

    guint id;
    WpNode *node;
    g_object_get(self, "id", &id, "node", &node, NULL);

    guint defaultId;
    const gchar *media_class =
        wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(node), "media.class");
    g_signal_emit_by_name(priv->default_plugin, "get-default-node", media_class, &defaultId);

    if (self->is_default && defaultId != id) {
        self->is_default = FALSE;
        g_object_notify(G_OBJECT(self), "is-default");
    } else if (!self->is_default && defaultId == id) {
        self->is_default = TRUE;
        g_object_notify(G_OBJECT(self), "is-default");
    }
}

void astal_wp_endpoint_init_as_default(AstalWpEndpoint *self, WpPlugin *mixer, WpPlugin *defaults,
                                       AstalWpMediaClass type) {
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);

    astal_wp_node_set_mixer(ASTAL_WP_NODE(self), mixer);
    astal_wp_node_set_type(ASTAL_WP_NODE(self), type);

    priv->default_plugin = g_object_ref(defaults);
    priv->is_default_node = TRUE;
    self->is_default = TRUE;

    priv->default_node_handler_signal_id =
        g_signal_connect_swapped(priv->default_plugin, "changed",
                                 G_CALLBACK(astal_wp_endpoint_default_changed_as_default), self);

    astal_wp_endpoint_default_changed_as_default(self);
}

AstalWpEndpoint *astal_wp_endpoint_new_default(AstalWpWp *wp) {
    return g_object_new(ASTAL_WP_TYPE_ENDPOINT, "wp", wp, "is-default-node", TRUE, NULL);
}

AstalWpEndpoint *astal_wp_endpoint_new(WpNode *node, WpPlugin *mixer, WpPlugin *defaults,
                                       AstalWpWp *wp) {
    AstalWpEndpoint* self =  g_object_new(ASTAL_WP_TYPE_ENDPOINT, "mixer-plugin", mixer, "node", node,
                        "default-plugin", defaults, "wp", wp, "is-default-node", FALSE, NULL);
  AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);

    priv->default_node_handler_signal_id =
        g_signal_connect_swapped(priv->default_plugin, "changed",
                                 G_CALLBACK(astal_wp_endpoint_default_changed), self);

  return self;
}

static void astal_wp_endpoint_dispose(GObject *object) {
    AstalWpEndpoint *self = ASTAL_WP_ENDPOINT(object);
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);

    g_signal_handler_disconnect(priv->default_plugin, priv->default_node_handler_signal_id);
    g_clear_object(&priv->default_plugin);

    G_OBJECT_CLASS(astal_wp_endpoint_parent_class)->dispose(object);
}

static void astal_wp_endpoint_init(AstalWpEndpoint *self) {}

static void astal_wp_endpoint_class_init(AstalWpEndpointClass *class) {
    GObjectClass *object_class = G_OBJECT_CLASS(class);
    object_class->get_property = astal_wp_endpoint_get_property;
    object_class->set_property = astal_wp_endpoint_set_property;
    object_class->dispose = astal_wp_endpoint_dispose;

    AstalWpNodeClass *node_class = ASTAL_WP_NODE_CLASS(class);
    node_class->params_changed = astal_wp_endpoint_real_params_changed;

    /**
     * AstalWpEndpoint:device-id:
     *
     * The id of the device associated with this endpoint.
     */
    astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_PROP_DEVICE_ID] =
        g_param_spec_uint("device-id", "device-id", "device-id", 0, UINT_MAX, 0, G_PARAM_READABLE);

    /**
     * AstalWpEndpoint:device:
     *
     * The the device associated with this endpoint.
     */
    astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_PROP_DEVICE] =
        g_param_spec_object("device", "device", "device", ASTAL_WP_TYPE_DEVICE, G_PARAM_READABLE);
    /**
     * AstalWpEndpoint:is-default:
     *
     * Whether this node is the default one used for this media-class. Note that setting this
     * property to false has no effect.
     */
    astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_PROP_DEFAULT] =
        g_param_spec_boolean("is-default", "is-default", "is-default", FALSE, G_PARAM_READWRITE);

    /**
     * AstalWpEndpoint:default-plugin: (skip)
     */
    astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_PROP_DEFAULT_PLUGIN] =
        g_param_spec_object("default-plugin", "default-plugin", "default-plugin", WP_TYPE_PLUGIN,
                            G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    /**
     * AstalWpEndpoint:is-default-node: (skip)
     */
    astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_PROP_IS_DEFAULT_NODE] =
        g_param_spec_boolean("is-default-node", "is-default-node", "is-default-node", FALSE,
                             G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);

    g_object_class_install_properties(object_class, ASTAL_WP_ENDPOINT_N_PROPERTIES,
                                      astal_wp_endpoint_properties);
}
