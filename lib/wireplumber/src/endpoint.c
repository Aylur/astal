#include "endpoint.h"

#include <wp/wp.h>

#include "device.h"
#include "glib-object.h"
#include "node-private.h"
#include "node.h"
#include "wp.h"

struct _AstalWpEndpoint {
    AstalWpNode parent_instance;
    
    guint device_id;
};

G_DEFINE_FINAL_TYPE(AstalWpEndpoint, astal_wp_endpoint, ASTAL_WP_TYPE_NODE);

typedef enum {
    ASTAL_WP_ENDPOINT_PROP_DEVICE_ID = 1,
    ASTAL_WP_ENDPOINT_PROP_DEVICE,
    ASTAL_WP_ENDPOINT_N_PROPERTIES,
} AstalWpEndpointProperties;

static GParamSpec *astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_N_PROPERTIES] = {
    NULL,
};

guint astal_wp_endpoint_get_device_id(AstalWpEndpoint* self) {
  return self->device_id;
}

AstalWpDevice *astal_wp_endpoint_get_device(AstalWpEndpoint* self) {
  AstalWpWp *wp;
  g_object_get(self, "wp", &wp, NULL);
  return astal_wp_wp_get_device(wp, self->device_id);
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
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

// static void astal_wp_endpoint_set_property(GObject *object, guint property_id, const GValue
// *value,
//                                            GParamSpec *pspec) {
//     AstalWpEndpoint *self = ASTAL_WP_ENDPOINT(object);
//
//     switch (property_id) {
//         default:
//             G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
//             break;
//     }
// }


static void astal_wp_endpoint_properties_changed(AstalWpEndpoint* self) {

    WpNode *node;
    AstalWpMediaClass media_class;
    g_object_get(G_OBJECT(self), "node", &node, "media-class", &media_class, NULL);
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
    if(device != NULL) {
      value = astal_wp_device_get_icon(device);
    }
    if(value == NULL) {
      value = media_class == ASTAL_WP_MEDIA_CLASS_AUDIO_SPEAKER
                           ? "audio-card-symbolic"
                           : "audio-input-microphone-symbolic";
    }
    astal_wp_node_set_icon(ASTAL_WP_NODE(self), value);

}

void astal_wp_endpoint_real_params_changed(AstalWpNode* node, const gchar* id) {
  AstalWpEndpoint* self = ASTAL_WP_ENDPOINT(node);
  
  g_object_freeze_notify(G_OBJECT(self));

  if(!g_strcmp0(id, "Props")) astal_wp_endpoint_properties_changed(self);

  ASTAL_WP_NODE_CLASS(astal_wp_endpoint_parent_class)->params_changed(node, id);
  g_object_thaw_notify(G_OBJECT(self));
}


AstalWpEndpoint *astal_wp_endpoint_new_default(AstalWpWp *wp) {
    return g_object_new(ASTAL_WP_TYPE_ENDPOINT, "wp", wp, "is-default-node", TRUE, NULL);
}

AstalWpEndpoint *astal_wp_endpoint_new(WpNode *node, WpPlugin *mixer, WpPlugin *defaults,
                                       AstalWpWp *wp) {
    return g_object_new(ASTAL_WP_TYPE_ENDPOINT, "node", node, "mixer-plugin", mixer,
                        "default-plugin", defaults, "wp", wp, "is-default-node", FALSE, NULL);
}

static void astal_wp_endpoint_init(AstalWpEndpoint *self) {}

static void astal_wp_endpoint_class_init(AstalWpEndpointClass *class) {
    GObjectClass *object_class = G_OBJECT_CLASS(class);
    object_class->get_property = astal_wp_endpoint_get_property;
    // object_class->set_property = astal_wp_endpoint_set_property;

    AstalWpNodeClass *node_class = ASTAL_WP_NODE_CLASS(class);
    node_class->params_changed = astal_wp_endpoint_real_params_changed;

    astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_PROP_DEVICE_ID] = g_param_spec_uint("device-id", "device-id", "device-id", 0, UINT_MAX, 0, G_PARAM_READABLE);

    astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_PROP_DEVICE] = g_param_spec_object("device", "device", "device", ASTAL_WP_TYPE_DEVICE, G_PARAM_READABLE);

    g_object_class_install_properties(object_class, ASTAL_WP_ENDPOINT_N_PROPERTIES,
    astal_wp_endpoint_properties);
}
