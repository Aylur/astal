#include "endpoint.h"
#include "glib-object.h"
#include "node-private.h"

#include <wp/wp.h>

#include "node.h"


struct _AstalWpEndpoint {
    AstalWpNode parent_instance;
};

G_DEFINE_FINAL_TYPE(AstalWpEndpoint, astal_wp_endpoint, ASTAL_WP_TYPE_NODE);

typedef enum {
    ASTAL_WP_ENDPOINT_PROP_INDEX = 1,
    ASTAL_WP_ENDPOINT_N_PROPERTIES,
} AstalWpEndpointProperties;

static GParamSpec *astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_N_PROPERTIES] = {
    NULL,
};

static void astal_wp_endpoint_get_property(GObject *object, guint property_id, GValue *value,
                                           GParamSpec *pspec) {
    AstalWpEndpoint *self = ASTAL_WP_ENDPOINT(object);

    switch (property_id) {
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_wp_endpoint_set_property(GObject *object, guint property_id, const GValue *value,
                                           GParamSpec *pspec) {
    AstalWpEndpoint *self = ASTAL_WP_ENDPOINT(object);

    switch (property_id) {
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

AstalWpEndpoint *astal_wp_endpoint_new_default(AstalWpWp *wp) {
  return g_object_new(ASTAL_WP_TYPE_ENDPOINT, "wp", wp, "is-default-node", TRUE, NULL);
}

AstalWpEndpoint *astal_wp_endpoint_new(WpNode *node, WpPlugin *mixer, WpPlugin *defaults, AstalWpWp *wp) {
  return g_object_new(ASTAL_WP_TYPE_ENDPOINT, "node", node, "mixer-plugin", mixer, "default-plugin", defaults, "wp", wp, "is-default-node", FALSE, NULL);
}

static void astal_wp_endpoint_init(AstalWpEndpoint *self) {}

static void astal_wp_endpoint_finalize(GObject *object) {
    AstalWpEndpoint *self = ASTAL_WP_ENDPOINT(object);
}

static void astal_wp_endpoint_class_init(AstalWpEndpointClass *class) {
    GObjectClass *object_class = G_OBJECT_CLASS(class);
    object_class->finalize = astal_wp_endpoint_finalize;
    object_class->get_property = astal_wp_endpoint_get_property;
    object_class->set_property = astal_wp_endpoint_set_property;

    // g_object_class_install_properties(object_class, ASTAL_WP_PROFILE_N_PROPERTIES,
    // astal_wp_profile_properties);
}
