#include "stream.h"

#include <wp/wp.h>

#include "node-private.h"
#include "node.h"

struct _AstalWpStream {
    AstalWpNode parent_instance;
};

G_DEFINE_FINAL_TYPE(AstalWpStream, astal_wp_stream, ASTAL_WP_TYPE_NODE);

typedef enum {
    ASTAL_WP_STREAM_PROP_INDEX = 1,
    ASTAL_WP_STREAM_N_PROPERTIES,
} AstalWpStreamProperties;

static GParamSpec *astal_wp_stream_properties[ASTAL_WP_STREAM_N_PROPERTIES] = {
    NULL,
};

static void astal_wp_stream_get_property(GObject *object, guint property_id, GValue *value,
                                         GParamSpec *pspec) {
    AstalWpStream *self = ASTAL_WP_STREAM(object);

    switch (property_id) {
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_wp_stream_set_property(GObject *object, guint property_id, const GValue *value,
                                         GParamSpec *pspec) {
    AstalWpStream *self = ASTAL_WP_STREAM(object);

    switch (property_id) {
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

AstalWpStream *astal_wp_stream_new(WpNode *node, WpPlugin *mixer, WpPlugin *defaults,
                                   AstalWpWp *wp) {
    return g_object_new(ASTAL_WP_TYPE_STREAM, "node", node, "mixer-plugin", mixer, "default-plugin", defaults, "wp", wp, "is-default-node", FALSE, NULL);
}

static void astal_wp_stream_init(AstalWpStream *self) {}

static void astal_wp_stream_finalize(GObject *object) {
    AstalWpStream *self = ASTAL_WP_STREAM(object);
}

static void astal_wp_stream_class_init(AstalWpStreamClass *class) {
    GObjectClass *object_class = G_OBJECT_CLASS(class);
    object_class->finalize = astal_wp_stream_finalize;
    object_class->get_property = astal_wp_stream_get_property;
    object_class->set_property = astal_wp_stream_set_property;

    // g_object_class_install_properties(object_class, ASTAL_WP_PROFILE_N_PROPERTIES,
    // astal_wp_profile_properties);
}
