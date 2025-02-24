#include "stream.h"

#include <wp/wp.h>

#include "glib-object.h"
#include "glib.h"
#include "node-private.h"
#include "node.h"
#include "wp-private.h"

struct _AstalWpStream {
    AstalWpNode parent_instance;

    gint target_serial;
};

G_DEFINE_FINAL_TYPE(AstalWpStream, astal_wp_stream, ASTAL_WP_TYPE_NODE);

typedef enum {
    ASTAL_WP_STREAM_PROP_TARGET_SERIAL = 1,
    ASTAL_WP_STREAM_N_PROPERTIES,
} AstalWpStreamProperties;

static GParamSpec *astal_wp_stream_properties[ASTAL_WP_STREAM_N_PROPERTIES] = {
    NULL,
};

gint astal_wp_stream_get_target_serial(AstalWpStream *self) { return self->target_serial; }

void astal_wp_stream_set_target_serial(AstalWpStream *self, gint serial) {
    AstalWpWp *wp;
    guint id;
    gchar *serial_str = g_strdup_printf("%d", serial);
    g_object_get(self, "wp", &wp, "id", &id, NULL);
    astal_wp_wp_set_matadata(wp, id, "target.object", "Spa:Id", serial_str);
    g_free(serial_str);
}

static void astal_wp_stream_get_property(GObject *object, guint property_id, GValue *value,
                                         GParamSpec *pspec) {
    AstalWpStream *self = ASTAL_WP_STREAM(object);

    switch (property_id) {
        case ASTAL_WP_STREAM_PROP_TARGET_SERIAL:
            g_value_set_int(value, self->target_serial);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_wp_stream_set_property(GObject *object, guint property_id, const GValue *value,
                                         GParamSpec *pspec) {
    AstalWpStream *self = ASTAL_WP_STREAM(object);

    switch (property_id) {
        case ASTAL_WP_STREAM_PROP_TARGET_SERIAL:
            astal_wp_stream_set_target_serial(self, g_value_get_int(value));
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

void astal_wp_stream_real_metadata_changed(AstalWpNode *node, const gchar *key, const gchar *type,
                                           const gchar *value) {
    ASTAL_WP_NODE_CLASS(astal_wp_stream_parent_class)->metadata_changed(node, key, type, value);

    AstalWpStream *self = ASTAL_WP_STREAM(node);

    if (g_str_equal(key, "target.object")) {
        if (value == NULL) {
            self->target_serial = -1;
        } else {
            guint serial = g_ascii_strtoll(value, NULL, 10);
            self->target_serial = serial;
        }
        g_object_notify(G_OBJECT(self), "target-serial");
    }
}

AstalWpStream *astal_wp_stream_new(WpNode *node, WpPlugin *mixer, WpPlugin *defaults,
                                   AstalWpWp *wp) {
    return g_object_new(ASTAL_WP_TYPE_STREAM, "node", node, "mixer-plugin", mixer, "default-plugin",
                        defaults, "wp", wp, "is-default-node", FALSE, NULL);
}

static void astal_wp_stream_constructed(GObject *object) {
    AstalWpStream *self = ASTAL_WP_STREAM(object);

    WpNode *node;
    g_object_get(self, "node", &node, NULL);
    const gchar *target_object =
        wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(node), "target.object");
    if (target_object == NULL) {
        self->target_serial = -1;
    } else {
        const gint serial = g_ascii_strtoull(target_object, NULL, 10);
        self->target_serial = serial;
    }

    G_OBJECT_CLASS(astal_wp_stream_parent_class)->constructed(object);
}

static void astal_wp_stream_init(AstalWpStream *self) {}

static void astal_wp_stream_class_init(AstalWpStreamClass *class) {
    GObjectClass *object_class = G_OBJECT_CLASS(class);
    object_class->get_property = astal_wp_stream_get_property;
    object_class->set_property = astal_wp_stream_set_property;
    object_class->constructed = astal_wp_stream_constructed;

    AstalWpNodeClass *node_class = ASTAL_WP_NODE_CLASS(class);
    node_class->metadata_changed = astal_wp_stream_real_metadata_changed;

    astal_wp_stream_properties[ASTAL_WP_STREAM_PROP_TARGET_SERIAL] = g_param_spec_int(
        "target-serial", "target-serial", "The object serial number of the target node", INT_MIN,
        INT_MAX, -1, G_PARAM_READWRITE);

    g_object_class_install_properties(object_class, ASTAL_WP_STREAM_N_PROPERTIES,
                                      astal_wp_stream_properties);
}
