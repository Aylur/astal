#include "stream.h"

#include <wp/wp.h>

#include "astal-wp-enum-types.h"
#include "endpoint.h"
#include "enums.h"
#include "glib-object.h"
#include "glib.h"
#include "node-private.h"
#include "node.h"
#include "wp-private.h"

struct _AstalWpStream {
    AstalWpNode parent_instance;

    gint target_serial;
    AstalWpMediaRole media_role;
    AstalWpMediaCategory media_category;
};

G_DEFINE_FINAL_TYPE(AstalWpStream, astal_wp_stream, ASTAL_WP_TYPE_NODE);

typedef enum {
    ASTAL_WP_STREAM_PROP_TARGET_SERIAL = 1,
    ASTAL_WP_STREAM_PROP_TARGET_ENDPOINT,
    ASTAL_WP_STREAM_PROP_MEDIA_CATEGORY,
    ASTAL_WP_STREAM_PROP_MEDIA_ROLE,
    ASTAL_WP_STREAM_N_PROPERTIES,
} AstalWpStreamProperties;

static GParamSpec *astal_wp_stream_properties[ASTAL_WP_STREAM_N_PROPERTIES] = {
    NULL,
};

gint astal_wp_stream_get_target_serial(AstalWpStream *self) {
    g_return_val_if_fail(self != NULL, -1);
    return self->target_serial;
}

void astal_wp_stream_set_target_serial(AstalWpStream *self, gint serial) {
    g_return_if_fail(self != NULL);
    AstalWpWp *wp;
    guint id;
    gchar *serial_str = g_strdup_printf("%d", serial);
    g_object_get(self, "wp", &wp, "id", &id, NULL);
    astal_wp_wp_set_matadata(wp, id, "target.object", "Spa:Id", serial_str);
    g_free(serial_str);
}

/**
 * astal_wp_stream_get_target_endpoint
 *
 * get the target [class@AstalWp.Endpoint]
 *
 * Returns: (transfer none)
 */
AstalWpEndpoint *astal_wp_stream_get_target_endpoint(AstalWpStream *self) {
    g_return_val_if_fail(self != NULL, NULL);
    AstalWpWp *wp;
    g_object_get(self, "wp", &wp, NULL);

    AstalWpNode *node = astal_wp_wp_get_node_by_serial(wp, self->target_serial);
    if (node != NULL && ASTAL_WP_IS_ENDPOINT(node)) return ASTAL_WP_ENDPOINT(node);
    return NULL;
}

void astal_wp_stream_set_target_endpoint(AstalWpStream *self, AstalWpEndpoint *target) {
    g_return_if_fail(self != NULL);
    if (target == NULL)
        astal_wp_stream_set_target_serial(self, -1);
    else
        astal_wp_stream_set_target_serial(self, astal_wp_node_get_serial(ASTAL_WP_NODE(target)));
}

AstalWpMediaRole astal_wp_stream_get_media_role(AstalWpStream *self) {
    g_return_val_if_fail(self != NULL, ASTAL_WP_MEDIA_ROLE_UNKNOWN);
    return self->media_role;
}

AstalWpMediaCategory astal_wp_stream_get_media_category(AstalWpStream *self) {
    g_return_val_if_fail(self != NULL, ASTAL_WP_MEDIA_CATEGORY_UNKNOWN);
    return self->media_category;
}

static void astal_wp_stream_get_property(GObject *object, guint property_id, GValue *value,
                                         GParamSpec *pspec) {
    AstalWpStream *self = ASTAL_WP_STREAM(object);

    switch (property_id) {
        case ASTAL_WP_STREAM_PROP_TARGET_SERIAL:
            g_value_set_int(value, self->target_serial);
            break;
        case ASTAL_WP_STREAM_PROP_TARGET_ENDPOINT:
            g_value_set_object(value, astal_wp_stream_get_target_endpoint(self));
            break;
        case ASTAL_WP_STREAM_PROP_MEDIA_ROLE:
            g_value_set_enum(value, self->media_role);
            break;
        case ASTAL_WP_STREAM_PROP_MEDIA_CATEGORY:
            g_value_set_enum(value, self->media_category);
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
        case ASTAL_WP_STREAM_PROP_TARGET_ENDPOINT:
            astal_wp_stream_set_target_endpoint(self, g_value_get_object(value));
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
    gint serial;

    if (!g_strcmp0(key, "target.object")) {
        if (value == NULL) {
            serial = -1;
        } else {
            serial = g_ascii_strtoll(value, NULL, 10);
        }
        if (serial != self->target_serial) {
            self->target_serial = serial;
            g_object_notify(G_OBJECT(self), "target-serial");
            g_object_notify(G_OBJECT(self), "target-endpoint");
        }
    }
}

static void astal_wp_stream_properties_changed(AstalWpStream *self) {
    WpNode *node;
    g_object_get(G_OBJECT(self), "node", &node, NULL);
    WpPipewireObject *pwo = WP_PIPEWIRE_OBJECT(node);

    const gchar *value;

    value = wp_pipewire_object_get_property(pwo, "media.icon-name");
    if (value == NULL) value = wp_pipewire_object_get_property(pwo, "window.icon-name");
    if (value == NULL) value = wp_pipewire_object_get_property(pwo, "application.icon-name");
    if (value == NULL) value = "application-x-executable-symbolic";
    astal_wp_node_set_icon(ASTAL_WP_NODE(self), value);

    value = wp_pipewire_object_get_property(pwo, "media.role");
    AstalWpMediaRole role = astal_wp_media_role_from_string(value);
    if (role != self->media_role) {
        self->media_role = role;
        g_object_notify(G_OBJECT(self), "media-role");
    }

    value = wp_pipewire_object_get_property(pwo, "media.category");
    AstalWpMediaCategory category = astal_wp_media_category_from_string(value);
    if (category != self->media_category) {
        self->media_category = category;
        g_object_notify(G_OBJECT(self), "media-category");
    }
}

void astal_wp_stream_real_params_changed(AstalWpNode *node, const gchar *id) {
    AstalWpStream *self = ASTAL_WP_STREAM(node);

    g_object_freeze_notify(G_OBJECT(self));

    if (!g_strcmp0(id, "Props")) astal_wp_stream_properties_changed(self);

    ASTAL_WP_NODE_CLASS(astal_wp_stream_parent_class)->params_changed(node, id);
    g_object_thaw_notify(G_OBJECT(self));
}

AstalWpStream *astal_wp_stream_new(WpNode *node, WpPlugin *mixer, AstalWpWp *wp) {
    return g_object_new(ASTAL_WP_TYPE_STREAM, "mixer-plugin", mixer, "node", node, "wp", wp, NULL);
}

static void astal_wp_stream_init(AstalWpStream *self) {}

static void astal_wp_stream_class_init(AstalWpStreamClass *class) {
    GObjectClass *object_class = G_OBJECT_CLASS(class);
    object_class->get_property = astal_wp_stream_get_property;
    object_class->set_property = astal_wp_stream_set_property;

    AstalWpNodeClass *node_class = ASTAL_WP_NODE_CLASS(class);
    node_class->metadata_changed = astal_wp_stream_real_metadata_changed;
    node_class->params_changed = astal_wp_stream_real_params_changed;

    /**
     * AstalWpWp:target-serial:
     *
     * The serial of the target endpoint of this node. Set it to -1 for the default.
     */
    astal_wp_stream_properties[ASTAL_WP_STREAM_PROP_TARGET_SERIAL] = g_param_spec_int(
        "target-serial", "target-serial", "The object serial number of the target node", INT_MIN,
        INT_MAX, -1, G_PARAM_READWRITE | G_PARAM_EXPLICIT_NOTIFY);
    /**
     * AstalWpWp:target-endpoint:
     *
     * The target endpoint for this node.
     */
    astal_wp_stream_properties[ASTAL_WP_STREAM_PROP_TARGET_ENDPOINT] =
        g_param_spec_object("target-endpoint", "target-endpoint", "target-endpoint",
                            ASTAL_WP_TYPE_ENDPOINT, G_PARAM_READWRITE | G_PARAM_EXPLICIT_NOTIFY);
    /**
     * AstalWpStream:media-role: (type AstalWpMediaRole)
     *
     * the media role of this stream.
     */
    astal_wp_stream_properties[ASTAL_WP_STREAM_PROP_MEDIA_ROLE] =
        g_param_spec_enum("media-role", "media-role", "media-role", ASTAL_WP_TYPE_MEDIA_ROLE,
                          ASTAL_WP_MEDIA_ROLE_UNKNOWN, G_PARAM_READABLE);
    /**
     * AstalWpStream:media-category: (type AstalWpMediaCategory)
     *
     * the media category of this stream.
     */
    astal_wp_stream_properties[ASTAL_WP_STREAM_PROP_MEDIA_CATEGORY] = g_param_spec_enum(
        "media-category", "media-category", "media-category", ASTAL_WP_TYPE_MEDIA_CATEGORY,
        ASTAL_WP_MEDIA_CATEGORY_UNKNOWN, G_PARAM_READABLE);

    g_object_class_install_properties(object_class, ASTAL_WP_STREAM_N_PROPERTIES,
                                      astal_wp_stream_properties);
}
