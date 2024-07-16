#include <limits.h>
#include <wp/wp.h>

#include "endpoint-private.h"
#include "glib-object.h"
#include "glib.h"

struct _AstalWpEndpoint {
    GObject parent_instance;

    guint id;
    gdouble volume;
    gboolean mute;
    const gchar *description;
};

typedef struct {
    WpNode *node;
    WpPlugin *mixer;

} AstalWpEndpointPrivate;

G_DEFINE_FINAL_TYPE_WITH_PRIVATE(AstalWpEndpoint, astal_wp_endpoint, G_TYPE_OBJECT);

typedef enum {
    ASTAL_WP_ENDPOINT_PROP_ID = 1,
    ASTAL_WP_ENDPOINT_PROP_VOLUME,
    ASTAL_WP_ENDPOINT_PROP_MUTE,
    ASTAL_WP_ENDPOINT_PROP_DESCRIPTION,
    ASTAL_WP_ENDPOINT_N_PROPERTIES,
} AstalWpEndpointProperties;

typedef enum {
    ASTAL_WP_ENDPOINT_SIGNAL_CHANGED,
    ASTAL_WP_ENDPOINT_N_SIGNALS
} AstalWpEndpointSignals;

static guint astal_wp_endpoint_signals[ASTAL_WP_ENDPOINT_N_SIGNALS] = {
    0,
};
static GParamSpec *astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_N_PROPERTIES] = {
    NULL,
};

void astal_wp_endpoint_update_volume(AstalWpEndpoint *self) {
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);

    gdouble volume;
    gboolean mute;
    GVariant *variant = NULL;

    g_signal_emit_by_name(priv->mixer, "get-volume", self->id, &variant);

    if (variant == NULL) return;

    g_variant_lookup(variant, "volume", "d", &volume);
    g_variant_lookup(variant, "mute", "b", &mute);

    if (mute != self->mute) {
        self->mute = mute;
        g_object_notify(G_OBJECT(self), "mute");
    }

    if (volume != self->volume) {
        self->volume = volume;
        g_object_notify(G_OBJECT(self), "volume");
    }

    g_signal_emit_by_name(self, "changed");
}

void astal_wp_endpoint_set_volume(AstalWpEndpoint *self, gdouble volume) {
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);

    GVariant *variant = NULL;
    GVariantBuilder b = G_VARIANT_BUILDER_INIT(G_VARIANT_TYPE_VARDICT);
    g_variant_builder_add(&b, "{sv}", "volume", g_variant_new_double(volume));
    variant = g_variant_builder_end(&b);

    g_signal_emit_by_name(priv->mixer, "set-volume", self->id, variant);
    g_variant_unref(variant);
}

void astal_wp_endpoint_set_mute(AstalWpEndpoint *self, gboolean mute) {
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);

    GVariant *variant = NULL;
    GVariantBuilder b = G_VARIANT_BUILDER_INIT(G_VARIANT_TYPE_VARDICT);
    g_variant_builder_add(&b, "{sv}", "mute", g_variant_new_boolean(mute));
    variant = g_variant_builder_end(&b);

    g_signal_emit_by_name(priv->mixer, "set-volume", self->id, variant);

    g_variant_unref(variant);
}

static void astal_wp_endpoint_get_property(GObject *object, guint property_id, GValue *value,
                                           GParamSpec *pspec) {
    AstalWpEndpoint *self = ASTAL_WP_ENDPOINT(object);

    switch (property_id) {
        case ASTAL_WP_ENDPOINT_PROP_ID:
            g_value_set_uint(value, self->id);
            break;
        case ASTAL_WP_ENDPOINT_PROP_MUTE:
            g_value_set_boolean(value, self->mute);
            break;
        case ASTAL_WP_ENDPOINT_PROP_VOLUME:
            g_value_set_double(value, self->volume);
            break;
        case ASTAL_WP_ENDPOINT_PROP_DESCRIPTION:
            g_value_set_string(value, self->description);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_wp_endpoint_set_property(GObject *object, guint property_id, const GValue *value,
                                           GParamSpec *pspec) {
    AstalWpEndpoint *self = ASTAL_WP_ENDPOINT(object);

    switch (property_id) {
        case ASTAL_WP_ENDPOINT_PROP_MUTE:
            astal_wp_endpoint_set_mute(self, g_value_get_boolean(value));
            break;
        case ASTAL_WP_ENDPOINT_PROP_VOLUME:
            astal_wp_endpoint_set_volume(self, g_value_get_double(value));
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

AstalWpEndpoint *astal_wp_endpoint_create(WpNode *node, WpPlugin *mixer) {
    AstalWpEndpoint *self = g_object_new(ASTAL_WP_TYPE_ENDPOINT, NULL);
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);

    priv->mixer = g_object_ref(mixer);
    priv->node = g_object_ref(node);

    self->id = wp_proxy_get_bound_id(WP_PROXY(node));

    astal_wp_endpoint_update_volume(self);

    const gchar *description =
        wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(node), "node.description");
    if (description == NULL) {
        description = wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(node), "node.nick");
    }
    if (description == NULL) {
        description = wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(node), "node.name");
    }
    if (description == NULL) {
        description = "unknown";
    }

    self->description = g_strdup(description);

    return self;
}

static void astal_wp_endpoint_init(AstalWpEndpoint *self) {
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);
    priv->node = NULL;
    priv->mixer = NULL;

    self->volume = 0;
    self->mute = TRUE;
    self->description = NULL;
}

static void astal_wp_endpoint_dispose(GObject *object) {
    AstalWpEndpoint *self = ASTAL_WP_ENDPOINT(object);
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);
    g_clear_object(&priv->node);
    g_clear_object(&priv->mixer);
}

static void astal_wp_endpoint_finalize(GObject *object) {}

static void astal_wp_endpoint_class_init(AstalWpEndpointClass *class) {
    GObjectClass *object_class = G_OBJECT_CLASS(class);
    object_class->dispose = astal_wp_endpoint_dispose;
    object_class->finalize = astal_wp_endpoint_finalize;
    object_class->get_property = astal_wp_endpoint_get_property;
    object_class->set_property = astal_wp_endpoint_set_property;

    astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_PROP_ID] =
        g_param_spec_uint("id", "id", "id", 0, UINT_MAX, 0, G_PARAM_READABLE);
    astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_PROP_VOLUME] =
        g_param_spec_double("volume", "volume", "volume", 0, 1, 0, G_PARAM_READWRITE);
    astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_PROP_MUTE] =
        g_param_spec_boolean("mute", "mute", "mute", TRUE, G_PARAM_READWRITE);
    astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_PROP_DESCRIPTION] =
        g_param_spec_string("description", "description", "description", NULL, G_PARAM_READABLE);

    g_object_class_install_properties(object_class, ASTAL_WP_ENDPOINT_N_PROPERTIES,
                                      astal_wp_endpoint_properties);

    astal_wp_endpoint_signals[ASTAL_WP_ENDPOINT_SIGNAL_CHANGED] =
        g_signal_new("changed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL, NULL,
                     G_TYPE_NONE, 0);
}
