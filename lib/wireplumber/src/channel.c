#include "channel.h"

#include "channel-private.h"
#include "node-private.h"

struct _AstalWpChannel {
    GObject parent_instance;

    AstalWpNode *node;

    gchar *name;
    gdouble volume;
};

G_DEFINE_TYPE(AstalWpChannel, astal_wp_channel, G_TYPE_OBJECT);

typedef enum {
    ASTAL_WP_CHANNEL_PROP_NAME = 1,
    ASTAL_WP_CHANNEL_PROP_VOLUME,
    ASTAL_WP_CHANNEL_PROP_VOLUME_ICON,
    ASTAL_WP_CHANNEL_N_PROPERTIES,
} AstalWpChannelProperties;

static GParamSpec *astal_wp_channel_properties[ASTAL_WP_CHANNEL_N_PROPERTIES] = {
    NULL,
};

/**
 * astal_wp_channel_set_volume:
 *
 * sets the volume for this channel. Note that if [property@AstalWp.Node:lock-channels] is true for
 * the node this channel is associated with, this method will set the volume for all channels.
 */
void astal_wp_channel_set_volume(AstalWpChannel *self, gdouble volume) {
    astal_wp_node_set_channel_volume(self->node, self->name, volume);
}

void astal_wp_channel_update_volume(AstalWpChannel *self, gdouble volume) {
    if (volume == self->volume) return;
    self->volume = volume;
    g_object_notify(G_OBJECT(self), "volume");
    g_object_notify(G_OBJECT(self), "volume-icon");
}

/**
 * astal_wp_channel_get_name:
 *
 * the name of the channel
 *
 * Returns: (nullable)
 */
const gchar *astal_wp_channel_get_name(AstalWpChannel *self) { return self->name; }

/**
 * astal_wp_channel_get_volume:
 *
 * the volume of the channel
 */
gdouble astal_wp_channel_get_volume(AstalWpChannel *self) { return self->volume; }

const gchar *astal_wp_channel_get_volume_icon(AstalWpChannel *self) {
    if (self->volume == 0) return "audio-volume-muted-symbolic";
    if (self->volume <= 0.33) return "audio-volume-low-symbolic";
    if (self->volume <= 0.66) return "audio-volume-medium-symbolic";
    if (self->volume <= 1) return "audio-volume-high-symbolic";
    return "audio-volume-overamplified-symbolic";
}

void astal_wp_channel_set_property(GObject *gobject, guint property_id, const GValue *value,
                                   GParamSpec *pspec) {
    AstalWpChannel *self = ASTAL_WP_CHANNEL(gobject);

    switch (property_id) {
        case ASTAL_WP_CHANNEL_PROP_NAME:
            g_free(self->name);
            self->name = g_value_dup_string(value);
            break;
        case ASTAL_WP_CHANNEL_PROP_VOLUME:
            astal_wp_channel_set_volume(self, g_value_get_double(value));
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(gobject, property_id, pspec);
            break;
    }
}

void astal_wp_channel_get_property(GObject *gobject, guint property_id, GValue *value,
                                   GParamSpec *pspec) {
    AstalWpChannel *self = ASTAL_WP_CHANNEL(gobject);

    switch (property_id) {
        case ASTAL_WP_CHANNEL_PROP_NAME:
            g_value_set_string(value, self->name);
            break;
        case ASTAL_WP_CHANNEL_PROP_VOLUME:
            g_value_set_double(value, self->volume);
            break;
        case ASTAL_WP_CHANNEL_PROP_VOLUME_ICON:
            g_value_set_string(value, astal_wp_channel_get_volume_icon(self));
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(gobject, property_id, pspec);
            break;
    }
}

AstalWpChannel *astal_wp_channel_new(AstalWpNode *ep, const gchar *name) {
    AstalWpChannel *self = g_object_new(ASTAL_WP_TYPE_CHANNEL, "name", name, NULL);
    self->node = ep;
    return self;
}

static void astal_wp_channel_finalize(GObject *gobject) {
    AstalWpChannel *self = ASTAL_WP_CHANNEL(gobject);
    g_free(self->name);

    G_OBJECT_CLASS(astal_wp_channel_parent_class)->finalize(gobject);
}

static void astal_wp_channel_class_init(AstalWpChannelClass *klass) {
    GObjectClass *object_class = G_OBJECT_CLASS(klass);

    object_class->set_property = astal_wp_channel_set_property;
    object_class->get_property = astal_wp_channel_get_property;
    object_class->finalize = astal_wp_channel_finalize;

    astal_wp_channel_properties[ASTAL_WP_CHANNEL_PROP_NAME] = g_param_spec_string(
        "name", "name", "name of the channel", NULL, G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE);

    astal_wp_channel_properties[ASTAL_WP_CHANNEL_PROP_VOLUME] = g_param_spec_double(
        "volume", "volume", "volume for this channel.", 0, G_MAXFLOAT, 0, G_PARAM_READWRITE);

    astal_wp_channel_properties[ASTAL_WP_CHANNEL_PROP_VOLUME_ICON] = g_param_spec_string(
        "volume-icon", "volume-icon", "volume-icon", "audio-volume-muted", G_PARAM_READABLE);

    g_object_class_install_properties(object_class, ASTAL_WP_CHANNEL_N_PROPERTIES,
                                      astal_wp_channel_properties);
}

static void astal_wp_channel_init(AstalWpChannel *self) {}
