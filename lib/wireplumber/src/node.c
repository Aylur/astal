#include "node.h"

#include <stdio.h>
#include <wp/wp.h>

#include "device.h"
#include "glib-object.h"
#include "glib.h"
#include "node-private.h"
#include "wp.h"
#include "wp/node.h"
#include "wp/plugin.h"

struct _AstalWpChannelVolume {
    GObject parent_instance;

    AstalWpNode *node;

    gchar *name;
    gdouble volume;
};

G_DEFINE_TYPE(AstalWpChannelVolume, astal_wp_channel_volume, G_TYPE_OBJECT);

typedef enum {
    ASTAL_WP_CHANNEL_VOLUME_PROP_NAME = 1,
    ASTAL_WP_CHANNEL_VOLUME_PROP_VOLUME,
    ASTAL_WP_CHANNEL_VOLUME_PROP_VOLUME_ICON,
    ASTAL_WP_CHANNEL_VOLUME_N_PROPERTIES,
} AstalWpChannelVolumeProperties;

static GParamSpec *astal_wp_channel_volume_properties[ASTAL_WP_CHANNEL_VOLUME_N_PROPERTIES] = {
    NULL,
};

void astal_wp_channel_volume_set_volume(AstalWpChannelVolume *self, gdouble volume) {
    astal_wp_node_set_channel_volume(self->node, self->name, volume);
}

void astal_wp_channel_volume_update_volume(AstalWpChannelVolume *self, gdouble volume) {
    if (volume == self->volume) return;
    self->volume = volume;
    g_object_notify(G_OBJECT(self), "volume");
    g_object_notify(G_OBJECT(self), "volume-icon");
}

/**
 * astal_wp_channel_volume_get_name:
 *
 * the name of the channel
 *
 * Returns: (nullable)
 */
const gchar *astal_wp_channel_volume_get_name(AstalWpChannelVolume *self) { return self->name; }

gdouble astal_wp_channel_volume_get_volume(AstalWpChannelVolume *self) { return self->volume; }

const gchar *astal_wp_channel_volume_get_volume_icon(AstalWpChannelVolume *self) {
    if (self->volume == 0) return "audio-volume-muted-symbolic";
    if (self->volume <= 0.33) return "audio-volume-low-symbolic";
    if (self->volume <= 0.66) return "audio-volume-medium-symbolic";
    if (self->volume <= 1) return "audio-volume-high-symbolic";
    return "audio-volume-overamplified-symbolic";
}

void astal_wp_channel_volume_set_property(GObject *gobject, guint property_id, const GValue *value,
                                          GParamSpec *pspec) {
    AstalWpChannelVolume *self = ASTAL_WP_CHANNEL_VOLUME(gobject);

    switch (property_id) {
        case ASTAL_WP_CHANNEL_VOLUME_PROP_NAME:
            g_free(self->name);
            self->name = g_value_dup_string(value);
            break;
        case ASTAL_WP_CHANNEL_VOLUME_PROP_VOLUME:
            astal_wp_channel_volume_set_volume(self, g_value_get_double(value));
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(gobject, property_id, pspec);
            break;
    }
}

void astal_wp_channel_volume_get_property(GObject *gobject, guint property_id, GValue *value,
                                          GParamSpec *pspec) {
    AstalWpChannelVolume *self = ASTAL_WP_CHANNEL_VOLUME(gobject);

    switch (property_id) {
        case ASTAL_WP_CHANNEL_VOLUME_PROP_NAME:
            g_value_set_string(value, self->name);
            break;
        case ASTAL_WP_CHANNEL_VOLUME_PROP_VOLUME:
            g_value_set_double(value, self->volume);
            break;
        case ASTAL_WP_CHANNEL_VOLUME_PROP_VOLUME_ICON:
            g_value_set_string(value, astal_wp_channel_volume_get_volume_icon(self));
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(gobject, property_id, pspec);
            break;
    }
}

static AstalWpChannelVolume *astal_wp_channel_volume_new(AstalWpNode *ep, const gchar *name) {
    AstalWpChannelVolume *self = g_object_new(ASTAL_WP_TYPE_CHANNEL_VOLUME, "name", name, NULL);
    self->node = ep;
    return self;
}

static void astal_wp_channel_volume_finalize(GObject *gobject) {
    AstalWpChannelVolume *self = ASTAL_WP_CHANNEL_VOLUME(gobject);
    g_free(self->name);

    G_OBJECT_CLASS(astal_wp_channel_volume_parent_class)->finalize(gobject);
}

static void astal_wp_channel_volume_class_init(AstalWpChannelVolumeClass *klass) {
    GObjectClass *object_class = G_OBJECT_CLASS(klass);

    object_class->set_property = astal_wp_channel_volume_set_property;
    object_class->get_property = astal_wp_channel_volume_get_property;
    object_class->finalize = astal_wp_channel_volume_finalize;

    astal_wp_channel_volume_properties[ASTAL_WP_CHANNEL_VOLUME_PROP_NAME] = g_param_spec_string(
        "name", "name", "name of the channel", NULL, G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE);

    astal_wp_channel_volume_properties[ASTAL_WP_CHANNEL_VOLUME_PROP_VOLUME] = g_param_spec_double(
        "volume", "volume", "volume for this channel.", 0, G_MAXFLOAT, 0, G_PARAM_READWRITE);

    astal_wp_channel_volume_properties[ASTAL_WP_CHANNEL_VOLUME_PROP_VOLUME_ICON] =
        g_param_spec_string("volume-icon", "volume-icon", "volume-icon", "audio-volume-muted",
                            G_PARAM_READABLE);

    g_object_class_install_properties(object_class, ASTAL_WP_CHANNEL_VOLUME_N_PROPERTIES,
                                      astal_wp_channel_volume_properties);
}

static void astal_wp_channel_volume_init(AstalWpChannelVolume *self) {}

typedef struct {
    WpNode *node;
    WpPlugin *mixer;
    WpPlugin *defaults;
    AstalWpWp *wp;

    gboolean is_default_node;
    AstalWpMediaClass media_class;

    gulong default_signal_handler_id;
    gulong mixer_signal_handler_id;

    guint id;
    gdouble volume;
    gboolean mute;
    gchar *description;
    gchar *name;
    guint serial;
    gchar *path;

    AstalWpMediaClass type;
    gboolean is_default;
    gboolean lock_channels;
    gchar *icon;
    GHashTable *channel_volumes;

} AstalWpNodePrivate;

G_DEFINE_TYPE_WITH_PRIVATE(AstalWpNode, astal_wp_node, G_TYPE_OBJECT);

G_DEFINE_ENUM_TYPE(AstalWpMediaClass, astal_wp_media_class,
                   G_DEFINE_ENUM_VALUE(ASTAL_WP_MEDIA_CLASS_AUDIO_MICROPHONE, "Audio/Source"),
                   G_DEFINE_ENUM_VALUE(ASTAL_WP_MEDIA_CLASS_AUDIO_SPEAKER, "Audio/Sink"),
                   G_DEFINE_ENUM_VALUE(ASTAL_WP_MEDIA_CLASS_AUDIO_RECORDER, "Stream/Input/Audio"),
                   G_DEFINE_ENUM_VALUE(ASTAL_WP_MEDIA_CLASS_AUDIO_STREAM, "Stream/Output/Audio"),
                   G_DEFINE_ENUM_VALUE(ASTAL_WP_MEDIA_CLASS_VIDEO_SOURCE, "Video/Source"),
                   G_DEFINE_ENUM_VALUE(ASTAL_WP_MEDIA_CLASS_VIDEO_SINK, "Video/Sink"),
                   G_DEFINE_ENUM_VALUE(ASTAL_WP_MEDIA_CLASS_VIDEO_RECORDER, "Stream/Input/Video"),
                   G_DEFINE_ENUM_VALUE(ASTAL_WP_MEDIA_CLASS_VIDEO_STREAM, "Stream/Output/Video"));

typedef enum {
    // private props
    ASTAL_WP_NODE_PROP_WP = 1,
    ASTAL_WP_NODE_PROP_NODE,
    ASTAL_WP_NODE_PROP_IS_DEFAULT_NODE,
    ASTAL_WP_NODE_PROP_MIXER_PLUGIN,
    ASTAL_WP_NODE_PROP_DEFAULT_PLUGIN,
    // public props
    ASTAL_WP_NODE_PROP_ID,
    ASTAL_WP_NODE_PROP_VOLUME,
    ASTAL_WP_NODE_PROP_MUTE,
    ASTAL_WP_NODE_PROP_DESCRIPTION,
    ASTAL_WP_NODE_PROP_NAME,
    ASTAL_WP_NODE_PROP_MEDIA_CLASS,
    ASTAL_WP_NODE_PROP_DEFAULT,
    ASTAL_WP_NODE_PROP_ICON,
    ASTAL_WP_NODE_PROP_VOLUME_ICON,
    ASTAL_WP_NODE_PROP_LOCK_CHANNELS,
    ASTAL_WP_NODE_PROP_SERIAL,
    ASTAL_WP_NODE_PROP_PATH,
    ASTAL_WP_NODE_PROP_CHANNEL_VOLUMES,
    ASTAL_WP_NODE_N_PROPERTIES,
} AstalWpNodeProperties;

static GParamSpec *astal_wp_node_properties[ASTAL_WP_NODE_N_PROPERTIES] = {
    NULL,
};

void astal_wp_node_update_volume(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);

    gdouble volume = 0;
    gboolean mute;
    GVariant *variant = NULL;
    GVariantIter *channels = NULL;

    g_signal_emit_by_name(priv->mixer, "get-volume", priv->id, &variant);

    if (variant == NULL) return;

    g_object_freeze_notify(G_OBJECT(self));

    g_variant_lookup(variant, "volume", "d", &volume);
    g_variant_lookup(variant, "mute", "b", &mute);
    g_variant_lookup(variant, "channelVolumes", "a{sv}", &channels);

    if (channels != NULL) {
        const gchar *key;
        const gchar *channel_str;
        gdouble channel_volume;
        GVariant *varvol;

        while (g_variant_iter_loop(channels, "{&sv}", &key, &varvol)) {
            g_variant_lookup(varvol, "volume", "d", &channel_volume);
            g_variant_lookup(varvol, "channel", "&s", &channel_str);
            if (channel_volume > volume) volume = channel_volume;

            AstalWpChannelVolume *cv = g_hash_table_lookup(priv->channel_volumes, channel_str);
            if (cv == NULL) {
                cv = astal_wp_channel_volume_new(self, channel_str);
                g_hash_table_insert(priv->channel_volumes, g_strdup(channel_str), cv);
                g_object_notify(G_OBJECT(self), "channel-volumes");
            }
            astal_wp_channel_volume_update_volume(cv, channel_volume);
        }
    }

    if (mute != priv->mute) {
        priv->mute = mute;
        g_object_notify(G_OBJECT(self), "mute");
    }

    if (volume != priv->volume) {
        priv->volume = volume;
        g_object_notify(G_OBJECT(self), "volume");
    }

    g_object_notify(G_OBJECT(self), "volume-icon");

    g_object_thaw_notify(G_OBJECT(self));
}

void astal_wp_node_set_channel_volume(AstalWpNode *self, const gchar *name, gdouble volume) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);

    gboolean ret;
    if (volume >= 1.5) volume = 1.5;
    if (volume <= 0) volume = 0;

    gboolean mute;
    GVariant *variant = NULL;
    GVariantIter *channels = NULL;

    g_auto(GVariantBuilder) vol_b = G_VARIANT_BUILDER_INIT(G_VARIANT_TYPE_VARDICT);
    g_signal_emit_by_name(priv->mixer, "get-volume", priv->id, &variant);

    if (variant == NULL) return;

    g_variant_lookup(variant, "mute", "b", &mute);
    g_variant_lookup(variant, "channelVolumes", "a{sv}", &channels);

    if (channels != NULL) {
        g_auto(GVariantBuilder) channel_volumes_b = G_VARIANT_BUILDER_INIT(G_VARIANT_TYPE_VARDICT);

        const gchar *key;
        const gchar *channel_str;
        gdouble channel_volume;
        GVariant *varvol;

        while (g_variant_iter_loop(channels, "{&sv}", &key, &varvol)) {
            g_auto(GVariantBuilder) channel_b = G_VARIANT_BUILDER_INIT(G_VARIANT_TYPE_VARDICT);
            g_variant_lookup(varvol, "volume", "d", &channel_volume);
            g_variant_lookup(varvol, "channel", "&s", &channel_str);
            gdouble vol = g_str_equal(name, channel_str) ? volume : channel_volume;
            g_variant_builder_add(&channel_b, "{sv}", "volume", g_variant_new_double(vol));
            g_variant_builder_add(&channel_volumes_b, "{sv}", key,
                                  g_variant_builder_end(&channel_b));
        }

        g_variant_builder_add(&vol_b, "{sv}", "channelVolumes",
                              g_variant_builder_end(&channel_volumes_b));
    }

    g_signal_emit_by_name(priv->mixer, "set-volume", priv->id, g_variant_builder_end(&vol_b), &ret);
}

/**
 * astal_wp_node_set_volume:
 * @self: the AstalWpNode object
 * @volume: The new volume level to set.
 *
 * Sets the volume level for this node. The volume is clamped to be between
 * 0 and 1.5.
 */
void astal_wp_node_set_volume(AstalWpNode *self, gdouble volume) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);

    gboolean ret;
    if (volume >= 1.5) volume = 1.5;
    if (volume <= 0) volume = 0;

    gboolean mute;
    GVariant *variant = NULL;
    GVariantIter *channels = NULL;

    g_auto(GVariantBuilder) vol_b = G_VARIANT_BUILDER_INIT(G_VARIANT_TYPE_VARDICT);
    g_signal_emit_by_name(priv->mixer, "get-volume", priv->id, &variant);

    if (variant == NULL) return;

    g_variant_lookup(variant, "mute", "b", &mute);
    g_variant_lookup(variant, "channelVolumes", "a{sv}", &channels);

    if (channels != NULL && !priv->lock_channels) {
        g_auto(GVariantBuilder) channel_volumes_b = G_VARIANT_BUILDER_INIT(G_VARIANT_TYPE_VARDICT);

        const gchar *key;
        const gchar *channel_str;
        gdouble channel_volume;
        GVariant *varvol;

        while (g_variant_iter_loop(channels, "{&sv}", &key, &varvol)) {
            g_auto(GVariantBuilder) channel_b = G_VARIANT_BUILDER_INIT(G_VARIANT_TYPE_VARDICT);
            g_variant_lookup(varvol, "volume", "d", &channel_volume);
            g_variant_lookup(varvol, "channel", "&s", &channel_str);
            gdouble vol = priv->volume == 0 ? volume : channel_volume * volume / priv->volume;
            g_variant_builder_add(&channel_b, "{sv}", "volume", g_variant_new_double(vol));
            g_variant_builder_add(&channel_volumes_b, "{sv}", key,
                                  g_variant_builder_end(&channel_b));
        }

        g_variant_builder_add(&vol_b, "{sv}", "channelVolumes",
                              g_variant_builder_end(&channel_volumes_b));
    } else {
        GVariant *volume_variant = g_variant_new_double(volume);
        g_variant_builder_add(&vol_b, "{sv}", "volume", volume_variant);
    }

    g_signal_emit_by_name(priv->mixer, "set-volume", priv->id, g_variant_builder_end(&vol_b), &ret);
}

/**
 * astal_wp_node_set_mute:
 * @self: the AstalWpNode instance.
 * @mute: A boolean indicating whether to mute the node.
 *
 * Sets the mute status for the node.
 */
void astal_wp_node_set_mute(AstalWpNode *self, gboolean mute) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);

    gboolean ret;
    GVariant *variant = NULL;
    GVariantBuilder b = G_VARIANT_BUILDER_INIT(G_VARIANT_TYPE_VARDICT);
    g_variant_builder_add(&b, "{sv}", "mute", g_variant_new_boolean(mute));
    variant = g_variant_builder_end(&b);

    g_signal_emit_by_name(priv->mixer, "set-volume", priv->id, variant, &ret);
}

/**
 * astal_wp_node_get_media_class:
 * @self: the AstalWpNode instance.
 *
 * gets the media class of the node.
 */
AstalWpMediaClass astal_wp_node_get_media_class(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    return priv->type;
}

/**
 * astal_wp_node_get_id:
 * @self: the AstalWpNode instance.
 *
 * gets the id of the node.
 */
guint astal_wp_node_get_id(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    return priv->id;
}

/**
 * astal_wp_node_get_mute:
 * @self: the AstalWpNode instance.
 *
 * gets the mute status of the node.
 */
gboolean astal_wp_node_get_mute(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    return priv->mute;
}

/**
 * astal_wp_node_get_volume:
 * @self: the AstalWpNode instance.
 *
 * gets the volume
 */
gdouble astal_wp_node_get_volume(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    return priv->volume;
}

/**
 * astal_wp_node_get_description:
 * @self: the AstalWpNode instance.
 *
 * gets the description of this node
 */
const gchar *astal_wp_node_get_description(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    return priv->description;
}

/**
 * astal_wp_node_get_name:
 * @self: the AstalWpNode instance.
 *
 * gets the name of this node
 */
const gchar *astal_wp_node_get_name(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    return priv->name;
}

/**
 * astal_wp_node_get_icon:
 * @self: the AstalWpNode instance.
 *
 * gets the icon for this node
 */
const gchar *astal_wp_node_get_icon(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    return priv->icon;
}

gboolean astal_wp_node_get_is_default(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    return priv->is_default;
}

void astal_wp_node_set_is_default(AstalWpNode *self, gboolean is_default) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);

    if (!is_default) return;
    gboolean ret;
    const gchar *name =
        wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node), "node.name");
    const gchar *media_class =
        wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node), "media.class");
    g_signal_emit_by_name(priv->defaults, "set-default-configured-node-name", media_class, name,
                          &ret);
}

gboolean astal_wp_node_get_lock_channels(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    return priv->lock_channels;
}

void astal_wp_node_set_lock_channels(AstalWpNode *self, gboolean lock_channels) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    priv->lock_channels = lock_channels;
    astal_wp_node_set_volume(self, priv->volume);
}

const gchar *astal_wp_node_get_volume_icon(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    if (priv->type == ASTAL_WP_MEDIA_CLASS_AUDIO_MICROPHONE) {
        if (priv->mute) return "microphone-sensitivity-muted-symbolic";
        if (priv->volume <= 0.33) return "microphone-sensitivity-low-symbolic";
        if (priv->volume <= 0.66) return "microphone-sensitivity-medium-symbolic";
        return "microphone-sensitivity-high-symbolic";

    } else {
        if (priv->mute) return "audio-volume-muted-symbolic";
        if (priv->volume <= 0.33) return "audio-volume-low-symbolic";
        if (priv->volume <= 0.66) return "audio-volume-medium-symbolic";
        if (priv->volume <= 1) return "audio-volume-high-symbolic";
        return "audio-volume-overamplified-symbolic";
    }
}

/**
 * astal_wp_node_get_serial:
 * @self: the AstalWpNode instance.
 *
 * gets the serial number of this node
 */
guint astal_wp_node_get_serial(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    return priv->serial;
}

/**
 * astal_wp_node_get_path:
 * @self: the AstalWpNode instance.
 *
 * gets the object path of this node
 */
const gchar *astal_wp_node_get_path(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    return priv->path;
}

/**
 * astal_wp_node_get_channel_volumes:
 * @self: the AstalWpNode instance
 *
 * gets the list representing the per channel volumes
 *
 * Returns: (transfer container) (nullable) (type GList(AstalWpChannelVolume))
 */
GList *astal_wp_node_get_channel_volumes(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    return g_hash_table_get_values(priv->channel_volumes);
}

static void astal_wp_node_get_property(GObject *object, guint property_id, GValue *value,
                                       GParamSpec *pspec) {
    AstalWpNode *self = ASTAL_WP_NODE(object);
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);

    switch (property_id) {
        case ASTAL_WP_NODE_PROP_ID:
            g_value_set_uint(value, priv->id);
            break;
        case ASTAL_WP_NODE_PROP_MUTE:
            g_value_set_boolean(value, priv->mute);
            break;
        case ASTAL_WP_NODE_PROP_VOLUME:
            g_value_set_double(value, priv->volume);
            break;
        case ASTAL_WP_NODE_PROP_DESCRIPTION:
            g_value_set_string(value, priv->description);
            break;
        case ASTAL_WP_NODE_PROP_NAME:
            g_value_set_string(value, priv->name);
            break;
        case ASTAL_WP_NODE_PROP_ICON:
            g_value_set_string(value, priv->icon);
            break;
        case ASTAL_WP_NODE_PROP_VOLUME_ICON:
            g_value_set_string(value, astal_wp_node_get_volume_icon(self));
            break;
        case ASTAL_WP_NODE_PROP_MEDIA_CLASS:
            g_value_set_enum(value, priv->type);
            break;
        case ASTAL_WP_NODE_PROP_DEFAULT:
            g_value_set_boolean(value, priv->is_default);
            break;
        case ASTAL_WP_NODE_PROP_LOCK_CHANNELS:
            g_value_set_boolean(value, priv->lock_channels);
            break;
        case ASTAL_WP_NODE_PROP_SERIAL:
            g_value_set_uint(value, priv->serial);
            break;
        case ASTAL_WP_NODE_PROP_PATH:
            g_value_set_string(value, priv->path);
            break;
        case ASTAL_WP_NODE_PROP_CHANNEL_VOLUMES:
            g_value_set_pointer(value, priv->channel_volumes);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_wp_node_set_property(GObject *object, guint property_id, const GValue *value,
                                       GParamSpec *pspec) {
    AstalWpNode *self = ASTAL_WP_NODE(object);
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
  
    WpPlugin *plugin;
    AstalWpWp *wp;
    WpNode * node;

    switch (property_id) {
        case ASTAL_WP_NODE_PROP_WP:
            wp = g_value_get_object(value);
            if(wp != NULL && ASTAL_WP_IS_WP(wp)) {
              g_clear_object(&priv->wp);
              priv->wp = g_object_ref(wp);
            }
            break;
        case ASTAL_WP_NODE_PROP_DEFAULT_PLUGIN:
            plugin = g_value_get_object(value);
            if(plugin != NULL && WP_IS_PLUGIN(plugin)) {
              g_clear_object(&priv->defaults);
              priv->defaults = g_object_ref(plugin);
            }
            break;
        case ASTAL_WP_NODE_PROP_MIXER_PLUGIN:
            plugin = g_value_get_object(value);
            if(plugin != NULL && WP_IS_PLUGIN(plugin)) {
              g_clear_object(&priv->mixer);
              priv->mixer = g_object_ref(plugin);
            }
            break;
        case ASTAL_WP_NODE_PROP_NODE:
            node = g_value_get_object(value);
            if(node != NULL && WP_IS_NODE(node)) {
              g_clear_object(&priv->node);
              priv->node = g_object_ref(node);
            }
            break;
        case ASTAL_WP_NODE_PROP_IS_DEFAULT_NODE:
            priv->is_default_node = g_value_get_boolean(value);
            break;
        case ASTAL_WP_NODE_PROP_MUTE:
            astal_wp_node_set_mute(self, g_value_get_boolean(value));
            break;
        case ASTAL_WP_NODE_PROP_VOLUME:
            astal_wp_node_set_volume(self, g_value_get_double(value));
            break;
        case ASTAL_WP_NODE_PROP_DEFAULT:
            astal_wp_node_set_is_default(self, g_value_get_boolean(value));
            break;
        case ASTAL_WP_NODE_PROP_ICON:
            g_free(priv->icon);
            priv->icon = g_strdup(g_value_get_string(value));
            break;
        case ASTAL_WP_NODE_PROP_LOCK_CHANNELS:
            astal_wp_node_set_lock_channels(self, g_value_get_boolean(value));
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_wp_node_update_properties(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    if (priv->node == NULL) return;
    priv->id = wp_proxy_get_bound_id(WP_PROXY(priv->node));
    astal_wp_node_update_volume(self);

    const gchar *description =
        wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node), "node.description");
    if (description == NULL) {
        description = wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node), "node.nick");
    }
    if (description == NULL) {
        description = wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node), "node.name");
    }
    g_free(priv->description);
    priv->description = g_strdup(description);

    const gchar *name =
        wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node), "media.name");
    g_free(priv->name);
    priv->name = g_strdup(name);

    const gchar *type =
        wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node), "media.class");
    GEnumClass *enum_class = g_type_class_ref(ASTAL_WP_TYPE_MEDIA_CLASS);
    if (g_enum_get_value_by_nick(enum_class, type) != NULL)
        priv->type = g_enum_get_value_by_nick(enum_class, type)->value;
    g_type_class_unref(enum_class);

    const gchar *icon = NULL;
    switch (priv->type) {
        case ASTAL_WP_MEDIA_CLASS_AUDIO_SPEAKER:
        case ASTAL_WP_MEDIA_CLASS_AUDIO_MICROPHONE:
            const gchar *dev =
                wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node), "device.id");
            if (dev != NULL) {
                guint device_id = g_ascii_strtoull(dev, NULL, 10);
                AstalWpDevice *device = astal_wp_wp_get_device(priv->wp, device_id);
                icon = astal_wp_device_get_icon(device);
            }
            if (icon == NULL) {
                icon = priv->type == ASTAL_WP_MEDIA_CLASS_AUDIO_SPEAKER
                           ? "audio-card-symbolic"
                           : "audio-input-microphone-symbolic";
            }
            break;
        case ASTAL_WP_MEDIA_CLASS_AUDIO_STREAM:
        case ASTAL_WP_MEDIA_CLASS_AUDIO_RECORDER:
            icon =
                wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node), "media.icon-name");
            if (icon == NULL)
                icon = wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node),
                                                       "window.icon-name");
            if (icon == NULL)
                icon = wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node),
                                                       "application.icon-name");
            if (icon == NULL) icon = "application-x-executable-symbolic";
            break;
        default:
            icon = "audio-card-symbolic";
    }

    g_free(priv->icon);
    priv->icon = g_strdup(icon);

    const gchar *serial =
        wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node), "object.serial");
    if (serial != NULL) {
        priv->serial = g_ascii_strtoull(serial, NULL, 10);
    }

    const gchar *path =
        wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node), "object.path");
    g_free(priv->path);
    priv->path = g_strdup(path);

    g_object_notify(G_OBJECT(self), "id");
    g_object_notify(G_OBJECT(self), "description");
    g_object_notify(G_OBJECT(self), "name");
    g_object_notify(G_OBJECT(self), "icon");
    g_object_notify(G_OBJECT(self), "media-class");
    g_object_notify(G_OBJECT(self), "serial");
    g_object_notify(G_OBJECT(self), "path");
}

static void astal_wp_node_default_changed_as_default(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);

    GEnumClass *enum_class = g_type_class_ref(ASTAL_WP_TYPE_MEDIA_CLASS);
    const gchar *media_class = g_enum_get_value(enum_class, priv->media_class)->value_nick;
    guint defaultId;
    g_signal_emit_by_name(priv->defaults, "get-default-node", media_class, &defaultId);
    g_type_class_unref(enum_class);

    if (defaultId != priv->id) {
        if (priv->node != NULL) g_clear_object(&priv->node);
        AstalWpNode *default_node = astal_wp_wp_get_node(priv->wp, defaultId);
        if (default_node != NULL &&
            astal_wp_node_get_media_class(default_node) == priv->media_class) {
            AstalWpNodePrivate *default_node_priv =
                astal_wp_node_get_instance_private(default_node);
            priv->node = g_object_ref(default_node_priv->node);
            astal_wp_node_update_properties(self);
        }
    }
}

static void astal_wp_node_default_changed(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);

    if (priv->is_default_node) {
        astal_wp_node_default_changed_as_default(self);
        return;
    }

    guint defaultId;
    const gchar *media_class =
        wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node), "media.class");
    g_signal_emit_by_name(priv->defaults, "get-default-node", media_class, &defaultId);

    if (priv->is_default && defaultId != priv->id) {
        priv->is_default = FALSE;
        g_object_notify(G_OBJECT(self), "is-default");
    } else if (!priv->is_default && defaultId == priv->id) {
        priv->is_default = TRUE;
        g_object_notify(G_OBJECT(self), "is-default");
    }
}

static void astal_wp_node_mixer_changed(AstalWpNode *self, guint node_id) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    if (priv->id != node_id) return;
    astal_wp_node_update_volume(self);
}

void astal_wp_node_init_as_default(AstalWpNode *self, WpPlugin *mixer, WpPlugin *defaults,
                                   AstalWpMediaClass type) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);

    priv->mixer = g_object_ref(mixer);
    priv->defaults = g_object_ref(defaults);

    priv->media_class = type;
    priv->is_default_node = TRUE;
    priv->is_default = TRUE;

    priv->default_signal_handler_id = g_signal_connect_swapped(
        priv->defaults, "changed", G_CALLBACK(astal_wp_node_default_changed_as_default), self);
    priv->mixer_signal_handler_id = g_signal_connect_swapped(
        priv->mixer, "changed", G_CALLBACK(astal_wp_node_mixer_changed), self);

    astal_wp_node_default_changed_as_default(self);
    astal_wp_node_update_properties(self);
}

// AstalWpNode *astal_wp_node_create(WpNode *node, WpPlugin *mixer, WpPlugin *defaults,
//                                           AstalWpWp *wp) {
//     AstalWpNode *self = g_object_new(ASTAL_WP_TYPE_NODE, NULL);
//     AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
//
//     priv->mixer = g_object_ref(mixer);
//     priv->defaults = g_object_ref(defaults);
//     priv->node = g_object_ref(node);
//     priv->is_default_node = FALSE;
//     priv->wp = g_object_ref(wp);
//
//     priv->default_signal_handler_id = g_signal_connect_swapped(
//         priv->defaults, "changed", G_CALLBACK(astal_wp_node_default_changed), self);
//     priv->mixer_signal_handler_id = g_signal_connect_swapped(
//         priv->mixer, "changed", G_CALLBACK(astal_wp_node_mixer_changed), self);
//
//     astal_wp_node_update_properties(self);
//     astal_wp_node_default_changed(self);
//     return self;
// }

static void astal_wp_node_constructed(GObject *object) {
    AstalWpNode *self = ASTAL_WP_NODE(object);
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);

    if (priv->is_default_node) return;

    priv->default_signal_handler_id = g_signal_connect_swapped(
        priv->defaults, "changed", G_CALLBACK(astal_wp_node_default_changed), self);
    priv->mixer_signal_handler_id = g_signal_connect_swapped(
        priv->mixer, "changed", G_CALLBACK(astal_wp_node_mixer_changed), self);

    astal_wp_node_update_properties(self);
    astal_wp_node_default_changed(self);
}

static void astal_wp_node_init(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    priv->node = NULL;
    priv->mixer = NULL;
    priv->defaults = NULL;
    priv->wp = NULL;

    priv->volume = 0;
    priv->mute = TRUE;
    priv->description = NULL;
    priv->name = NULL;
    priv->path = NULL;

    priv->channel_volumes = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, g_object_unref);
}

static void astal_wp_node_dispose(GObject *object) {
    AstalWpNode *self = ASTAL_WP_NODE(object);
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);

    g_signal_handler_disconnect(priv->defaults, priv->default_signal_handler_id);
    g_signal_handler_disconnect(priv->mixer, priv->mixer_signal_handler_id);

    if (priv->channel_volumes) {
        g_hash_table_destroy(priv->channel_volumes);
        priv->channel_volumes = NULL;
    }

    g_clear_object(&priv->node);
    g_clear_object(&priv->mixer);
    g_clear_object(&priv->defaults);
    g_clear_object(&priv->wp);

    G_OBJECT_CLASS(astal_wp_node_parent_class)->dispose(object);
}

static void astal_wp_node_finalize(GObject *object) {
    AstalWpNode *self = ASTAL_WP_NODE(object);
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    g_free(priv->description);
    g_free(priv->name);
    g_free(priv->path);

    G_OBJECT_CLASS(astal_wp_node_parent_class)->finalize(object);
}

static void astal_wp_node_class_init(AstalWpNodeClass *class) {
    GObjectClass *object_class = G_OBJECT_CLASS(class);
    object_class->dispose = astal_wp_node_dispose;
    object_class->finalize = astal_wp_node_finalize;
    object_class->get_property = astal_wp_node_get_property;
    object_class->set_property = astal_wp_node_set_property;
    object_class->constructed = astal_wp_node_constructed;

    astal_wp_node_properties[ASTAL_WP_NODE_PROP_WP] =
        g_param_spec_object("wp", "wp", "wp", ASTAL_WP_TYPE_WP, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_NODE] =
        g_param_spec_object("node", "node", "node", WP_TYPE_NODE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_MIXER_PLUGIN] = g_param_spec_object(
        "mixer-plugin", "mixer-plugin", "mixer-plugin", WP_TYPE_PLUGIN, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_DEFAULT_PLUGIN] =
        g_param_spec_object("default-plugin", "default-plugin", "default-plugin", WP_TYPE_PLUGIN,
                            G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_IS_DEFAULT_NODE] = g_param_spec_boolean(
        "is-default-node", "is-default-node", "is-default-node", FALSE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);

    /**
     * AstalWpNode:id:
     *
     * The pipewire id of this node.
     */
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_ID] =
        g_param_spec_uint("id", "id", "id", 0, UINT_MAX, 0, G_PARAM_READABLE);
    /**
     * AstalWpNode:volume:
     *
     * The volume of this node
     */
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_VOLUME] =
        g_param_spec_double("volume", "volume", "volume", 0, G_MAXFLOAT, 0, G_PARAM_READWRITE);
    /**
     * AstalWpNode:mute:
     *
     * The mute state of this node
     */
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_MUTE] =
        g_param_spec_boolean("mute", "mute", "mute", TRUE, G_PARAM_READWRITE);
    /**
     * AstalWpNode:description:
     *
     * The description of this node
     */
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_DESCRIPTION] =
        g_param_spec_string("description", "description", "description", NULL, G_PARAM_READABLE);
    /**
     * AstalWpNode:name:
     *
     * The name of this node
     */
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_NAME] =
        g_param_spec_string("name", "name", "name", NULL, G_PARAM_READABLE);
    /**
     * AstalWpNode:icon:
     *
     * The icon of this node. Note that nodes do not have icons associated with them in
     * pipewire, so the icon of the associated device is used instead.
     */
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_ICON] = g_param_spec_string(
        "icon", "icon", "icon", "audio-card-symbolic", G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    /**
     * AstalWpNode:volume-icon:
     *
     * The volume icon of this node
     */
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_VOLUME_ICON] = g_param_spec_string(
        "volume-icon", "volume-icon", "volume-icon", "audio-volume-muted", G_PARAM_READABLE);
    /**
     * AstalWpNode:media-class: (type AstalWpMediaClass)
     *
     * The media class of this node
     */
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_MEDIA_CLASS] =
        g_param_spec_enum("media-class", "media-class", "media-class", ASTAL_WP_TYPE_MEDIA_CLASS, 1,
                          G_PARAM_READABLE);
    /**
     * AstalWpNode:is-default:
     *
     * Whether this node is the default one used for this media-class. Note that setting this
     * property to false has no effect.
     */
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_DEFAULT] =
        g_param_spec_boolean("is-default", "is-default", "is-default", FALSE, G_PARAM_READWRITE);
    /**
     * AstalWpNode:lock-channels:
     *
     * Whether to lock the channels together or not.
     */
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_LOCK_CHANNELS] = g_param_spec_boolean(
        "lock-channels", "lock-channels", "lock channels", FALSE, G_PARAM_READWRITE);

    /**
     * AstalWpNode:serial:
     *
     * The object serial of this node.
     */
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_SERIAL] =
        g_param_spec_uint("serial", "serial", "serial", 0, UINT_MAX, 0, G_PARAM_READABLE);

    /**
     * AstalWpNode:path:
     *
     * The object path of this node
     */
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_PATH] =
        g_param_spec_string("path", "path", "path", NULL, G_PARAM_READABLE);

    /**
     * AstalWpNode:channel-volumes: (type GList(AstalWpChannelVolume)) (transfer container)
     * (nullable)
     *
     * A list of per channel volumes
     */
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_CHANNEL_VOLUMES] = g_param_spec_pointer(
        "channel-volumes", "channel-volumes", "per channel volume", G_PARAM_READABLE);

    g_object_class_install_properties(object_class, ASTAL_WP_NODE_N_PROPERTIES,
                                      astal_wp_node_properties);
}
