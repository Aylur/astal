#include "node.h"

#include <wp/wp.h>

#include "astal-wp-enum-types.h"
#include "channel-private.h"
#include "channel.h"
#include "enums.h"
#include "glib-object.h"
#include "glib.h"
#include "node-private.h"
#include "wp.h"

typedef struct {
    WpNode *node;
    WpPlugin *mixer;
    AstalWpWp *wp;

    AstalWpMediaClass media_class;

    gulong mixer_signal_handler_id;
    gulong params_signal_handler_id;
    gulong properties_signal_handler_id;
    gulong state_change_handler_id;

    guint id;
    gdouble volume;
    gboolean mute;

    gchar *description;
    gchar *nick;
    gchar *node_name;

    gchar *name;
    gint serial;
    gchar *path;

    AstalWpNodeState state;
    AstalWpMediaClass type;
    gboolean lock_channels;
    gchar *icon;
    GHashTable *channels;

} AstalWpNodePrivate;

G_DEFINE_TYPE_WITH_PRIVATE(AstalWpNode, astal_wp_node, G_TYPE_OBJECT);

typedef enum {
    // private props
    ASTAL_WP_NODE_PROP_WP = 1,
    ASTAL_WP_NODE_PROP_NODE,
    ASTAL_WP_NODE_PROP_MIXER_PLUGIN,
    // public props
    ASTAL_WP_NODE_PROP_ID,
    ASTAL_WP_NODE_PROP_VOLUME,
    ASTAL_WP_NODE_PROP_MUTE,
    ASTAL_WP_NODE_PROP_DESCRIPTION,
    ASTAL_WP_NODE_PROP_NAME,
    ASTAL_WP_NODE_PROP_MEDIA_CLASS,
    ASTAL_WP_NODE_PROP_ICON,
    ASTAL_WP_NODE_PROP_VOLUME_ICON,
    ASTAL_WP_NODE_PROP_LOCK_CHANNELS,
    ASTAL_WP_NODE_PROP_SERIAL,
    ASTAL_WP_NODE_PROP_PATH,
    ASTAL_WP_NODE_PROP_STATE,
    ASTAL_WP_NODE_PROP_CHANNELS,
    ASTAL_WP_NODE_N_PROPERTIES,
} AstalWpNodeProperties;

static GParamSpec *astal_wp_node_properties[ASTAL_WP_NODE_N_PROPERTIES] = {
    NULL,
};

static void astal_wp_node_pw_properties_changed(AstalWpNode *self);

void astal_wp_node_update_volume(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);

    if (priv->mixer == NULL) return;

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

            if(channel_str == NULL) continue;
            if (channel_volume > volume) volume = channel_volume;

            AstalWpChannel *cv = g_hash_table_lookup(priv->channels, channel_str);
            if (cv == NULL) {
                cv = astal_wp_channel_new(self, channel_str);
                g_hash_table_insert(priv->channels, g_strdup(channel_str), cv);
                g_object_notify(G_OBJECT(self), "channels");
            }
            astal_wp_channel_update_volume(cv, channel_volume);
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

static void astal_wp_node_mixer_changed(AstalWpNode *self, guint node_id) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    if (priv->id != node_id) return;
    astal_wp_node_update_volume(self);
}

void astal_wp_node_set_channel_volume(AstalWpNode *self, const gchar *name, gdouble volume) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);

    if (priv->lock_channels) {
        astal_wp_node_set_volume(self, volume);
        return;
    }

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
 * astal_wp_node_get_state:
 * @self: The AstalWpNode instance.
 *
 * gets the current state of this node
 */
AstalWpNodeState astal_wp_node_get_state(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    return priv->state;
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
    g_return_val_if_fail(self != NULL, NULL);
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    if (priv->description != NULL) return priv->description;
    if (priv->nick != NULL) return priv->nick;
    if (priv->node_name != NULL) return priv->node_name;
    return NULL;
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

void astal_wp_node_set_icon(AstalWpNode *self, const gchar *icon) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    if (!g_strcmp0(priv->icon, icon)) return;
    g_free(priv->icon);
    priv->icon = g_strdup(icon);
    g_object_notify(G_OBJECT(self), "icon");
}

gboolean astal_wp_node_get_lock_channels(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    return priv->lock_channels;
}

/**
 * astal_wp_node_set_lock_channels:
 *
 * Lock the channel volumes together. If set, all channels will always have the same volume.
 */
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
gint astal_wp_node_get_serial(AstalWpNode *self) {
    g_return_val_if_fail(self != NULL, -1);
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
 * astal_wp_node_get_channels:
 * @self: the AstalWpNode instance
 *
 * gets the list representing the per channel volumes
 *
 * Returns: (transfer container) (nullable) (type GList(AstalWpChannel))
 */
GList *astal_wp_node_get_channels(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    return g_hash_table_get_values(priv->channels);
}

static void astal_wp_node_state_changed(AstalWpNode *self, WpNodeState old_state,
                                        WpNodeState new_state) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    priv->state = (AstalWpNodeState)new_state;
    g_object_notify(G_OBJECT(self), "state");
}

void astal_wp_node_set_node(AstalWpNode *self, WpNode *node) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    if (node != NULL && WP_IS_NODE(node)) {
        if (priv->node != NULL) {
            g_signal_handler_disconnect(priv->node, priv->params_signal_handler_id);
            g_signal_handler_disconnect(priv->node, priv->properties_signal_handler_id);
            g_signal_handler_disconnect(priv->node, priv->state_change_handler_id);
        }
        g_clear_object(&priv->node);
        priv->node = g_object_ref(node);
        priv->params_signal_handler_id = g_signal_connect_swapped(
            priv->node, "params-changed", G_CALLBACK(astal_wp_node_params_changed), self);
        priv->properties_signal_handler_id =
            g_signal_connect_swapped(priv->node, "notify::properties",
                                     G_CALLBACK(astal_wp_node_pw_properties_changed), self);
        priv->state_change_handler_id = g_signal_connect_swapped(
            priv->node, "state-changed", G_CALLBACK(astal_wp_node_state_changed), self);
    }
    astal_wp_node_params_changed(self, "Props");
    astal_wp_node_update_volume(self);
}

void astal_wp_node_set_mixer(AstalWpNode *self, WpPlugin *mixer) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    if (mixer != NULL && WP_IS_PLUGIN(mixer)) {
        if (priv->mixer != NULL)
            g_signal_handler_disconnect(priv->mixer, priv->mixer_signal_handler_id);
        g_clear_object(&priv->mixer);
        priv->mixer = g_object_ref(mixer);
        priv->mixer_signal_handler_id = g_signal_connect_swapped(
            priv->mixer, "changed", G_CALLBACK(astal_wp_node_mixer_changed), self);
        astal_wp_node_update_volume(self);
    }
}

void astal_wp_node_set_type(AstalWpNode *self, AstalWpMediaClass type) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    priv->type = type;
}

/**
 * astal_wp_node_get_pw_property
 *
 * Gets the pipewire property with the give key. You should use the GObject properties of this node
 * whereever possible, as you can get notified on changes, which is not the case here.
 *
 * Returns: (transfer full)
 */
gchar *astal_wp_node_get_pw_property(AstalWpNode *self, const gchar *key) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    const gchar *value = wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node), key);
    return g_strdup(value);
}

static void astal_wp_node_get_property(GObject *object, guint property_id, GValue *value,
                                       GParamSpec *pspec) {
    AstalWpNode *self = ASTAL_WP_NODE(object);
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);

    switch (property_id) {
        case ASTAL_WP_NODE_PROP_NODE:
            g_value_set_object(value, priv->node);
            break;
        case ASTAL_WP_NODE_PROP_WP:
            g_value_set_object(value, priv->wp);
            break;
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
            g_value_set_string(value, astal_wp_node_get_description(self));
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
        case ASTAL_WP_NODE_PROP_LOCK_CHANNELS:
            g_value_set_boolean(value, priv->lock_channels);
            break;
        case ASTAL_WP_NODE_PROP_SERIAL:
            g_value_set_int(value, priv->serial);
            break;
        case ASTAL_WP_NODE_PROP_PATH:
            g_value_set_string(value, priv->path);
            break;
        case ASTAL_WP_NODE_PROP_CHANNELS:
            g_value_set_pointer(value, astal_wp_node_get_channels(self));
            break;
        case ASTAL_WP_NODE_PROP_STATE:
            g_value_set_enum(value, priv->state);
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
    WpNode *node;

    switch (property_id) {
        case ASTAL_WP_NODE_PROP_WP:
            wp = g_value_get_object(value);
            if (wp != NULL && ASTAL_WP_IS_WP(wp)) {
                g_clear_object(&priv->wp);
                priv->wp = g_object_ref(wp);
            }
            break;
        case ASTAL_WP_NODE_PROP_MIXER_PLUGIN:
            plugin = g_value_get_object(value);
            astal_wp_node_set_mixer(self, plugin);
            break;
        case ASTAL_WP_NODE_PROP_NODE:
            node = g_value_get_object(value);
            astal_wp_node_set_node(self, node);
            break;
        case ASTAL_WP_NODE_PROP_MUTE:
            astal_wp_node_set_mute(self, g_value_get_boolean(value));
            break;
        case ASTAL_WP_NODE_PROP_VOLUME:
            astal_wp_node_set_volume(self, g_value_get_double(value));
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

static void astal_wp_node_real_metadata_changed(AstalWpNode *self, const gchar *key,
                                                const gchar *type, const gchar *value) {}

void astal_wp_node_metadata_changed(AstalWpNode *self, const gchar *key, const gchar *type,
                                    const gchar *value) {
    AstalWpNodeClass *klass = ASTAL_WP_NODE_GET_CLASS(self);
    (*klass->metadata_changed)(self, key, type, value);
}

void astal_wp_node_properties_changed(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);

    if (priv->node == NULL) return;

    WpPipewireObject *pwo = WP_PIPEWIRE_OBJECT(priv->node);

    const gchar *value;

    value = wp_pipewire_object_get_property(pwo, "object.id");
    guint id = g_ascii_strtoull(value, NULL, 10);
    if (priv->id != id) {
        priv->id = id;
        g_object_notify(G_OBJECT(self), "id");
    }

    value = wp_pipewire_object_get_property(pwo, "node.description");
    if (g_strcmp0(priv->description, value)) {
        g_free(priv->description);
        priv->description = g_strdup(value);
        g_object_notify(G_OBJECT(self), "description");
    }

    value = wp_pipewire_object_get_property(pwo, "node.name");
    if (g_strcmp0(priv->node_name, value)) {
        g_free(priv->node_name);
        priv->node_name = g_strdup(value);
        g_object_notify(G_OBJECT(self), "description");
    }

    value = wp_pipewire_object_get_property(pwo, "node.nick");
    if (g_strcmp0(priv->nick, value)) {
        g_free(priv->nick);
        priv->nick = g_strdup(value);
        g_object_notify(G_OBJECT(self), "description");
    }

    value = wp_pipewire_object_get_property(pwo, "media.name");
    if (g_strcmp0(priv->name, value)) {
        g_free(priv->name);
        priv->name = g_strdup(value);
        g_object_notify(G_OBJECT(self), "name");
    }

    value = wp_pipewire_object_get_property(pwo, "media.class");
    AstalWpMediaClass media_class = astal_wp_media_class_from_string(value);
    if (media_class != priv->type) {
        priv->type = media_class;
        g_object_notify(G_OBJECT(self), "media-class");
    }

    value = wp_pipewire_object_get_property(pwo, "object.serial");
    gint serial = g_ascii_strtoll(value, NULL, 10);
    if (priv->serial != serial) {
        priv->serial = serial;
        g_object_notify(G_OBJECT(self), "serial");
    }

    value = wp_pipewire_object_get_property(pwo, "object.path");
    if (g_strcmp0(priv->path, value)) {
        g_free(priv->path);
        priv->path = g_strdup(value);
        g_object_notify(G_OBJECT(self), "path");
    }
}

static void astal_wp_node_real_params_changed(AstalWpNode *self, const gchar *id) {
    g_object_freeze_notify(G_OBJECT(self));

    if (!g_strcmp0(id, "Props")) astal_wp_node_properties_changed(self);

    g_object_thaw_notify(G_OBJECT(self));
}

void astal_wp_node_params_changed(AstalWpNode *self, const gchar *id) {
    AstalWpNodeClass *klass = ASTAL_WP_NODE_GET_CLASS(self);
    (*klass->params_changed)(self, id);
}

static void astal_wp_node_pw_properties_changed(AstalWpNode *self) {
    astal_wp_node_params_changed(self, "Props");
}

static void astal_wp_node_init(AstalWpNode *self) {
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    priv->node = NULL;
    priv->mixer = NULL;
    priv->wp = NULL;

    priv->volume = 0;
    priv->mute = TRUE;
    priv->description = NULL;
    priv->nick = NULL;
    priv->node_name = NULL;
    priv->name = NULL;
    priv->path = NULL;

    priv->channels = g_hash_table_new_full(g_str_hash, g_str_equal, g_free, g_object_unref);
}

static void astal_wp_node_constructed(GObject *obj) {
    AstalWpNode *self = ASTAL_WP_NODE(obj);
    astal_wp_node_params_changed(self, "Props");
}

static void astal_wp_node_dispose(GObject *object) {
    AstalWpNode *self = ASTAL_WP_NODE(object);
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);

    if (priv->mixer != NULL)
        g_signal_handler_disconnect(priv->mixer, priv->mixer_signal_handler_id);
    if (priv->node != NULL) g_signal_handler_disconnect(priv->node, priv->state_change_handler_id);

    if (priv->channels) {
        g_hash_table_destroy(priv->channels);
        priv->channels = NULL;
    }

    g_clear_object(&priv->node);
    g_clear_object(&priv->mixer);
    g_clear_object(&priv->wp);

    G_OBJECT_CLASS(astal_wp_node_parent_class)->dispose(object);
}

static void astal_wp_node_finalize(GObject *object) {
    AstalWpNode *self = ASTAL_WP_NODE(object);
    AstalWpNodePrivate *priv = astal_wp_node_get_instance_private(self);
    g_free(priv->description);
    g_free(priv->nick);
    g_free(priv->node_name);
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

    class->metadata_changed = astal_wp_node_real_metadata_changed;
    class->params_changed = astal_wp_node_real_params_changed;

    /**
     * AstalWpNode:wp: (skip)
     */
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_WP] = g_param_spec_object(
        "wp", "wp", "wp", ASTAL_WP_TYPE_WP, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    /**
     * AstalWpNode:node: (skip)
     */
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_NODE] = g_param_spec_object(
        "node", "node", "node", WP_TYPE_NODE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    /**
     * AstalWpNode:mixer-plugin: (skip)
     */
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_MIXER_PLUGIN] =
        g_param_spec_object("mixer-plugin", "mixer-plugin", "mixer-plugin", WP_TYPE_PLUGIN,
                            G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
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
        g_param_spec_double("volume", "volume", "volume", 0, G_MAXFLOAT, 0,
                            G_PARAM_READWRITE | G_PARAM_EXPLICIT_NOTIFY);
    /**
     * AstalWpNode:mute:
     *
     * The mute state of this node
     */
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_MUTE] = g_param_spec_boolean(
        "mute", "mute", "mute", TRUE, G_PARAM_READWRITE | G_PARAM_EXPLICIT_NOTIFY);
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
        g_param_spec_int("serial", "serial", "serial", INT_MIN, INT_MAX, 0, G_PARAM_READABLE);

    /**
     * AstalWpNode:path:
     *
     * The object path of this node
     */
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_PATH] =
        g_param_spec_string("path", "path", "path", NULL, G_PARAM_READABLE);

    /**
     * AstalWpNode:channels: (type GList(AstalWpChannel)) (transfer container)
     *
     * A list of per channel volumes
     */
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_CHANNELS] =
        g_param_spec_pointer("channels", "channels", "a list of channels", G_PARAM_READABLE);

    /**
     * AstalWpNode:state: (type AstalWpNodeState)
     *
     * the current state of this node.
     */
    astal_wp_node_properties[ASTAL_WP_NODE_PROP_STATE] =
        g_param_spec_enum("state", "state", "state", ASTAL_WP_TYPE_NODE_STATE, 0, G_PARAM_READABLE);

    g_object_class_install_properties(object_class, ASTAL_WP_NODE_N_PROPERTIES,
                                      astal_wp_node_properties);
}
