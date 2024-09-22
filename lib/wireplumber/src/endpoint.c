#include "endpoint.h"

#include <wp/wp.h>

#include "device.h"
#include "endpoint-private.h"
#include "glib.h"
#include "wp.h"

struct _AstalWpEndpoint {
    GObject parent_instance;

    guint id;
    gdouble volume;
    gboolean mute;
    gchar *description;
    gchar *name;
    AstalWpMediaClass type;
    gboolean is_default;
    gboolean lock_channels;

    gchar *icon;
};

typedef struct {
    WpNode *node;
    WpPlugin *mixer;
    WpPlugin *defaults;
    AstalWpWp *wp;

    gboolean is_default_node;
    AstalWpMediaClass media_class;

    gulong default_signal_handler_id;
    gulong mixer_signal_handler_id;

} AstalWpEndpointPrivate;

G_DEFINE_FINAL_TYPE_WITH_PRIVATE(AstalWpEndpoint, astal_wp_endpoint, G_TYPE_OBJECT);

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
    ASTAL_WP_ENDPOINT_PROP_ID = 1,
    ASTAL_WP_ENDPOINT_PROP_VOLUME,
    ASTAL_WP_ENDPOINT_PROP_MUTE,
    ASTAL_WP_ENDPOINT_PROP_DESCRIPTION,
    ASTAL_WP_ENDPOINT_PROP_NAME,
    ASTAL_WP_ENDPOINT_PROP_MEDIA_CLASS,
    ASTAL_WP_ENDPOINT_PROP_DEFAULT,
    ASTAL_WP_ENDPOINT_PROP_ICON,
    ASTAL_WP_ENDPOINT_PROP_VOLUME_ICON,
    ASTAL_WP_ENDPOINT_PROP_LOCK_CHANNELS,
    ASTAL_WP_ENDPOINT_N_PROPERTIES,
} AstalWpEndpointProperties;

static GParamSpec *astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_N_PROPERTIES] = {
    NULL,
};

void astal_wp_endpoint_update_volume(AstalWpEndpoint *self) {
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);

    gdouble volume = 0;
    gboolean mute;
    GVariant *variant = NULL;
    GVariantIter *channels = NULL;

    g_signal_emit_by_name(priv->mixer, "get-volume", self->id, &variant);

    if (variant == NULL) return;

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
        }
    }

    if (mute != self->mute) {
        self->mute = mute;
        g_object_notify(G_OBJECT(self), "mute");
    }

    if (volume != self->volume) {
        self->volume = volume;
        g_object_notify(G_OBJECT(self), "volume");
    }

    g_object_notify(G_OBJECT(self), "volume-icon");
}

/**
 * astal_wp_endpoint_set_volume:
 * @self: the AstalWpEndpoint object
 * @volume: The new volume level to set.
 *
 * Sets the volume level for this endpoint. The volume is clamped to be between
 * 0 and 1.5.
 */
void astal_wp_endpoint_set_volume(AstalWpEndpoint *self, gdouble volume) {
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);

    gboolean ret;
    if (volume >= 1.5) volume = 1.5;
    if (volume <= 0) volume = 0;

    gboolean mute;
    GVariant *variant = NULL;
    GVariantIter *channels = NULL;

    g_auto(GVariantBuilder) vol_b = G_VARIANT_BUILDER_INIT(G_VARIANT_TYPE_VARDICT);
    g_signal_emit_by_name(priv->mixer, "get-volume", self->id, &variant);

    if (variant == NULL) return;

    g_variant_lookup(variant, "mute", "b", &mute);
    g_variant_lookup(variant, "channelVolumes", "a{sv}", &channels);

    if (channels != NULL && !self->lock_channels) {
        g_auto(GVariantBuilder) channel_volumes_b = G_VARIANT_BUILDER_INIT(G_VARIANT_TYPE_VARDICT);

        const gchar *key;
        const gchar *channel_str;
        gdouble channel_volume;
        GVariant *varvol;

        while (g_variant_iter_loop(channels, "{&sv}", &key, &varvol)) {
            g_auto(GVariantBuilder) channel_b = G_VARIANT_BUILDER_INIT(G_VARIANT_TYPE_VARDICT);
            g_variant_lookup(varvol, "volume", "d", &channel_volume);
            g_variant_lookup(varvol, "channel", "&s", &channel_str);
            gdouble vol = self->volume == 0 ? volume : channel_volume * volume / self->volume;
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

    g_signal_emit_by_name(priv->mixer, "set-volume", self->id, g_variant_builder_end(&vol_b), &ret);
}

/**
 * astal_wp_endpoint_set_mute:
 * @self: the AstalWpEndpoint instance.
 * @mute: A boolean indicating whether to mute the endpoint.
 *
 * Sets the mute status for the endpoint.
 */
void astal_wp_endpoint_set_mute(AstalWpEndpoint *self, gboolean mute) {
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);

    gboolean ret;
    GVariant *variant = NULL;
    GVariantBuilder b = G_VARIANT_BUILDER_INIT(G_VARIANT_TYPE_VARDICT);
    g_variant_builder_add(&b, "{sv}", "mute", g_variant_new_boolean(mute));
    variant = g_variant_builder_end(&b);

    g_signal_emit_by_name(priv->mixer, "set-volume", self->id, variant, &ret);
}

/**
 * astal_wp_endpoint_get_media_class:
 * @self: the AstalWpEndpoint instance.
 *
 * gets the media class of the endpoint.
 */
AstalWpMediaClass astal_wp_endpoint_get_media_class(AstalWpEndpoint *self) { return self->type; }

/**
 * astal_wp_endpoint_get_id:
 * @self: the AstalWpEndpoint instance.
 *
 * gets the id of the endpoint.
 */
guint astal_wp_endpoint_get_id(AstalWpEndpoint *self) { return self->id; }

/**
 * astal_wp_endpoint_get_mute:
 * @self: the AstalWpEndpoint instance.
 *
 * gets the mute status of the endpoint.
 */
gboolean astal_wp_endpoint_get_mute(AstalWpEndpoint *self) { return self->mute; }

/**
 * astal_wp_endpoint_get_volume:
 * @self: the AstalWpEndpoint instance.
 *
 * gets the volume
 */
gdouble astal_wp_endpoint_get_volume(AstalWpEndpoint *self) { return self->volume; }

/**
 * astal_wp_endpoint_get_description:
 * @self: the AstalWpEndpoint instance.
 *
 * gets the description of this endpoint
 */
const gchar *astal_wp_endpoint_get_description(AstalWpEndpoint *self) { return self->description; }

/**
 * astal_wp_endpoint_get_name:
 * @self: the AstalWpEndpoint instance.
 *
 * gets the name of this endpoint
 */
const gchar *astal_wp_endpoint_get_name(AstalWpEndpoint *self) { return self->name; }

/**
 * astal_wp_endpoint_get_icon:
 * @self: the AstalWpEndpoint instance.
 *
 * gets the icon for this endpoint
 */
const gchar *astal_wp_endpoint_get_icon(AstalWpEndpoint *self) { return self->icon; }

gboolean astal_wp_endpoint_get_is_default(AstalWpEndpoint *self) { return self->is_default; }

void astal_wp_endpoint_set_is_default(AstalWpEndpoint *self, gboolean is_default) {
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);

    if (!is_default) return;
    gboolean ret;
    const gchar *name =
        wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node), "node.name");
    const gchar *media_class =
        wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node), "media.class");
    g_signal_emit_by_name(priv->defaults, "set-default-configured-node-name", media_class, name,
                          &ret);
}

gboolean astal_wp_endpoint_get_lock_channels(AstalWpEndpoint *self) { return self->lock_channels; }

void astal_wp_endpoint_set_lock_channels(AstalWpEndpoint *self, gboolean lock_channels) {
    self->lock_channels = lock_channels;
    astal_wp_endpoint_set_volume(self, self->volume);
}

const gchar *astal_wp_endpoint_get_volume_icon(AstalWpEndpoint *self) {
    if (self->type == ASTAL_WP_MEDIA_CLASS_AUDIO_MICROPHONE) {
        if (self->mute) return "microphone-sensitivity-muted-symbolic";
        if (self->volume <= 0.33) return "microphone-sensitivity-low-symbolic";
        if (self->volume <= 0.66) return "microphone-sensitivity-medium-symbolic";
        return "microphone-sensitivity-high-symbolic";

    } else {
        if (self->mute) return "audio-volume-muted-symbolic";
        if (self->volume <= 0.33) return "audio-volume-low-symbolic";
        if (self->volume <= 0.66) return "audio-volume-medium-symbolic";
        if (self->volume <= 1) return "audio-volume-high-symbolic";
        return "audio-volume-overamplified-symbolic";
    }
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
        case ASTAL_WP_ENDPOINT_PROP_NAME:
            g_value_set_string(value, self->name);
            break;
        case ASTAL_WP_ENDPOINT_PROP_ICON:
            g_value_set_string(value, self->icon);
            break;
        case ASTAL_WP_ENDPOINT_PROP_VOLUME_ICON:
            g_value_set_string(value, astal_wp_endpoint_get_volume_icon(self));
            break;
        case ASTAL_WP_ENDPOINT_PROP_MEDIA_CLASS:
            g_value_set_enum(value, self->type);
            break;
        case ASTAL_WP_ENDPOINT_PROP_DEFAULT:
            g_value_set_boolean(value, self->is_default);
            break;
        case ASTAL_WP_ENDPOINT_PROP_LOCK_CHANNELS:
            g_value_set_boolean(value, self->lock_channels);
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
        case ASTAL_WP_ENDPOINT_PROP_DEFAULT:
            astal_wp_endpoint_set_is_default(self, g_value_get_boolean(value));
            break;
        case ASTAL_WP_ENDPOINT_PROP_ICON:
            g_free(self->icon);
            self->icon = g_strdup(g_value_get_string(value));
            break;
        case ASTAL_WP_ENDPOINT_PROP_LOCK_CHANNELS:
            astal_wp_endpoint_set_lock_channels(self, g_value_get_boolean(value));
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_wp_endpoint_update_properties(AstalWpEndpoint *self) {
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);
    if (priv->node == NULL) return;
    self->id = wp_proxy_get_bound_id(WP_PROXY(priv->node));
    astal_wp_endpoint_update_volume(self);

    const gchar *description =
        wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node), "node.description");
    if (description == NULL) {
        description = wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node), "node.nick");
    }
    if (description == NULL) {
        description = wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node), "node.name");
    }
    g_free(self->description);
    self->description = g_strdup(description);

    const gchar *name =
        wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node), "media.name");
    g_free(self->name);
    self->name = g_strdup(name);

    const gchar *type =
        wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node), "media.class");
    GEnumClass *enum_class = g_type_class_ref(ASTAL_WP_TYPE_MEDIA_CLASS);
    if (g_enum_get_value_by_nick(enum_class, type) != NULL)
        self->type = g_enum_get_value_by_nick(enum_class, type)->value;
    g_type_class_unref(enum_class);

    const gchar *icon = NULL;
    switch (self->type) {
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
                icon = self->type == ASTAL_WP_MEDIA_CLASS_AUDIO_SPEAKER
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
    g_free(self->icon);
    self->icon = g_strdup(icon);

    g_object_notify(G_OBJECT(self), "id");
    g_object_notify(G_OBJECT(self), "description");
    g_object_notify(G_OBJECT(self), "name");
    g_object_notify(G_OBJECT(self), "icon");
    g_object_notify(G_OBJECT(self), "media-class");
}

static void astal_wp_endpoint_default_changed_as_default(AstalWpEndpoint *self) {
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);

    GEnumClass *enum_class = g_type_class_ref(ASTAL_WP_TYPE_MEDIA_CLASS);
    const gchar *media_class = g_enum_get_value(enum_class, priv->media_class)->value_nick;
    guint defaultId;
    g_signal_emit_by_name(priv->defaults, "get-default-node", media_class, &defaultId);
    g_type_class_unref(enum_class);

    if (defaultId != self->id) {
        if (priv->node != NULL) g_clear_object(&priv->node);
        AstalWpEndpoint *default_endpoint = astal_wp_wp_get_endpoint(priv->wp, defaultId);
        if (default_endpoint != NULL &&
            astal_wp_endpoint_get_media_class(default_endpoint) == priv->media_class) {
            AstalWpEndpointPrivate *default_endpoint_priv =
                astal_wp_endpoint_get_instance_private(default_endpoint);
            priv->node = g_object_ref(default_endpoint_priv->node);
            astal_wp_endpoint_update_properties(self);
        }
    }
}

static void astal_wp_endpoint_default_changed(AstalWpEndpoint *self) {
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);

    guint defaultId;
    const gchar *media_class =
        wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->node), "media.class");
    g_signal_emit_by_name(priv->defaults, "get-default-node", media_class, &defaultId);

    if (self->is_default && defaultId != self->id) {
        self->is_default = FALSE;
        g_object_notify(G_OBJECT(self), "is-default");
    } else if (!self->is_default && defaultId == self->id) {
        self->is_default = TRUE;
        g_object_notify(G_OBJECT(self), "is-default");
    }
}

static void astal_wp_endpoint_mixer_changed(AstalWpEndpoint *self, guint node_id) {
    if (self->id != node_id) return;
    astal_wp_endpoint_update_volume(self);
}

AstalWpEndpoint *astal_wp_endpoint_init_as_default(AstalWpEndpoint *self, WpPlugin *mixer,
                                                   WpPlugin *defaults, AstalWpMediaClass type,
                                                   AstalWpWp *wp) {
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);

    priv->mixer = g_object_ref(mixer);
    priv->defaults = g_object_ref(defaults);

    priv->media_class = type;
    priv->is_default_node = TRUE;
    self->is_default = TRUE;
    priv->wp = g_object_ref(wp);

    priv->default_signal_handler_id = g_signal_connect_swapped(
        priv->defaults, "changed", G_CALLBACK(astal_wp_endpoint_default_changed_as_default), self);
    priv->mixer_signal_handler_id = g_signal_connect_swapped(
        priv->mixer, "changed", G_CALLBACK(astal_wp_endpoint_mixer_changed), self);

    astal_wp_endpoint_default_changed_as_default(self);
    astal_wp_endpoint_update_properties(self);
    return self;
}

AstalWpEndpoint *astal_wp_endpoint_create(WpNode *node, WpPlugin *mixer, WpPlugin *defaults,
                                          AstalWpWp *wp) {
    AstalWpEndpoint *self = g_object_new(ASTAL_WP_TYPE_ENDPOINT, NULL);
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);

    priv->mixer = g_object_ref(mixer);
    priv->defaults = g_object_ref(defaults);
    priv->node = g_object_ref(node);
    priv->is_default_node = FALSE;
    priv->wp = g_object_ref(wp);

    priv->default_signal_handler_id = g_signal_connect_swapped(
        priv->defaults, "changed", G_CALLBACK(astal_wp_endpoint_default_changed), self);
    priv->mixer_signal_handler_id = g_signal_connect_swapped(
        priv->mixer, "changed", G_CALLBACK(astal_wp_endpoint_mixer_changed), self);

    astal_wp_endpoint_update_properties(self);
    astal_wp_endpoint_default_changed(self);
    return self;
}

static void astal_wp_endpoint_init(AstalWpEndpoint *self) {
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);
    priv->node = NULL;
    priv->mixer = NULL;
    priv->defaults = NULL;
    priv->wp = NULL;

    self->volume = 0;
    self->mute = TRUE;
    self->description = NULL;
    self->name = NULL;
}

static void astal_wp_endpoint_dispose(GObject *object) {
    AstalWpEndpoint *self = ASTAL_WP_ENDPOINT(object);
    AstalWpEndpointPrivate *priv = astal_wp_endpoint_get_instance_private(self);

    g_signal_handler_disconnect(priv->defaults, priv->default_signal_handler_id);
    g_signal_handler_disconnect(priv->mixer, priv->mixer_signal_handler_id);

    g_clear_object(&priv->node);
    g_clear_object(&priv->mixer);
    g_clear_object(&priv->defaults);
    g_clear_object(&priv->wp);
}

static void astal_wp_endpoint_finalize(GObject *object) {
    AstalWpEndpoint *self = ASTAL_WP_ENDPOINT(object);
    g_free(self->description);
    g_free(self->name);
}

static void astal_wp_endpoint_class_init(AstalWpEndpointClass *class) {
    GObjectClass *object_class = G_OBJECT_CLASS(class);
    object_class->dispose = astal_wp_endpoint_dispose;
    object_class->finalize = astal_wp_endpoint_finalize;
    object_class->get_property = astal_wp_endpoint_get_property;
    object_class->set_property = astal_wp_endpoint_set_property;
    /**
     * AstalWpEndpoint:id:
     *
     * The pipewire id of this endpoint.
     */
    astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_PROP_ID] =
        g_param_spec_uint("id", "id", "id", 0, UINT_MAX, 0, G_PARAM_READABLE);
    /**
     * AstalWpEndpoint:volume:
     *
     * The volume of this endpoint
     */
    astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_PROP_VOLUME] =
        g_param_spec_double("volume", "volume", "volume", 0, G_MAXFLOAT, 0, G_PARAM_READWRITE);
    /**
     * AstalWpEndpoint:mute:
     *
     * The mute state of this endpoint
     */
    astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_PROP_MUTE] =
        g_param_spec_boolean("mute", "mute", "mute", TRUE, G_PARAM_READWRITE);
    /**
     * AstalWpEndpoint:description:
     *
     * The description of this endpoint
     */
    astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_PROP_DESCRIPTION] =
        g_param_spec_string("description", "description", "description", NULL, G_PARAM_READABLE);
    /**
     * AstalWpEndpoint:name:
     *
     * The name of this endpoint
     */
    astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_PROP_NAME] =
        g_param_spec_string("name", "name", "name", NULL, G_PARAM_READABLE);
    /**
     * AstalWpEndpoint:icon:
     *
     * The icon of this endpoint. Note that endpoints do not have icons associated with them in
     * pipewire, so the icon of the associated device is used instead.
     */
    astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_PROP_ICON] = g_param_spec_string(
        "icon", "icon", "icon", "audio-card-symbolic", G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    /**
     * AstalWpEndpoint:volume-icon:
     *
     * The volume icon of this endpoint
     */
    astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_PROP_VOLUME_ICON] = g_param_spec_string(
        "volume-icon", "volume-icon", "volume-icon", "audio-volume-muted", G_PARAM_READABLE);
    /**
     * AstalWpEndpoint:media-class: (type AstalWpMediaClass)
     *
     * The media class of this endpoint
     */
    astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_PROP_MEDIA_CLASS] =
        g_param_spec_enum("media-class", "media-class", "media-class", ASTAL_WP_TYPE_MEDIA_CLASS, 1,
                          G_PARAM_READABLE);
    /**
     * AstalWpEndpoint:is-default:
     *
     * Whether this endpoint is the default one used for this media-class. Note that setting this
     * property to false has no effect.
     */
    astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_PROP_DEFAULT] =
        g_param_spec_boolean("is-default", "is-default", "is-default", FALSE, G_PARAM_READWRITE);
    /**
     * AstalWpEndpoint:lock-channels:
     *
     * Whether to lock the channels together or not.
     */
    astal_wp_endpoint_properties[ASTAL_WP_ENDPOINT_PROP_LOCK_CHANNELS] = g_param_spec_boolean(
        "lock-channels", "lock-channels", "lock channels", FALSE, G_PARAM_READWRITE);

    g_object_class_install_properties(object_class, ASTAL_WP_ENDPOINT_N_PROPERTIES,
                                      astal_wp_endpoint_properties);
}
