#include <wp/wp.h>

#include "device-private.h"
#include "profile.h"

struct _AstalWpDevice {
    GObject parent_instance;

    guint id;
    gchar *description;
    gchar *icon;
    gint active_profile;
    AstalWpDeviceType type;
};

typedef struct {
    WpDevice *device;
    GHashTable *profiles;
} AstalWpDevicePrivate;

G_DEFINE_FINAL_TYPE_WITH_PRIVATE(AstalWpDevice, astal_wp_device, G_TYPE_OBJECT);

G_DEFINE_ENUM_TYPE(AstalWpDeviceType, astal_wp_device_type,
                   G_DEFINE_ENUM_VALUE(ASTAL_WP_DEVICE_TYPE_AUDIO, "Audio/Device"),
                   G_DEFINE_ENUM_VALUE(ASTAL_WP_DEVICE_TYPE_VIDEO, "Video/Device"));

typedef enum {
    ASTAL_WP_DEVICE_PROP_ID = 1,
    ASTAL_WP_DEVICE_PROP_DESCRIPTION,
    ASTAL_WP_DEVICE_PROP_ICON,
    ASTAL_WP_DEVICE_PROP_PROFILES,
    ASTAL_WP_DEVICE_PROP_ACTIVE_PROFILE,
    ASTAL_WP_DEVICE_PROP_DEVICE_TYPE,
    ASTAL_WP_DEVICE_N_PROPERTIES,
} AstalWpDeviceProperties;

static GParamSpec *astal_wp_device_properties[ASTAL_WP_DEVICE_N_PROPERTIES] = {
    NULL,
};

/**
 * astal_wp_device_get_id
 * @self: the AstalWpDevice object
 *
 * gets the id of this device
 *
 */
guint astal_wp_device_get_id(AstalWpDevice *self) { return self->id; }

/**
 * astal_wp_device_get_description
 * @self: the AstalWpDevice object
 *
 * gets the description of this device
 *
 */
const gchar *astal_wp_device_get_description(AstalWpDevice *self) { return self->description; }

/**
 * astal_wp_device_get_icon
 * @self: the AstalWpDevice object
 *
 * gets the icon of this device
 *
 */
const gchar *astal_wp_device_get_icon(AstalWpDevice *self) {
    g_return_val_if_fail(self != NULL, "audio-card-symbolic");
    return self->icon;
}

/**
 * astal_wp_device_get_device_type
 * @self: the AstalWpDevice object
 *
 * gets the type of this device
 *
 */
AstalWpDeviceType astal_wp_device_get_device_type(AstalWpDevice *self) { return self->type; }

/**
 * astal_wp_device_get_active_profile
 * @self: the AstalWpDevice object
 *
 * gets the currently active profile of this device
 *
 */
gint astal_wp_device_get_active_profile(AstalWpDevice *self) { return self->active_profile; }

/**
 * astal_wp_device_set_active_profile
 * @self: the AstalWpDevice object
 * @profile_id: the id of the profile
 *
 * sets the profile for this device
 *
 */
void astal_wp_device_set_active_profile(AstalWpDevice *self, int profile_id) {
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);

    WpSpaPodBuilder *builder =
        wp_spa_pod_builder_new_object("Spa:Pod:Object:Param:Profile", "Profile");
    wp_spa_pod_builder_add_property(builder, "index");
    wp_spa_pod_builder_add_int(builder, profile_id);
    WpSpaPod *pod = wp_spa_pod_builder_end(builder);
    wp_pipewire_object_set_param(WP_PIPEWIRE_OBJECT(priv->device), "Profile", 0, pod);

    wp_spa_pod_builder_unref(builder);
}

/**
 * astal_wp_device_get_profile:
 * @self: the AstalWpDevice object
 * @id: the id of the profile
 *
 * gets the profile with the given id
 *
 * Returns: (transfer none) (nullable)
 */
AstalWpProfile *astal_wp_device_get_profile(AstalWpDevice *self, gint id) {
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);

    return g_hash_table_lookup(priv->profiles, GINT_TO_POINTER(id));
}

/**
 * astal_wp_device_get_profiles:
 * @self: the AstalWpDevice object
 *
 * gets a GList containing the profiles
 *
 * Returns: (transfer container) (nullable) (type GList(AstalWpProfile))
 */
GList *astal_wp_device_get_profiles(AstalWpDevice *self) {
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);
    return g_hash_table_get_values(priv->profiles);
}

static void astal_wp_device_get_property(GObject *object, guint property_id, GValue *value,
                                         GParamSpec *pspec) {
    AstalWpDevice *self = ASTAL_WP_DEVICE(object);

    switch (property_id) {
        case ASTAL_WP_DEVICE_PROP_ID:
            g_value_set_uint(value, self->id);
            break;
        case ASTAL_WP_DEVICE_PROP_DESCRIPTION:
            g_value_set_string(value, self->description);
            break;
        case ASTAL_WP_DEVICE_PROP_ICON:
            g_value_set_string(value, self->icon);
            break;
        case ASTAL_WP_DEVICE_PROP_PROFILES:
            g_value_set_pointer(value, astal_wp_device_get_profiles(self));
            break;
        case ASTAL_WP_DEVICE_PROP_DEVICE_TYPE:
            g_value_set_enum(value, astal_wp_device_get_device_type(self));
            break;
        case ASTAL_WP_DEVICE_PROP_ACTIVE_PROFILE:
            g_value_set_int(value, self->active_profile);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_wp_device_set_property(GObject *object, guint property_id, const GValue *value,
                                         GParamSpec *pspec) {
    AstalWpDevice *self = ASTAL_WP_DEVICE(object);

    switch (property_id) {
        case ASTAL_WP_DEVICE_PROP_ACTIVE_PROFILE:
            astal_wp_device_set_active_profile(self, g_value_get_int(value));
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_wp_device_update_profiles(AstalWpDevice *self) {
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);
    g_hash_table_remove_all(priv->profiles);

    WpIterator *iter =
        wp_pipewire_object_enum_params_sync(WP_PIPEWIRE_OBJECT(priv->device), "EnumProfile", NULL);
    if (iter == NULL) return;
    GValue profile = G_VALUE_INIT;
    while (wp_iterator_next(iter, &profile)) {
        WpSpaPod *pod = g_value_get_boxed(&profile);

        gint index;
        gchar *description;
        wp_spa_pod_get_object(pod, NULL, "index", "i", &index, "description", "s", &description,
                              NULL);

        g_hash_table_insert(
            priv->profiles, GINT_TO_POINTER(index),
            g_object_new(ASTAL_WP_TYPE_PROFILE, "index", index, "description", description, NULL));
        g_value_unset(&profile);
    }
    wp_iterator_unref(iter);

    g_object_notify(G_OBJECT(self), "profiles");
}

static void astal_wp_device_update_active_profile(AstalWpDevice *self) {
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);

    WpIterator *iter =
        wp_pipewire_object_enum_params_sync(WP_PIPEWIRE_OBJECT(priv->device), "Profile", NULL);
    if (iter == NULL) return;
    GValue profile = G_VALUE_INIT;
    while (wp_iterator_next(iter, &profile)) {
        WpSpaPod *pod = g_value_get_boxed(&profile);

        gint index;
        gchar *description;
        wp_spa_pod_get_object(pod, NULL, "index", "i", &index, "description", "s", &description,
                              NULL);

        g_hash_table_insert(
            priv->profiles, GINT_TO_POINTER(index),
            g_object_new(ASTAL_WP_TYPE_PROFILE, "index", index, "description", description, NULL));

        self->active_profile = index;
        g_value_unset(&profile);
    }
    wp_iterator_unref(iter);

    g_object_notify(G_OBJECT(self), "active-profile-id");
}

static void astal_wp_device_params_changed(AstalWpDevice *self, const gchar *prop) {
    if (g_strcmp0(prop, "EnumProfile") == 0) {
        astal_wp_device_update_profiles(self);
    } else if (g_strcmp0(prop, "Profile") == 0) {
        astal_wp_device_update_active_profile(self);
    }
}

static void astal_wp_device_update_properties(AstalWpDevice *self) {
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);
    if (priv->device == NULL) return;
    self->id = wp_proxy_get_bound_id(WP_PROXY(priv->device));
    const gchar *description =
        wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->device), "device.description");
    if (description == NULL) {
        description =
            wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->device), "device.name");
    }
    if (description == NULL) {
        description = "unknown";
    }
    g_free(self->description);
    self->description = g_strdup(description);

    const gchar *icon =
        wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->device), "device.icon-name");
    if (icon == NULL) {
        icon = "audio-card-symbolic";
    }
    g_free(self->icon);
    self->icon = g_strdup(icon);

    const gchar *type =
        wp_pipewire_object_get_property(WP_PIPEWIRE_OBJECT(priv->device), "media.class");
    GEnumClass *enum_class = g_type_class_ref(ASTAL_WP_TYPE_DEVICE_TYPE);
    if (g_enum_get_value_by_nick(enum_class, type) != NULL)
        self->type = g_enum_get_value_by_nick(enum_class, type)->value;
    g_type_class_unref(enum_class);

    astal_wp_device_update_profiles(self);
    astal_wp_device_update_active_profile(self);

    g_object_notify(G_OBJECT(self), "id");
    g_object_notify(G_OBJECT(self), "device-type");
    g_object_notify(G_OBJECT(self), "icon");
    g_object_notify(G_OBJECT(self), "description");
}

AstalWpDevice *astal_wp_device_create(WpDevice *device) {
    AstalWpDevice *self = g_object_new(ASTAL_WP_TYPE_DEVICE, NULL);
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);

    priv->device = g_object_ref(device);

    g_signal_connect_swapped(priv->device, "params-changed",
                             G_CALLBACK(astal_wp_device_params_changed), self);

    astal_wp_device_update_properties(self);
    return self;
}

static void astal_wp_device_init(AstalWpDevice *self) {
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);
    priv->device = NULL;

    priv->profiles = g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL, g_object_unref);

    self->description = NULL;
    self->icon = NULL;
}

static void astal_wp_device_dispose(GObject *object) {
    AstalWpDevice *self = ASTAL_WP_DEVICE(object);
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);

    g_clear_object(&priv->device);
}

static void astal_wp_device_finalize(GObject *object) {
    AstalWpDevice *self = ASTAL_WP_DEVICE(object);
    g_free(self->description);
    g_free(self->icon);
}

static void astal_wp_device_class_init(AstalWpDeviceClass *class) {
    GObjectClass *object_class = G_OBJECT_CLASS(class);
    object_class->dispose = astal_wp_device_dispose;
    object_class->finalize = astal_wp_device_finalize;
    object_class->get_property = astal_wp_device_get_property;
    object_class->set_property = astal_wp_device_set_property;
    /**
     * AstalWpDevice:id
     *
     * The id of this device.
     */
    astal_wp_device_properties[ASTAL_WP_DEVICE_PROP_ID] =
        g_param_spec_uint("id", "id", "id", 0, UINT_MAX, 0, G_PARAM_READABLE);
    /**
     * AstalWpDevice:description
     *
     * The description of this device.
     */
    astal_wp_device_properties[ASTAL_WP_DEVICE_PROP_DESCRIPTION] =
        g_param_spec_string("description", "description", "description", NULL, G_PARAM_READABLE);
    /**
     * AstalWpDevice:icon
     *
     * The icon name for this device.
     */
    astal_wp_device_properties[ASTAL_WP_DEVICE_PROP_ICON] =
        g_param_spec_string("icon", "icon", "icon", NULL, G_PARAM_READABLE);
    /**
     * AstalWpDevice:device-type: (type AstalWpDeviceType)
     *
     * The type of this device
     */
    astal_wp_device_properties[ASTAL_WP_DEVICE_PROP_DEVICE_TYPE] =
        g_param_spec_enum("device-type", "device-type", "device-type", ASTAL_WP_TYPE_DEVICE_TYPE, 1,
                          G_PARAM_READABLE);
    /**
     * AstalWpDevice:profiles: (type GList(AstalWpProfile)) (transfer container)
     *
     * A list of available profiles
     */
    astal_wp_device_properties[ASTAL_WP_DEVICE_PROP_PROFILES] =
        g_param_spec_pointer("profiles", "profiles", "profiles", G_PARAM_READABLE);
    /**
     * AstalWpDevice:active-profile-id
     *
     * The id of the currently active profile.
     */
    astal_wp_device_properties[ASTAL_WP_DEVICE_PROP_ACTIVE_PROFILE] =
        g_param_spec_int("active-profile-id", "active-profile-id", "active-profile-id", G_MININT,
                         G_MAXINT, 0, G_PARAM_READWRITE);

    g_object_class_install_properties(object_class, ASTAL_WP_DEVICE_N_PROPERTIES,
                                      astal_wp_device_properties);
}
