#include "device-private.h"

#include <wp/wp.h>
#include "profile.h"


struct _AstalWpDevice {
    GObject parent_instance;

    guint id;
    gchar *description;
    gchar *icon;
    gint active_profile;
};

typedef struct {
    WpDevice *device;
    GHashTable *profiles;
} AstalWpDevicePrivate;

G_DEFINE_FINAL_TYPE_WITH_PRIVATE(AstalWpDevice, astal_wp_device, G_TYPE_OBJECT);

typedef enum {
    ASTAL_WP_DEVICE_PROP_ID = 1,
    ASTAL_WP_DEVICE_PROP_DESCRIPTION,
    ASTAL_WP_DEVICE_PROP_ICON,
    ASTAL_WP_DEVICE_PROP_PROFILES,
    ASTAL_WP_DEVICE_PROP_ACTIVE_PROFILE,
    ASTAL_WP_DEVICE_N_PROPERTIES,
} AstalWpDeviceProperties;

typedef enum { ASTAL_WP_DEVICE_SIGNAL_CHANGED, ASTAL_WP_DEVICE_N_SIGNALS } AstalWpDeviceSignals;

static guint astal_wp_device_signals[ASTAL_WP_DEVICE_N_SIGNALS] = {
    0,
};
static GParamSpec *astal_wp_device_properties[ASTAL_WP_DEVICE_N_PROPERTIES] = {
    NULL,
};

guint astal_wp_device_get_id(AstalWpDevice *self) { return self->id; }

const gchar *astal_wp_device_get_description(AstalWpDevice *self) { return self->description; }

const gchar *astal_wp_device_get_icon(AstalWpDevice *self) { return self->icon; }

gint astal_wp_device_get_active_profile(AstalWpDevice *self) { return self->active_profile; }

void astal_wp_device_set_active_profile(AstalWpDevice *self, int profile_id) {
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);

    WpSpaPodBuilder *builder =
        wp_spa_pod_builder_new_object("Spa:Pod:Object:Param:Profile", "Profile");
    wp_spa_pod_builder_add_property(builder, "index");
    wp_spa_pod_builder_add_int(builder, profile_id);
    WpSpaPod *pod = wp_spa_pod_builder_end(builder);
    wp_pipewire_object_set_param(WP_PIPEWIRE_OBJECT(priv->device), "Profile", 0, pod);

    wp_spa_pod_unref(pod);
    wp_spa_pod_builder_unref(builder);
}

/**
 * astal_wp_device_get_profile:
 * @self: the AstalWpDevice object
 * @id: the id of the profile
 *
 * Returns: (transfer none) (nullable): the profile with the given id
 */
AstalWpProfile *astal_wp_device_get_profile(AstalWpDevice *self, gint id) {
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);

    return g_hash_table_lookup(priv->profiles, GINT_TO_POINTER(id));
}

/**
 * astal_wp_device_get_profiles:
 * @self: the AstalWpDevice object
 *
 * Returns: (transfer container) (nullable) (type GList(AstalWpProfile)): a GList containing the
 * profiles
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
    }
    wp_iterator_unref(iter);

    g_object_notify(G_OBJECT(self), "profiles");
}

static void astal_wp_device_update_active_profile(AstalWpDevice *self) {
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);

    WpIterator *iter =
        wp_pipewire_object_enum_params_sync(WP_PIPEWIRE_OBJECT(priv->device), "Profile", NULL);
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
    if (description == NULL) {
        icon = "soundcard-symbolic";
    }
    g_free(self->icon);
    self->icon = g_strdup(icon);

    astal_wp_device_update_profiles(self);
    astal_wp_device_update_active_profile(self);

    g_object_notify(G_OBJECT(self), "id");
    g_object_notify(G_OBJECT(self), "icon");
    g_object_notify(G_OBJECT(self), "description");
    g_signal_emit_by_name(self, "changed");
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

    astal_wp_device_properties[ASTAL_WP_DEVICE_PROP_ID] =
        g_param_spec_uint("id", "id", "id", 0, UINT_MAX, 0, G_PARAM_READABLE);
    astal_wp_device_properties[ASTAL_WP_DEVICE_PROP_DESCRIPTION] =
        g_param_spec_string("description", "description", "description", NULL, G_PARAM_READABLE);
    astal_wp_device_properties[ASTAL_WP_DEVICE_PROP_ICON] =
        g_param_spec_string("icon", "icon", "icon", NULL, G_PARAM_READABLE);
    /**
     * AstalWpDevice:profiles: (type GList(AstalWpProfile)) (transfer container)
     *
     * A list of AstalWpProfile objects
     */
    astal_wp_device_properties[ASTAL_WP_DEVICE_PROP_PROFILES] =
        g_param_spec_pointer("profiles", "profiles", "profiles", G_PARAM_READABLE);
    astal_wp_device_properties[ASTAL_WP_DEVICE_PROP_ACTIVE_PROFILE] =
        g_param_spec_int("active-profile-id", "active-profile-id", "active-profile-id", G_MININT,
                         G_MAXINT, 0, G_PARAM_READWRITE);

    g_object_class_install_properties(object_class, ASTAL_WP_DEVICE_N_PROPERTIES,
                                      astal_wp_device_properties);

    astal_wp_device_signals[ASTAL_WP_DEVICE_SIGNAL_CHANGED] =
        g_signal_new("changed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL, NULL,
                     G_TYPE_NONE, 0);
}
