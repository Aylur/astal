#include "device.h"

#include <wp/wp.h>

#include "astal-wp-enum-types.h"
#include "enums.h"
#include "glib-object.h"
#include "glib.h"
#include "profile.h"
#include "route.h"

struct _AstalWpDevice {
    GObject parent_instance;

    guint id;
    gchar *description;
    gchar *icon;
    gchar *from_factor;
    gint active_profile_id;
    gint input_route_id;
    gint output_route_id;
    AstalWpDeviceType type;
};

typedef struct {
    WpDevice *device;
    GHashTable *profiles;
    GHashTable *routes;
} AstalWpDevicePrivate;

G_DEFINE_FINAL_TYPE_WITH_PRIVATE(AstalWpDevice, astal_wp_device, G_TYPE_OBJECT);

typedef enum {
    ASTAL_WP_DEVICE_PROP_ID = 1,
    ASTAL_WP_DEVICE_PROP_DEVICE,
    ASTAL_WP_DEVICE_PROP_DESCRIPTION,
    ASTAL_WP_DEVICE_PROP_ICON,
    ASTAL_WP_DEVICE_PROP_PROFILES,
    ASTAL_WP_DEVICE_PROP_ACTIVE_PROFILE_ID,
    ASTAL_WP_DEVICE_PROP_ROUTES,
    ASTAL_WP_DEVICE_PROP_INPUT_ROUTES,
    ASTAL_WP_DEVICE_PROP_OUTPUT_ROUTES,
    ASTAL_WP_DEVICE_PROP_INPUT_ROUTE_ID,
    ASTAL_WP_DEVICE_PROP_OUTPUT_ROUTE_ID,
    ASTAL_WP_DEVICE_PROP_DEVICE_TYPE,
    ASTAL_WP_DEVICE_PROP_FORM_FACTOR,
    ASTAL_WP_DEVICE_N_PROPERTIES,
} AstalWpDeviceProperties;

static GParamSpec *astal_wp_device_properties[ASTAL_WP_DEVICE_N_PROPERTIES] = {
    NULL,
};

/**
 * astal_wp_device_get_form_factor
 * @self: the AstalWpDevice object
 *
 * gets the form factor of this device.
 */
const gchar *astal_wp_device_get_form_factor(AstalWpDevice *self) { return self->from_factor; }

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
 * astal_wp_device_get_active_profile_id
 * @self: the AstalWpDevice object
 *
 * gets the currently active profile of this device
 *
 */
gint astal_wp_device_get_active_profile_id(AstalWpDevice *self) { return self->active_profile_id; }

/**
 * astal_wp_device_set_active_profile_id
 * @self: the AstalWpDevice object
 * @profile_id: the id of the profile
 *
 * sets the profile for this device
 *
 */
void astal_wp_device_set_active_profile_id(AstalWpDevice *self, int profile_id) {
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

/**
 * astal_wp_device_get_input_route_id
 * @self: the AstalWpDevice object
 *
 * gets the currently active input route of this device
 *
 */
gint astal_wp_device_get_input_route_id(AstalWpDevice *self) { return self->input_route_id; }

/**
 * astal_wp_device_get_output_route_id
 * @self: the AstalWpDevice object
 *
 * gets the currently active output route of this device
 *
 */
gint astal_wp_device_get_output_route_id(AstalWpDevice *self) { return self->output_route_id; }

/**
 * astal_wp_device_get_route:
 * @self: the AstalWpDevice object
 * @id: the id of the route
 *
 * gets the route with the given id
 *
 * Returns: (transfer none) (nullable)
 */
AstalWpRoute *astal_wp_device_get_route(AstalWpDevice *self, gint id) {
    g_return_val_if_fail(self != NULL, NULL);
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);

    return g_hash_table_lookup(priv->routes, GINT_TO_POINTER(id));
}

/**
 * astal_wp_device_set_route:
 * @self: The AstalWpDevice object
 * @card_device: card device index
 *
 * sets the route for this device. You should use the [method@AstalWp.Endpoint.set_route] instead.
 */
void astal_wp_device_set_route(AstalWpDevice *self, AstalWpRoute *route, guint card_device) {
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);
    WpSpaPodBuilder *builder = wp_spa_pod_builder_new_object("Spa:Pod:Object:Param:Route", "Route");
    wp_spa_pod_builder_add_property(builder, "index");
    wp_spa_pod_builder_add_int(builder, astal_wp_route_get_index(route));
    wp_spa_pod_builder_add_property(builder, "device");
    wp_spa_pod_builder_add_int(builder, card_device);
    wp_spa_pod_builder_add_property(builder, "save");
    wp_spa_pod_builder_add_boolean(builder, TRUE);
    WpSpaPod *pod = wp_spa_pod_builder_end(builder);
    wp_pipewire_object_set_param(WP_PIPEWIRE_OBJECT(priv->device), "Route", 0, pod);

    wp_spa_pod_builder_unref(builder);
}

/**
 * astal_wp_device_get_routes:
 * @self: the AstalWpDevice object
 *
 * gets a GList containing the routes
 *
 * Returns: (transfer container) (nullable) (type GList(AstalWpRoute))
 */
GList *astal_wp_device_get_routes(AstalWpDevice *self) {
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);
    return g_hash_table_get_values(priv->routes);
}

static void astal_wp_device_filter_by_direction(gpointer key, gpointer value, gpointer user_data) {
    struct {
        AstalWpDirection direction;
        GList **list;
    } *data = user_data;
    if (astal_wp_route_get_direction(ASTAL_WP_ROUTE(value)) == data->direction) {
        *(data->list) = g_list_append(*(data->list), ASTAL_WP_ROUTE(value));
    }
}

/**
 * astal_wp_device_get_input_routes:
 * @self: the AstalWpDevice object
 *
 * gets a GList containing the input routes
 *
 * Returns: (transfer container) (nullable) (type GList(AstalWpRoute))
 */
GList *astal_wp_device_get_input_routes(AstalWpDevice *self) {
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);

    GList *routes = NULL;
    struct {
        AstalWpDirection direction;
        GList **list;
    } data = {ASTAL_WP_DIRECTION_INPUT, &routes};

    g_hash_table_foreach(priv->routes, astal_wp_device_filter_by_direction, &data);
    return routes;
}

/**
 * astal_wp_device_get_output_routes:
 * @self: the AstalWpDevice object
 *
 * gets a GList containing the output routes
 *
 * Returns: (transfer container) (nullable) (type GList(AstalWpRoute))
 */
GList *astal_wp_device_get_output_routes(AstalWpDevice *self) {
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);

    GList *routes = NULL;
    struct {
        AstalWpDirection direction;
        GList **list;
    } data = {ASTAL_WP_DIRECTION_OUTPUT, &routes};

    g_hash_table_foreach(priv->routes, astal_wp_device_filter_by_direction, &data);
    return routes;
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
        case ASTAL_WP_DEVICE_PROP_ACTIVE_PROFILE_ID:
            g_value_set_int(value, self->active_profile_id);
            break;
        case ASTAL_WP_DEVICE_PROP_ROUTES:
            g_value_set_pointer(value, astal_wp_device_get_routes(self));
            break;
        case ASTAL_WP_DEVICE_PROP_INPUT_ROUTES:
            g_value_set_pointer(value, astal_wp_device_get_input_routes(self));
            break;
        case ASTAL_WP_DEVICE_PROP_OUTPUT_ROUTES:
            g_value_set_pointer(value, astal_wp_device_get_output_routes(self));
            break;
        case ASTAL_WP_DEVICE_PROP_INPUT_ROUTE_ID:
            g_value_set_int(value, self->input_route_id);
            break;
        case ASTAL_WP_DEVICE_PROP_OUTPUT_ROUTE_ID:
            g_value_set_int(value, self->output_route_id);
            break;
        case ASTAL_WP_DEVICE_PROP_FORM_FACTOR:
            g_value_set_string(value, self->from_factor);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_wp_device_set_property(GObject *object, guint property_id, const GValue *value,
                                         GParamSpec *pspec) {
    AstalWpDevice *self = ASTAL_WP_DEVICE(object);
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);
    WpDevice *dev;

    switch (property_id) {
        case ASTAL_WP_DEVICE_PROP_DEVICE:
            dev = g_value_get_object(value);
            if (dev != NULL && WP_IS_DEVICE(dev)) {
                g_clear_object(&priv->device);
                priv->device = g_object_ref(dev);
            }
            break;
        case ASTAL_WP_DEVICE_PROP_ACTIVE_PROFILE_ID:
            astal_wp_device_set_active_profile_id(self, g_value_get_int(value));
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
        wp_spa_pod_get_object(pod, NULL, "index", "i", &index, NULL);

        self->active_profile_id = index;
        g_value_unset(&profile);
    }
    wp_iterator_unref(iter);

    g_object_notify(G_OBJECT(self), "active-profile-id");
}

static void astal_wp_device_update_routes(AstalWpDevice *self) {
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);
    g_hash_table_remove_all(priv->routes);

    WpIterator *iter =
        wp_pipewire_object_enum_params_sync(WP_PIPEWIRE_OBJECT(priv->device), "EnumRoute", NULL);
    if (iter == NULL) return;
    GValue route = G_VALUE_INIT;
    while (wp_iterator_next(iter, &route)) {
        WpSpaPod *pod = g_value_get_boxed(&route);

        gint index, priority = 0;
        gchar *description = NULL, *name = NULL;
        AstalWpDirection direction = ASTAL_WP_DIRECTION_OUTPUT;
        AstalWpAvailable available = ASTAL_WP_AVAILABLE_UNKNOWN;

        wp_spa_pod_get_object(pod, NULL, "index", "i", &index, "name", "?s", &name, "description",
                              "s", &description, "direction", "I", &direction, "priority", "?i",
                              &priority, "available", "?I", &available, NULL);

        g_hash_table_insert(priv->routes, GINT_TO_POINTER(index),
                            g_object_new(ASTAL_WP_TYPE_ROUTE, "index", index, "description",
                                         description, "name", name, "direction", direction,
                                         "priority", priority, "available", available, NULL));
        g_value_unset(&route);
    }
    wp_iterator_unref(iter);

    g_object_notify(G_OBJECT(self), "routes");
}

static void astal_wp_device_update_active_routes(AstalWpDevice *self) {
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);

    WpIterator *iter =
        wp_pipewire_object_enum_params_sync(WP_PIPEWIRE_OBJECT(priv->device), "Route", NULL);
    if (iter == NULL) return;
    GValue route = G_VALUE_INIT;
    while (wp_iterator_next(iter, &route)) {
        WpSpaPod *pod = g_value_get_boxed(&route);

        gint index;
        gint direction;
        wp_spa_pod_get_object(pod, NULL, "index", "i", &index, "direction", "Id", &direction, NULL);

        if (direction == 0) {
            self->input_route_id = index;
        } else if (direction == 1) {
            self->output_route_id = index;
        }
        g_value_unset(&route);
    }
    wp_iterator_unref(iter);

    g_object_notify(G_OBJECT(self), "input-route-id");
    g_object_notify(G_OBJECT(self), "output-route-id");
}

static void astal_wp_device_update_properties(AstalWpDevice *self) {
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);
    if (priv->device == NULL) return;

    WpPipewireObject *pwo = WP_PIPEWIRE_OBJECT(priv->device);

    guint id = wp_proxy_get_bound_id(WP_PROXY(priv->device));
    if (self->id != id) {
        self->id = id;
        g_object_notify(G_OBJECT(self), "id");
    }

    const gchar *description = wp_pipewire_object_get_property(pwo, "device.description");
    if (description == NULL) {
        description = wp_pipewire_object_get_property(pwo, "device.name");
    }
    if (g_strcmp0(self->description, description)) {
        g_free(self->description);
        self->description = g_strdup(description);
        g_object_notify(G_OBJECT(self), "description");
    }

    const gchar *icon = wp_pipewire_object_get_property(pwo, "device.icon-name");
    if (icon == NULL) {
        icon = "audio-card-symbolic";
    }
    if (g_strcmp0(self->icon, icon)) {
        g_free(self->icon);
        self->icon = g_strdup(icon);
        g_object_notify(G_OBJECT(self), "icon");
    }

    const gchar *type = wp_pipewire_object_get_property(pwo, "media.class");
    AstalWpDeviceType device_type = astal_wp_device_type_from_string(type);
    if (device_type != self->type) {
        self->type = device_type;
        g_object_notify(G_OBJECT(self), "device-type");
    }

    const gchar *form_factor = wp_pipewire_object_get_property(pwo, "device.form-factor");
    if (g_strcmp0(self->from_factor, form_factor)) {
        g_free(self->from_factor);
        self->from_factor = g_strdup(form_factor);
        g_object_notify(G_OBJECT(self), "form-factor");
    }
}

static void astal_wp_device_params_changed(AstalWpDevice *self, const gchar *prop) {
    g_object_freeze_notify(G_OBJECT(self));

    if (!g_strcmp0(prop, "EnumProfile")) {
        astal_wp_device_update_profiles(self);
    } else if (!g_strcmp0(prop, "Profile")) {
        astal_wp_device_update_active_profile(self);
    } else if (!g_strcmp0(prop, "Props")) {
        astal_wp_device_update_properties(self);
    } else if (!g_strcmp0(prop, "EnumRoute")) {
        astal_wp_device_update_routes(self);
    } else if (!g_strcmp0(prop, "Route")) {
        astal_wp_device_update_active_routes(self);
    }

    g_object_thaw_notify(G_OBJECT(self));
}

static void astal_wp_device_pw_properties_changed(AstalWpDevice *self) {
    astal_wp_device_params_changed(self, "Props");
}

void astal_wp_device_constructed(GObject *object) {
    AstalWpDevice *self = ASTAL_WP_DEVICE(object);
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);

    g_signal_connect_swapped(priv->device, "params-changed",
                             G_CALLBACK(astal_wp_device_params_changed), self);

    g_signal_connect_swapped(priv->device, "notify::properties",
                             G_CALLBACK(astal_wp_device_pw_properties_changed), self);
    astal_wp_device_params_changed(self, "Props");
    astal_wp_device_params_changed(self, "EnumProfile");
    astal_wp_device_params_changed(self, "Profile");
    astal_wp_device_params_changed(self, "EnumRoute");
    astal_wp_device_params_changed(self, "Route");

    G_OBJECT_CLASS(astal_wp_device_parent_class)->constructed(object);
}

static void astal_wp_device_init(AstalWpDevice *self) {
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);
    priv->device = NULL;

    priv->profiles = g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL, g_object_unref);
    priv->routes = g_hash_table_new_full(g_direct_hash, g_direct_equal, NULL, g_object_unref);

    self->description = NULL;
    self->icon = NULL;
    self->from_factor = NULL;
}

static void astal_wp_device_dispose(GObject *object) {
    AstalWpDevice *self = ASTAL_WP_DEVICE(object);
    AstalWpDevicePrivate *priv = astal_wp_device_get_instance_private(self);

    g_clear_object(&priv->device);
    g_hash_table_unref(priv->profiles);
    g_hash_table_unref(priv->routes);

    G_OBJECT_CLASS(astal_wp_device_parent_class)->dispose(object);
}

static void astal_wp_device_finalize(GObject *object) {
    AstalWpDevice *self = ASTAL_WP_DEVICE(object);
    g_free(self->description);
    g_free(self->icon);
    g_free(self->from_factor);

    G_OBJECT_CLASS(astal_wp_device_parent_class)->finalize(object);
}

static void astal_wp_device_class_init(AstalWpDeviceClass *class) {
    GObjectClass *object_class = G_OBJECT_CLASS(class);
    object_class->dispose = astal_wp_device_dispose;
    object_class->finalize = astal_wp_device_finalize;
    object_class->get_property = astal_wp_device_get_property;
    object_class->set_property = astal_wp_device_set_property;
    object_class->constructed = astal_wp_device_constructed;

    /**
     * AstalWpDevice:id:
     *
     * The id of this device.
     */
    astal_wp_device_properties[ASTAL_WP_DEVICE_PROP_ID] =
        g_param_spec_uint("id", "id", "id", 0, UINT_MAX, 0, G_PARAM_READABLE);
    /**
     *  AstalWpDevice:device: (skip)
     */
    astal_wp_device_properties[ASTAL_WP_DEVICE_PROP_DEVICE] = g_param_spec_object(
        "device", "device", "device", WP_TYPE_DEVICE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    /*
     * AstalWpDevice:description:
     *
     * The description of this device.
     */
    astal_wp_device_properties[ASTAL_WP_DEVICE_PROP_DESCRIPTION] =
        g_param_spec_string("description", "description", "description", NULL, G_PARAM_READABLE);
    /**
     * AstalWpDevice:icon:
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
     * AstalWpDevice:active-profile-id:
     *
     * The id of the currently active profile.
     */
    astal_wp_device_properties[ASTAL_WP_DEVICE_PROP_ACTIVE_PROFILE_ID] =
        g_param_spec_int("active-profile-id", "active-profile-id", "active-profile-id", G_MININT,
                         G_MAXINT, 0, G_PARAM_READWRITE | G_PARAM_EXPLICIT_NOTIFY);
    /**
     * AstalWpDevice:routes: (type GList(AstalWpRoute)) (transfer container)
     *
     * A list of available routes
     */
    astal_wp_device_properties[ASTAL_WP_DEVICE_PROP_ROUTES] =
        g_param_spec_pointer("routes", "routes", "routes", G_PARAM_READABLE);
    /**
     * AstalWpDevice:input-routes: (type GList(AstalWpRoute)) (transfer container)
     *
     * A list of available input routes
     */
    astal_wp_device_properties[ASTAL_WP_DEVICE_PROP_INPUT_ROUTES] =
        g_param_spec_pointer("input-routes", "input-routes", "input-routes", G_PARAM_READABLE);
    /**
     * AstalWpDevice:output-routes: (type GList(AstalWpRoute)) (transfer container)
     *
     * A list of available output routes
     */
    astal_wp_device_properties[ASTAL_WP_DEVICE_PROP_OUTPUT_ROUTES] =
        g_param_spec_pointer("output-routes", "output-routes", "output-routes", G_PARAM_READABLE);
    /**
     * AstalWpDevice:input-route-id:
     *
     * The id of the currently active input route.
     */
    astal_wp_device_properties[ASTAL_WP_DEVICE_PROP_INPUT_ROUTE_ID] =
        g_param_spec_int("input-route-id", "input-route-id", "input-route-id", G_MININT, G_MAXINT,
                         0, G_PARAM_READWRITE | G_PARAM_EXPLICIT_NOTIFY);
    /**
     * AstalWpDevice:output-route-id:
     *
     * The id of the currently active output route.
     */
    astal_wp_device_properties[ASTAL_WP_DEVICE_PROP_OUTPUT_ROUTE_ID] =
        g_param_spec_int("output-route-id", "output-route-id", "output-route-id", G_MININT,
                         G_MAXINT, 0, G_PARAM_READWRITE | G_PARAM_EXPLICIT_NOTIFY);
    /**
     * AstalWpDevice:form-factor:
     *
     * The from factor of this device.
     */
    astal_wp_device_properties[ASTAL_WP_DEVICE_PROP_FORM_FACTOR] =
        g_param_spec_string("form-factor", "form-factor", "form-factor", NULL, G_PARAM_READABLE);

    g_object_class_install_properties(object_class, ASTAL_WP_DEVICE_N_PROPERTIES,
                                      astal_wp_device_properties);
}
