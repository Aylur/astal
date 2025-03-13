#include "route.h"

#include <wp/wp.h>

#include "astal-wp-enum-types.h"
#include "glib-object.h"
#include "glib.h"

struct _AstalWpRoute {
    GObject parent_instance;

    gint index;
    gchar *name;
    gchar *description;
    AstalWpDirection direction;
    AstalWpAvailable available;
    gint priority;
};

G_DEFINE_FINAL_TYPE(AstalWpRoute, astal_wp_route, G_TYPE_OBJECT);

typedef enum {
    ASTAL_WP_ROUTE_PROP_INDEX = 1,
    ASTAL_WP_ROUTE_PROP_DESCRIPTION,
    ASTAL_WP_ROUTE_PROP_NAME,
    ASTAL_WP_ROUTE_PROP_DIRECTION,
    ASTAL_WP_ROUTE_PROP_AVAILABLE,
    ASTAL_WP_ROUTE_PROP_PRIORITY,
    ASTAL_WP_ROUTE_N_PROPERTIES,
} AstalWpRouteProperties;

static GParamSpec *astal_wp_route_properties[ASTAL_WP_ROUTE_N_PROPERTIES] = {
    NULL,
};

gint astal_wp_route_get_index(AstalWpRoute *self) { return self->index; }

const gchar *astal_wp_route_get_description(AstalWpRoute *self) { return self->description; }

const gchar *astal_wp_route_get_name(AstalWpRoute *self) { return self->name; }

AstalWpDirection astal_wp_route_get_direction(AstalWpRoute *self) { return self->direction; }

AstalWpAvailable astal_wp_route_get_available(AstalWpRoute *self) { return self->available; }

gint astal_wp_route_get_priority(AstalWpRoute *self) { return self->priority; }

static void astal_wp_route_get_property(GObject *object, guint property_id, GValue *value,
                                        GParamSpec *pspec) {
    AstalWpRoute *self = ASTAL_WP_ROUTE(object);

    switch (property_id) {
        case ASTAL_WP_ROUTE_PROP_INDEX:
            g_value_set_int(value, self->index);
            break;
        case ASTAL_WP_ROUTE_PROP_DESCRIPTION:
            g_value_set_string(value, self->description);
            break;
        case ASTAL_WP_ROUTE_PROP_NAME:
            g_value_set_string(value, self->name);
            break;
        case ASTAL_WP_ROUTE_PROP_PRIORITY:
            g_value_set_int(value, self->priority);
            break;
        case ASTAL_WP_ROUTE_PROP_DIRECTION:
            g_value_set_enum(value, self->direction);
            break;
        case ASTAL_WP_ROUTE_PROP_AVAILABLE:
            g_value_set_enum(value, self->available);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_wp_route_set_property(GObject *object, guint property_id, const GValue *value,
                                        GParamSpec *pspec) {
    AstalWpRoute *self = ASTAL_WP_ROUTE(object);

    switch (property_id) {
        case ASTAL_WP_ROUTE_PROP_INDEX:
            self->index = g_value_get_int(value);
            break;
        case ASTAL_WP_ROUTE_PROP_DESCRIPTION:
            g_free(self->description);
            self->description = g_strdup(g_value_get_string(value));
            break;
        case ASTAL_WP_ROUTE_PROP_NAME:
            g_free(self->name);
            self->name = g_strdup(g_value_get_string(value));
            break;
        case ASTAL_WP_ROUTE_PROP_PRIORITY:
            self->priority = g_value_get_int(value);
            break;
        case ASTAL_WP_ROUTE_PROP_DIRECTION:
            self->direction = g_value_get_enum(value);
            break;
        case ASTAL_WP_ROUTE_PROP_AVAILABLE:
            self->available = g_value_get_enum(value);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_wp_route_init(AstalWpRoute *self) { self->description = NULL; }

static void astal_wp_route_finalize(GObject *object) {
    AstalWpRoute *self = ASTAL_WP_ROUTE(object);
    g_free(self->description);
    g_free(self->name);

    G_OBJECT_CLASS(astal_wp_route_parent_class)->finalize(object);
}

static void astal_wp_route_class_init(AstalWpRouteClass *class) {
    GObjectClass *object_class = G_OBJECT_CLASS(class);
    object_class->finalize = astal_wp_route_finalize;
    object_class->get_property = astal_wp_route_get_property;
    object_class->set_property = astal_wp_route_set_property;

    astal_wp_route_properties[ASTAL_WP_ROUTE_PROP_DESCRIPTION] =
        g_param_spec_string("description", "description", "description", NULL,
                            G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    astal_wp_route_properties[ASTAL_WP_ROUTE_PROP_NAME] = g_param_spec_string(
        "name", "name", "name", NULL, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    astal_wp_route_properties[ASTAL_WP_ROUTE_PROP_INDEX] =
        g_param_spec_int("index", "index", "index", G_MININT, G_MAXINT, 0,
                         G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    astal_wp_route_properties[ASTAL_WP_ROUTE_PROP_PRIORITY] =
        g_param_spec_int("priority", "priority", "priority", G_MININT, G_MAXINT, 0,
                         G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    /**
     * AstalWpRoute:direction: (type AstalWpDirection)
     *
     * The direction for this route
     */
    astal_wp_route_properties[ASTAL_WP_ROUTE_PROP_DIRECTION] =
        g_param_spec_enum("direction", "direction", "direction", ASTAL_WP_TYPE_DIRECTION, 0,
                          G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    /**
     * AstalWpRoute:available: (type AstalWpAvailable)
     *
     * the available state of this route
     */
    astal_wp_route_properties[ASTAL_WP_ROUTE_PROP_AVAILABLE] =
        g_param_spec_enum("available", "available", "available", ASTAL_WP_TYPE_AVAILABLE, 0,
                          G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    g_object_class_install_properties(object_class, ASTAL_WP_ROUTE_N_PROPERTIES,
                                      astal_wp_route_properties);
}
