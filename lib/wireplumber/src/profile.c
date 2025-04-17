#include "profile.h"

#include <wp/wp.h>

#include "astal-wp-enum-types.h"
#include "glib-object.h"

struct _AstalWpProfile {
    GObject parent_instance;

    gint index;
    gchar *description;
    gchar *name;
    gint priority;
    AstalWpAvailable available;
};

G_DEFINE_FINAL_TYPE(AstalWpProfile, astal_wp_profile, G_TYPE_OBJECT);

typedef enum {
    ASTAL_WP_PROFILE_PROP_INDEX = 1,
    ASTAL_WP_PROFILE_PROP_DESCRIPTION,
    ASTAL_WP_PROFILE_PROP_NAME,
    ASTAL_WP_PROFILE_PROP_PRIORITY,
    ASTAL_WP_PROFILE_PROP_AVAILABLE,
    ASTAL_WP_PROFILE_N_PROPERTIES,
} AstalWpProfileProperties;

static GParamSpec *astal_wp_profile_properties[ASTAL_WP_PROFILE_N_PROPERTIES] = {
    NULL,
};

gint astal_wp_profile_get_index(AstalWpProfile *self) { return self->index; }

const gchar *astal_wp_profile_get_description(AstalWpProfile *self) { return self->description; }

const gchar *astal_wp_profile_get_name(AstalWpProfile *self) { return self->name; }

AstalWpAvailable astal_wp_profile_get_available(AstalWpProfile *self) { return self->available; }

gint astal_wp_profile_get_priority(AstalWpProfile *self) { return self->priority; }

static void astal_wp_profile_get_property(GObject *object, guint property_id, GValue *value,
                                          GParamSpec *pspec) {
    AstalWpProfile *self = ASTAL_WP_PROFILE(object);

    switch (property_id) {
        case ASTAL_WP_PROFILE_PROP_INDEX:
            g_value_set_int(value, self->index);
            break;
        case ASTAL_WP_PROFILE_PROP_DESCRIPTION:
            g_value_set_string(value, self->description);
            break;
        case ASTAL_WP_PROFILE_PROP_NAME:
            g_value_set_string(value, self->name);
            break;
        case ASTAL_WP_PROFILE_PROP_PRIORITY:
            g_value_set_int(value, self->priority);
            break;
        case ASTAL_WP_PROFILE_PROP_AVAILABLE:
            g_value_set_enum(value, self->available);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_wp_profile_set_property(GObject *object, guint property_id, const GValue *value,
                                          GParamSpec *pspec) {
    AstalWpProfile *self = ASTAL_WP_PROFILE(object);

    switch (property_id) {
        case ASTAL_WP_PROFILE_PROP_INDEX:
            self->index = g_value_get_int(value);
            break;
        case ASTAL_WP_PROFILE_PROP_DESCRIPTION:
            g_free(self->description);
            self->description = g_strdup(g_value_get_string(value));
            break;
        case ASTAL_WP_PROFILE_PROP_NAME:
            g_free(self->name);
            self->name = g_strdup(g_value_get_string(value));
            break;
        case ASTAL_WP_PROFILE_PROP_PRIORITY:
            self->priority = g_value_get_int(value);
            break;
        case ASTAL_WP_PROFILE_PROP_AVAILABLE:
            self->available = g_value_get_enum(value);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_wp_profile_init(AstalWpProfile *self) { self->description = NULL; }

static void astal_wp_profile_finalize(GObject *object) {
    AstalWpProfile *self = ASTAL_WP_PROFILE(object);
    g_free(self->description);
    g_free(self->name);

    G_OBJECT_CLASS(astal_wp_profile_parent_class)->finalize(object);
}

static void astal_wp_profile_class_init(AstalWpProfileClass *class) {
    GObjectClass *object_class = G_OBJECT_CLASS(class);
    object_class->finalize = astal_wp_profile_finalize;
    object_class->get_property = astal_wp_profile_get_property;
    object_class->set_property = astal_wp_profile_set_property;

    astal_wp_profile_properties[ASTAL_WP_PROFILE_PROP_DESCRIPTION] =
        g_param_spec_string("description", "description", "description", NULL,
                            G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    astal_wp_profile_properties[ASTAL_WP_PROFILE_PROP_INDEX] =
        g_param_spec_int("index", "index", "index", G_MININT, G_MAXINT, 0,
                         G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);

    astal_wp_profile_properties[ASTAL_WP_PROFILE_PROP_NAME] = g_param_spec_string(
        "name", "name", "name", NULL, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    astal_wp_profile_properties[ASTAL_WP_PROFILE_PROP_PRIORITY] =
        g_param_spec_int("priority", "priority", "priority", G_MININT, G_MAXINT, 0,
                         G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    /**
     * AstalWpProfile:available: (type AstalWpAvailable)
     *
     * the available state of this profile
     */
    astal_wp_profile_properties[ASTAL_WP_PROFILE_PROP_AVAILABLE] =
        g_param_spec_enum("available", "available", "available", ASTAL_WP_TYPE_AVAILABLE, 0,
                          G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);

    g_object_class_install_properties(object_class, ASTAL_WP_PROFILE_N_PROPERTIES,
                                      astal_wp_profile_properties);
}
