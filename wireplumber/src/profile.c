#include "profile.h"

#include <wp/wp.h>

struct _AstalWpProfile {
    GObject parent_instance;

    gint index;
    gchar *description;
};

G_DEFINE_FINAL_TYPE(AstalWpProfile, astal_wp_profile, G_TYPE_OBJECT);

typedef enum {
    ASTAL_WP_PROFILE_PROP_INDEX = 1,
    ASTAL_WP_PROFILE_PROP_DESCRIPTION,
    ASTAL_WP_PROFILE_N_PROPERTIES,
} AstalWpProfileProperties;

static GParamSpec *astal_wp_profile_properties[ASTAL_WP_PROFILE_N_PROPERTIES] = {
    NULL,
};

gint astal_wp_profile_get_index(AstalWpProfile *self) { return self->index; }

const gchar *astal_wp_profile_get_description(AstalWpProfile *self) { return self->description; }

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
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_wp_profile_init(AstalWpProfile *self) { self->description = NULL; }

static void astal_wp_profile_finalize(GObject *object) {
    AstalWpProfile *self = ASTAL_WP_PROFILE(object);
    g_free(self->description);
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
    g_object_class_install_properties(object_class, ASTAL_WP_PROFILE_N_PROPERTIES,
                                      astal_wp_profile_properties);
}
