#ifndef ASTAL_WP_PROFILE_H
#define ASTAL_WP_PROFILE_H

#include <glib-object.h>

#include "enums.h"

G_BEGIN_DECLS

#define ASTAL_WP_TYPE_PROFILE (astal_wp_profile_get_type())

G_DECLARE_FINAL_TYPE(AstalWpProfile, astal_wp_profile, ASTAL_WP, PROFILE, GObject)

gint astal_wp_profile_get_index(AstalWpProfile *self);
const gchar *astal_wp_profile_get_description(AstalWpProfile *self);
const gchar *astal_wp_profile_get_name(AstalWpProfile *self);
AstalWpAvailable astal_wp_profile_get_available(AstalWpProfile *self);
gint astal_wp_profile_get_priority(AstalWpProfile *self);

G_END_DECLS

#endif  // !ASTAL_WP_PROFILE_H
