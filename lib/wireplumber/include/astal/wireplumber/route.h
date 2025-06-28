#ifndef ASTAL_WP_ROUTE_H
#define ASTAL_WP_ROUTE_H

#include <glib-object.h>

#include "enums.h"

G_BEGIN_DECLS

#define ASTAL_WP_TYPE_ROUTE (astal_wp_route_get_type())

G_DECLARE_FINAL_TYPE(AstalWpRoute, astal_wp_route, ASTAL_WP, ROUTE, GObject)

gint astal_wp_route_get_index(AstalWpRoute *self);
const gchar *astal_wp_route_get_description(AstalWpRoute *self);
const gchar *astal_wp_route_get_name(AstalWpRoute *self);
AstalWpDirection astal_wp_route_get_direction(AstalWpRoute *self);
AstalWpAvailable astal_wp_route_get_available(AstalWpRoute *self);
gint astal_wp_route_get_priority(AstalWpRoute *self);
gint astal_wp_route_get_device(AstalWpRoute *self);

G_END_DECLS

#endif  // !ASTAL_WP_ROUTE_H
