#ifndef ASTAL_WP_DEVICE_H
#define ASTAL_WP_DEVICE_H

#include <glib-object.h>

#include "profile.h"
#include "route.h"

G_BEGIN_DECLS

#define ASTAL_WP_TYPE_DEVICE (astal_wp_device_get_type())

G_DECLARE_FINAL_TYPE(AstalWpDevice, astal_wp_device, ASTAL_WP, DEVICE, GObject)

guint astal_wp_device_get_id(AstalWpDevice *self);
const gchar *astal_wp_device_get_description(AstalWpDevice *self);
const gchar *astal_wp_device_get_icon(AstalWpDevice *self);
AstalWpDeviceType astal_wp_device_get_device_type(AstalWpDevice *self);
const gchar *astal_wp_device_get_form_factor(AstalWpDevice *self);

AstalWpProfile *astal_wp_device_get_profile(AstalWpDevice *self, gint id);
GList *astal_wp_device_get_profiles(AstalWpDevice *self);
void astal_wp_device_set_active_profile_id(AstalWpDevice *self, int profile_id);
gint astal_wp_device_get_active_profile_id(AstalWpDevice *self);

gint astal_wp_device_get_input_route_id(AstalWpDevice *self);
gint astal_wp_device_get_output_route_id(AstalWpDevice *self);
AstalWpRoute *astal_wp_device_get_route(AstalWpDevice *self, gint id);
void astal_wp_device_set_route(AstalWpDevice *self, AstalWpRoute *route, guint card_device);
GList *astal_wp_device_get_routes(AstalWpDevice *self);
GList *astal_wp_device_get_input_routes(AstalWpDevice *self);
GList *astal_wp_device_get_output_routes(AstalWpDevice *self);

G_END_DECLS

#endif  // !ASTAL_WP_DEVICE_H
