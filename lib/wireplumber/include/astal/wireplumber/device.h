#ifndef ASTAL_WP_DEVICE_H
#define ASTAL_WP_DEVICE_H

#include <glib-object.h>

#include "astal-wp-enum-types.h"
#include "profile.h"

G_BEGIN_DECLS

#define ASTAL_WP_TYPE_DEVICE (astal_wp_device_get_type())

G_DECLARE_FINAL_TYPE(AstalWpDevice, astal_wp_device, ASTAL_WP, DEVICE, GObject)

typedef enum {
    ASTAL_WP_DEVICE_TYPE_AUDIO, /*< nick=Audio/Device >*/
    ASTAL_WP_DEVICE_TYPE_VIDEO  /*< nick=Video/Device >*/
} AstalWpDeviceType;

guint astal_wp_device_get_id(AstalWpDevice *self);
const gchar *astal_wp_device_get_description(AstalWpDevice *self);
const gchar *astal_wp_device_get_icon(AstalWpDevice *self);
AstalWpProfile *astal_wp_device_get_profile(AstalWpDevice *self, gint id);
GList *astal_wp_device_get_profiles(AstalWpDevice *self);
void astal_wp_device_set_active_profile(AstalWpDevice *self, int profile_id);
gint astal_wp_device_get_active_profile(AstalWpDevice *self);
AstalWpDeviceType astal_wp_device_get_device_type(AstalWpDevice *self);
const gchar *astal_wp_device_get_form_factor(AstalWpDevice *self);

G_END_DECLS

#endif  // !ASTAL_WP_DEVICE_H
