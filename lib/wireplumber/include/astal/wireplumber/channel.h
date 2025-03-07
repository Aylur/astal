#ifndef ASTAL_WP_CHANNEL_H
#define ASTAL_WP_CHANNEL_H

#include <glib-object.h>

G_BEGIN_DECLS

#define ASTAL_WP_TYPE_CHANNEL (astal_wp_channel_get_type())

G_DECLARE_FINAL_TYPE(AstalWpChannel, astal_wp_channel, ASTAL_WP, CHANNEL, GObject)

gdouble astal_wp_channel_get_volume(AstalWpChannel *self);
void astal_wp_channel_set_volume(AstalWpChannel *self, gdouble volume);
const gchar *astal_wp_channel_get_name(AstalWpChannel *self);
const gchar *astal_wp_channel_get_volume_icon(AstalWpChannel *self);

G_END_DECLS

#endif  // !ASTAL_WP_CHANNEL_H
