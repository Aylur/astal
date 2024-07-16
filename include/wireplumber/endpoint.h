#ifndef ASTAL_WP_ENDPOINT_H
#define ASTAL_WP_ENDPOINT_H

#include <glib-object.h>

G_BEGIN_DECLS

#define ASTAL_WP_TYPE_ENDPOINT (astal_wp_endpoint_get_type())

G_DECLARE_FINAL_TYPE(AstalWpEndpoint, astal_wp_endpoint, ASTAL_WP, ENDPOINT, GObject)

void astal_wp_endpoint_update_volume(AstalWpEndpoint *self);
void astal_wp_endpoint_set_volume(AstalWpEndpoint *self, gdouble volume);
void astal_wp_endpoint_set_mute(AstalWpEndpoint *self, gboolean mute);

G_END_DECLS

#endif  // !ASTAL_WP_ENDPOINT_H
