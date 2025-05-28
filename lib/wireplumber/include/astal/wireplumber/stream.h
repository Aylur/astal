#ifndef ASTAL_WIREPLUMBER_STREAM_H
#define ASTAL_WIREPLUMBER_STREAM_H

#include <glib-object.h>

#include "endpoint.h"
#include "node.h"

G_BEGIN_DECLS

#define ASTAL_WP_TYPE_STREAM (astal_wp_stream_get_type())

G_DECLARE_FINAL_TYPE(AstalWpStream, astal_wp_stream, ASTAL_WP, STREAM, AstalWpNode)

AstalWpMediaRole astal_wp_stream_get_media_role(AstalWpStream *self);
AstalWpMediaCategory astal_wp_stream_get_media_category(AstalWpStream *self);
gint astal_wp_stream_get_target_serial(AstalWpStream *self);
void astal_wp_stream_set_target_serial(AstalWpStream *self, gint serial);
AstalWpEndpoint *astal_wp_stream_get_target_endpoint(AstalWpStream *self);
void astal_wp_stream_set_target_endpoint(AstalWpStream *self, AstalWpEndpoint *target);

G_END_DECLS

#endif  // !ASTAL_WIREPLUMBER_STREAM_H
