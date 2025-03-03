#ifndef ASTAL_WIREPLUMBER_VIDEO_H
#define ASTAL_WIREPLUMBER_VIDEO_H

#include <glib-object.h>

#include "device.h"
#include "endpoint.h"
#include "stream.h"

G_BEGIN_DECLS

#define ASTAL_WP_TYPE_VIDEO (astal_wp_video_get_type())

G_DECLARE_FINAL_TYPE(AstalWpVideo, astal_wp_video, ASTAL_WP, VIDEO, GObject)

AstalWpEndpoint *astal_wp_video_get_source(AstalWpVideo *self, guint id);
AstalWpEndpoint *astal_wp_video_get_sink(AstalWpVideo *self, guint id);
AstalWpStream *astal_wp_video_get_recorder(AstalWpVideo *self, guint id);
AstalWpStream *astal_wp_video_get_stream(AstalWpVideo *self, guint id);
AstalWpDevice *astal_wp_video_get_device(AstalWpVideo *self, guint id);

GList *astal_wp_video_get_sources(AstalWpVideo *self);
GList *astal_wp_video_get_sinks(AstalWpVideo *self);
GList *astal_wp_video_get_recorders(AstalWpVideo *self);
GList *astal_wp_video_get_streams(AstalWpVideo *self);
GList *astal_wp_video_get_devices(AstalWpVideo *self);

G_END_DECLS

#endif  // !ASTAL_WIREPLUMBER_VIDEO_H
