#ifndef ASTAL_WIREPLUMBER_H
#define ASTAL_WIREPLUMBER_H

#include <glib-object.h>

#include "audio.h"
#include "device.h"
#include "endpoint.h"
#include "video.h"

G_BEGIN_DECLS

#define ASTAL_WP_TYPE_SCALE (astal_wp_scale_get_type())

typedef enum {
    ASTAL_WP_SCALE_LINEAR,
    ASTAL_WP_SCALE_CUBIC,
} AstalWpScale;

#define ASTAL_WP_TYPE_WP (astal_wp_wp_get_type())

G_DECLARE_FINAL_TYPE(AstalWpWp, astal_wp_wp, ASTAL_WP, WP, GObject)

AstalWpWp* astal_wp_wp_get_default();
AstalWpWp* astal_wp_get_default();

AstalWpAudio* astal_wp_wp_get_audio(AstalWpWp* self);
AstalWpVideo* astal_wp_wp_get_video(AstalWpWp* self);

AstalWpEndpoint* astal_wp_wp_get_endpoint(AstalWpWp* self, guint id);
GList* astal_wp_wp_get_endpoints(AstalWpWp* self);

AstalWpDevice* astal_wp_wp_get_device(AstalWpWp* self, guint id);
GList* astal_wp_wp_get_devices(AstalWpWp* self);

AstalWpEndpoint* astal_wp_wp_get_default_speaker(AstalWpWp* self);
AstalWpEndpoint* astal_wp_wp_get_default_microphone(AstalWpWp* self);

AstalWpScale astal_wp_wp_get_scale(AstalWpWp* self);
void astal_wp_wp_set_scale(AstalWpWp* self, AstalWpScale scale);

AstalWpVideo* astal_wp_video_new(AstalWpWp* wp);
AstalWpAudio* astal_wp_audio_new(AstalWpWp* wp);

G_END_DECLS

#endif  // !ASTAL_WIREPLUMBER_H
