#ifndef ASTAL_WP_ENDPOINT_H
#define ASTAL_WP_ENDPOINT_H

#include <glib-object.h>

G_BEGIN_DECLS

#define ASTAL_WP_TYPE_ENDPOINT (astal_wp_endpoint_get_type())

G_DECLARE_FINAL_TYPE(AstalWpEndpoint, astal_wp_endpoint, ASTAL_WP, ENDPOINT, GObject)

#define ASTAL_WP_TYPE_MEDIA_CLASS (astal_wp_media_class_get_type())

typedef enum {
    ASTAL_WP_MEDIA_CLASS_AUDIO_MICROPHONE,
    ASTAL_WP_MEDIA_CLASS_AUDIO_SPEAKER,
    ASTAL_WP_MEDIA_CLASS_AUDIO_RECORDER,
    ASTAL_WP_MEDIA_CLASS_AUDIO_STREAM,
    ASTAL_WP_MEDIA_CLASS_VIDEO_SOURCE,
    ASTAL_WP_MEDIA_CLASS_VIDEO_SINK,
    ASTAL_WP_MEDIA_CLASS_VIDEO_RECORDER,
    ASTAL_WP_MEDIA_CLASS_VIDEO_STREAM,
} AstalWpMediaClass;

void astal_wp_endpoint_set_volume(AstalWpEndpoint *self, gdouble volume);
void astal_wp_endpoint_set_mute(AstalWpEndpoint *self, gboolean mute);
gboolean astal_wp_endpoint_get_is_default(AstalWpEndpoint *self);
void astal_wp_endpoint_set_is_default(AstalWpEndpoint *self, gboolean is_default);
gboolean astal_wp_endpoint_get_lock_channels(AstalWpEndpoint *self);
void astal_wp_endpoint_set_lock_channels(AstalWpEndpoint *self, gboolean lock_channels);

AstalWpMediaClass astal_wp_endpoint_get_media_class(AstalWpEndpoint *self);
guint astal_wp_endpoint_get_id(AstalWpEndpoint *self);
gboolean astal_wp_endpoint_get_mute(AstalWpEndpoint *self);
gdouble astal_wp_endpoint_get_volume(AstalWpEndpoint *self);
const gchar *astal_wp_endpoint_get_description(AstalWpEndpoint *self);
const gchar *astal_wp_endpoint_get_name(AstalWpEndpoint *self);
const gchar *astal_wp_endpoint_get_icon(AstalWpEndpoint *self);
const gchar *astal_wp_endpoint_get_volume_icon(AstalWpEndpoint *self);

G_END_DECLS

#endif  // !ASTAL_WP_ENDPOINT_H
