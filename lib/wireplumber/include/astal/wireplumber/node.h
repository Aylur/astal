#ifndef ASTAL_WP_NODE_H
#define ASTAL_WP_NODE_H

#include <glib-object.h>

G_BEGIN_DECLS

#define ASTAL_WP_TYPE_CHANNEL_VOLUME (astal_wp_channel_volume_get_type())

G_DECLARE_FINAL_TYPE(AstalWpChannelVolume, astal_wp_channel_volume, ASTAL_WP, CHANNEL_VOLUME,
                     GObject)

gdouble astal_wp_channel_volume_get_volume(AstalWpChannelVolume *self);
void astal_wp_channel_volume_set_volume(AstalWpChannelVolume *self, gdouble volume);
const gchar *astal_wp_channel_volume_get_name(AstalWpChannelVolume *self);
const gchar *astal_wp_channel_volume_get_volume_icon(AstalWpChannelVolume *self);

#define ASTAL_WP_TYPE_NODE (astal_wp_node_get_type())

G_DECLARE_DERIVABLE_TYPE(AstalWpNode, astal_wp_node, ASTAL_WP, NODE, GObject)

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

struct _AstalWpNodeClass {
    GObjectClass parent_class;
};

void astal_wp_node_set_volume(AstalWpNode *self, gdouble volume);
void astal_wp_node_set_mute(AstalWpNode *self, gboolean mute);
gboolean astal_wp_node_get_is_default(AstalWpNode *self);
void astal_wp_node_set_is_default(AstalWpNode *self, gboolean is_default);
gboolean astal_wp_node_get_lock_channels(AstalWpNode *self);
void astal_wp_node_set_lock_channels(AstalWpNode *self, gboolean lock_channels);

GList *astal_wp_node_get_channel_volumes(AstalWpNode *self);
AstalWpMediaClass astal_wp_node_get_media_class(AstalWpNode *self);
guint astal_wp_node_get_id(AstalWpNode *self);
gboolean astal_wp_node_get_mute(AstalWpNode *self);
gdouble astal_wp_node_get_volume(AstalWpNode *self);
const gchar *astal_wp_node_get_description(AstalWpNode *self);
const gchar *astal_wp_node_get_name(AstalWpNode *self);
const gchar *astal_wp_node_get_icon(AstalWpNode *self);
const gchar *astal_wp_node_get_volume_icon(AstalWpNode *self);
guint astal_wp_node_get_serial(AstalWpNode *self);
const gchar *astal_wp_node_get_path(AstalWpNode *self);

G_END_DECLS

#endif  // !ASTAL_WP_NODE_H
