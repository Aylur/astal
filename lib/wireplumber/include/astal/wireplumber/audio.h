#ifndef ASTAL_WIREPLUMBER_AUDIO_H
#define ASTAL_WIREPLUMBER_AUDIO_H

#include <glib-object.h>

#include "device.h"
#include "endpoint.h"
#include "stream.h"

G_BEGIN_DECLS

#define ASTAL_WP_TYPE_AUDIO (astal_wp_audio_get_type())

G_DECLARE_FINAL_TYPE(AstalWpAudio, astal_wp_audio, ASTAL_WP, AUDIO, GObject)

AstalWpEndpoint *astal_wp_audio_get_speaker(AstalWpAudio *self, guint id);
AstalWpEndpoint *astal_wp_audio_get_microphone(AstalWpAudio *self, guint id);
AstalWpStream *astal_wp_audio_get_recorder(AstalWpAudio *self, guint id);
AstalWpStream *astal_wp_audio_get_stream(AstalWpAudio *self, guint id);
AstalWpNode *astal_wp_audio_get_node(AstalWpAudio *self, guint id);
AstalWpDevice *astal_wp_audio_get_device(AstalWpAudio *self, guint id);

AstalWpEndpoint *astal_wp_audio_get_default_speaker(AstalWpAudio *self);
AstalWpEndpoint *astal_wp_audio_get_default_microphone(AstalWpAudio *self);

GList *astal_wp_audio_get_microphones(AstalWpAudio *self);
GList *astal_wp_audio_get_speakers(AstalWpAudio *self);
GList *astal_wp_audio_get_recorders(AstalWpAudio *self);
GList *astal_wp_audio_get_streams(AstalWpAudio *self);
GList *astal_wp_audio_get_devices(AstalWpAudio *self);

G_END_DECLS

#endif  // !ASTAL_WIREPLUMBER_AUDIO_H
