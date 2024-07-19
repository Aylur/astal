#include "audio.h"

#include <wp/wp.h>

#include "endpoint.h"
#include "wp.h"

struct _AstalWpAudio {
    GObject parent_instance;
};

typedef struct {
    AstalWpWp *wp;
} AstalWpAudioPrivate;

G_DEFINE_FINAL_TYPE_WITH_PRIVATE(AstalWpAudio, astal_wp_audio, G_TYPE_OBJECT);

typedef enum {
    ASTAL_WP_AUDIO_SIGNAL_CHANGED,
    ASTAL_WP_AUDIO_SIGNAL_MICROPHONE_ADDED,
    ASTAL_WP_AUDIO_SIGNAL_MICROPHONE_REMOVED,
    ASTAL_WP_AUDIO_SIGNAL_SPEAKER_ADDED,
    ASTAL_WP_AUDIO_SIGNAL_SPEAKER_REMOVED,
    ASTAL_WP_AUDIO_SIGNAL_STREAM_ADDED,
    ASTAL_WP_AUDIO_SIGNAL_STREAM_REMOVED,
    ASTAL_WP_AUDIO_SIGNAL_RECORDER_ADDED,
    ASTAL_WP_AUDIO_SIGNAL_RECORDER_REMOVED,
    ASTAL_WP_AUDIO_N_SIGNALS
} AstalWpWpSignals;

static guint astal_wp_audio_signals[ASTAL_WP_AUDIO_N_SIGNALS] = {
    0,
};

typedef enum {
    ASTAL_WP_AUDIO_PROP_MICROPHONES = 1,
    ASTAL_WP_AUDIO_PROP_SPEAKERS,
    ASTAL_WP_AUDIO_PROP_STREAMS,
    ASTAL_WP_AUDIO_PROP_RECORDERS,
    ASTAL_WP_AUDIO_PROP_DEFAULT_SPEAKER,
    ASTAL_WP_AUDIO_PROP_DEFAULT_MICROPHONE,
    ASTAL_WP_AUDIO_N_PROPERTIES,
} AstalWpAudioProperties;

static GParamSpec *astal_wp_audio_properties[ASTAL_WP_AUDIO_N_PROPERTIES] = {
    NULL,
};

/**
 * astal_wp_audio_get_speaker:
 * @self: the AstalWpAudio object
 * @id: the id of the endpoint
 *
 * Returns: (transfer none) (nullable): the speaker with the given id
 */
AstalWpEndpoint *astal_wp_audio_get_speaker(AstalWpAudio *self, guint id) {
    AstalWpAudioPrivate *priv = astal_wp_audio_get_instance_private(self);

    AstalWpEndpoint *endpoint = astal_wp_wp_get_endpoint(priv->wp, id);
    if (astal_wp_endpoint_get_media_class(endpoint) == ASTAL_WP_MEDIA_CLASS_AUDIO_SPEAKER)
        return endpoint;
    return NULL;
}

/**
 * astal_wp_audio_get_microphone:
 * @self: the AstalWpAudio object
 * @id: the id of the endpoint
 *
 * Returns: (transfer none) (nullable): the microphone with the given id
 */
AstalWpEndpoint *astal_wp_audio_get_microphone(AstalWpAudio *self, guint id) {
    AstalWpAudioPrivate *priv = astal_wp_audio_get_instance_private(self);

    AstalWpEndpoint *endpoint = astal_wp_wp_get_endpoint(priv->wp, id);
    if (astal_wp_endpoint_get_media_class(endpoint) == ASTAL_WP_MEDIA_CLASS_AUDIO_MICROPHONE)
        return endpoint;
    return NULL;
}

/**
 * astal_wp_audio_get_recorder:
 * @self: the AstalWpAudio object
 * @id: the id of the endpoint
 *
 * Returns: (transfer none) (nullable): the recorder with the given id
 */
AstalWpEndpoint *astal_wp_audio_get_recorder(AstalWpAudio *self, guint id) {
    AstalWpAudioPrivate *priv = astal_wp_audio_get_instance_private(self);

    AstalWpEndpoint *endpoint = astal_wp_wp_get_endpoint(priv->wp, id);
    if (astal_wp_endpoint_get_media_class(endpoint) == ASTAL_WP_MEDIA_CLASS_AUDIO_RECORDER)
        return endpoint;
    return NULL;
}

/**
 * astal_wp_audio_get_stream:
 * @self: the AstalWpAudio object
 * @id: the id of the endpoint
 *
 * Returns: (transfer none) (nullable): the stream with the given id
 */
AstalWpEndpoint *astal_wp_audio_get_stream(AstalWpAudio *self, guint id) {
    AstalWpAudioPrivate *priv = astal_wp_audio_get_instance_private(self);

    AstalWpEndpoint *endpoint = astal_wp_wp_get_endpoint(priv->wp, id);
    if (astal_wp_endpoint_get_media_class(endpoint) == ASTAL_WP_MEDIA_CLASS_AUDIO_STREAM)
        return endpoint;
    return NULL;
}

/**
 * astal_wp_audio_get_microphones:
 * @self: the AstalWpAudio object
 *
 * Returns: (transfer container) (nullable) (type GList(AstalWpEndpoint)): a GList containing the
 * microphones
 */
GList *astal_wp_audio_get_microphones(AstalWpAudio *self) {
    AstalWpAudioPrivate *priv = astal_wp_audio_get_instance_private(self);
    GList *eps = astal_wp_wp_get_endpoints(priv->wp);
    GList *mics = NULL;

    for (GList *l = eps; l != NULL; l = l->next) {
        if (astal_wp_endpoint_get_media_class(l->data) == ASTAL_WP_MEDIA_CLASS_AUDIO_MICROPHONE) {
            mics = g_list_append(mics, l->data);
        }
    }
    g_list_free(eps);
    return mics;
}

/**
 * astal_wp_audio_get_speakers:
 * @self: the AstalWpAudio object
 *
 * Returns: (transfer container) (nullable) (type GList(AstalWpEndpoint)): a GList containing the
 * speaker
 */
GList *astal_wp_audio_get_speakers(AstalWpAudio *self) {
    AstalWpAudioPrivate *priv = astal_wp_audio_get_instance_private(self);
    GList *eps = astal_wp_wp_get_endpoints(priv->wp);
    GList *speakers = NULL;

    for (GList *l = eps; l != NULL; l = l->next) {
        if (astal_wp_endpoint_get_media_class(l->data) == ASTAL_WP_MEDIA_CLASS_AUDIO_SPEAKER) {
            speakers = g_list_append(speakers, l->data);
        }
    }
    g_list_free(eps);
    return speakers;
}

/**
 * astal_wp_audio_get_recorders:
 * @self: the AstalWpAudio object
 *
 * Returns: (transfer container) (nullable) (type GList(AstalWpEndpoint)): a GList containing the
 * recorders
 */
GList *astal_wp_audio_get_recorders(AstalWpAudio *self) {
    AstalWpAudioPrivate *priv = astal_wp_audio_get_instance_private(self);
    GList *eps = astal_wp_wp_get_endpoints(priv->wp);
    GList *recorders = NULL;

    for (GList *l = eps; l != NULL; l = l->next) {
        if (astal_wp_endpoint_get_media_class(l->data) == ASTAL_WP_MEDIA_CLASS_AUDIO_RECORDER) {
            recorders = g_list_append(recorders, l->data);
        }
    }
    g_list_free(eps);
    return recorders;
}

/**
 * astal_wp_audio_get_streams:
 * @self: the AstalWpAudio object
 *
 * Returns: (transfer container) (nullable) (type GList(AstalWpEndpoint)): a GList containing the
 * streams
 */
GList *astal_wp_audio_get_streams(AstalWpAudio *self) {
    AstalWpAudioPrivate *priv = astal_wp_audio_get_instance_private(self);
    GList *eps = astal_wp_wp_get_endpoints(priv->wp);
    GList *streams = NULL;

    for (GList *l = eps; l != NULL; l = l->next) {
        if (astal_wp_endpoint_get_media_class(l->data) == ASTAL_WP_MEDIA_CLASS_AUDIO_STREAM) {
            streams = g_list_append(streams, l->data);
        }
    }
    g_list_free(eps);
    return streams;
}

/**
 * astal_wp_audio_get_endpoint:
 * @self: the AstalWpAudio object
 * @id: the id of the endpoint
 *
 * Returns: (transfer none) (nullable): the endpoint with the given id
 */
AstalWpEndpoint *astal_wp_audio_get_endpoint(AstalWpAudio *self, guint id) {
    AstalWpAudioPrivate *priv = astal_wp_audio_get_instance_private(self);

    AstalWpEndpoint *endpoint = astal_wp_wp_get_endpoint(priv->wp, id);
    return endpoint;
}

/**
 * astal_wp_audio_get_default_speaker
 *
 * Returns: (nullable) (transfer none): gets the default speaker object
 */
AstalWpEndpoint *astal_wp_audio_get_default_speaker(AstalWpAudio *self) {
    AstalWpAudioPrivate *priv = astal_wp_audio_get_instance_private(self);
    return astal_wp_wp_get_default_speaker(priv->wp);
}

/**
 * astal_wp_audio_get_default_microphone
 *
 * Returns: (nullable) (transfer none): gets the default microphone object
 */
AstalWpEndpoint *astal_wp_audio_get_default_microphone(AstalWpAudio *self) {
    AstalWpAudioPrivate *priv = astal_wp_audio_get_instance_private(self);
    return astal_wp_wp_get_default_microphone(priv->wp);
}

static void astal_wp_audio_get_property(GObject *object, guint property_id, GValue *value,
                                        GParamSpec *pspec) {
    AstalWpAudio *self = ASTAL_WP_AUDIO(object);

    switch (property_id) {
        case ASTAL_WP_AUDIO_PROP_MICROPHONES:
            g_value_set_pointer(value, astal_wp_audio_get_microphones(self));
            break;
        case ASTAL_WP_AUDIO_PROP_SPEAKERS:
            g_value_set_pointer(value, astal_wp_audio_get_speakers(self));
            break;
        case ASTAL_WP_AUDIO_PROP_STREAMS:
            g_value_set_pointer(value, astal_wp_audio_get_streams(self));
            break;
        case ASTAL_WP_AUDIO_PROP_RECORDERS:
            g_value_set_pointer(value, astal_wp_audio_get_recorders(self));
            break;
        case ASTAL_WP_AUDIO_PROP_DEFAULT_SPEAKER:
            g_value_set_object(value, astal_wp_audio_get_default_speaker(self));
            break;
        case ASTAL_WP_AUDIO_PROP_DEFAULT_MICROPHONE:
            g_value_set_object(value, astal_wp_audio_get_default_microphone(self));
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_wp_audio_object_added(AstalWpAudio *self, gpointer object) {
    AstalWpEndpoint *endpoint = ASTAL_WP_ENDPOINT(object);
    switch (astal_wp_endpoint_get_media_class(endpoint)) {
        case ASTAL_WP_MEDIA_CLASS_AUDIO_MICROPHONE:
            g_signal_emit_by_name(self, "microphone-added", endpoint);
            g_object_notify(G_OBJECT(self), "microphones");
            break;
        case ASTAL_WP_MEDIA_CLASS_AUDIO_SPEAKER:
            g_signal_emit_by_name(self, "speaker-added", endpoint);
            g_object_notify(G_OBJECT(self), "speakers");
            break;
        case ASTAL_WP_MEDIA_CLASS_AUDIO_STREAM:
            g_signal_emit_by_name(self, "stream-added", endpoint);
            g_object_notify(G_OBJECT(self), "streams");
            break;
        case ASTAL_WP_MEDIA_CLASS_AUDIO_RECORDER:
            g_signal_emit_by_name(self, "recorder-added", endpoint);
            g_object_notify(G_OBJECT(self), "recorders");
            break;
    }

    g_signal_emit_by_name(self, "changed");
}

static void astal_wp_audio_object_removed(AstalWpAudio *self, gpointer object) {
    AstalWpEndpoint *endpoint = ASTAL_WP_ENDPOINT(object);
    switch (astal_wp_endpoint_get_media_class(endpoint)) {
        case ASTAL_WP_MEDIA_CLASS_AUDIO_MICROPHONE:
            g_signal_emit_by_name(self, "microphone-removed", endpoint);
            g_object_notify(G_OBJECT(self), "microphones");
            break;
        case ASTAL_WP_MEDIA_CLASS_AUDIO_SPEAKER:
            g_signal_emit_by_name(self, "speaker-removed", endpoint);
            g_object_notify(G_OBJECT(self), "speakers");
            break;
        case ASTAL_WP_MEDIA_CLASS_AUDIO_STREAM:
            g_signal_emit_by_name(self, "stream-removed", endpoint);
            g_object_notify(G_OBJECT(self), "streams");
            break;
        case ASTAL_WP_MEDIA_CLASS_AUDIO_RECORDER:
            g_signal_emit_by_name(self, "recorder-removed", endpoint);
            g_object_notify(G_OBJECT(self), "recorders");
            break;
    }

    g_signal_emit_by_name(self, "changed");
}

/**
 * astal_wp_audio_get_default
 *
 * Returns: (nullable) (transfer none): gets the default audio object.
 */
AstalWpAudio *astal_wp_audio_get_default() {
    static AstalWpAudio *self = NULL;

    if (self == NULL) self = g_object_new(ASTAL_WP_TYPE_AUDIO, NULL);

    return self;
}

/**
 * astal_wp_get_default_audio
 *
 * Returns: (nullable) (transfer none): gets the default audio object.
 */
AstalWpAudio *astal_wp_get_default_audio() { return astal_wp_audio_get_default(); }

static void astal_wp_audio_dispose(GObject *object) {
    AstalWpAudio *self = ASTAL_WP_AUDIO(object);
    AstalWpAudioPrivate *priv = astal_wp_audio_get_instance_private(self);
}

static void astal_wp_audio_finalize(GObject *object) {
    AstalWpAudio *self = ASTAL_WP_AUDIO(object);
    AstalWpAudioPrivate *priv = astal_wp_audio_get_instance_private(self);
}

static void astal_wp_audio_init(AstalWpAudio *self) {
    AstalWpAudioPrivate *priv = astal_wp_audio_get_instance_private(self);

    priv->wp = astal_wp_wp_get_default();

    g_signal_connect_swapped(priv->wp, "endpoint-added", G_CALLBACK(astal_wp_audio_object_added),
                             self);
    g_signal_connect_swapped(priv->wp, "endpoint-removed",
                             G_CALLBACK(astal_wp_audio_object_removed), self);
}

static void astal_wp_audio_class_init(AstalWpAudioClass *class) {
    GObjectClass *object_class = G_OBJECT_CLASS(class);
    object_class->finalize = astal_wp_audio_finalize;
    object_class->dispose = astal_wp_audio_dispose;
    object_class->get_property = astal_wp_audio_get_property;

    /**
     * AstalWpAudio:microphones: (type GList(AstalWpEndpoint)) (transfer container)
     *
     * A list of AstalWpEndpoint objects
     */
    astal_wp_audio_properties[ASTAL_WP_AUDIO_PROP_MICROPHONES] =
        g_param_spec_pointer("microphones", "microphones", "microphones", G_PARAM_READABLE);
    /**
     * AstalWpAudio:speakers: (type GList(AstalWpEndpoint)) (transfer container)
     *
     * A list of AstalWpEndpoint objects
     */
    astal_wp_audio_properties[ASTAL_WP_AUDIO_PROP_SPEAKERS] =
        g_param_spec_pointer("speakers", "speakers", "speakers", G_PARAM_READABLE);
    /**
     * AstalWpAudio:recorders: (type GList(AstalWpEndpoint)) (transfer container)
     *
     * A list of AstalWpEndpoint objects
     */
    astal_wp_audio_properties[ASTAL_WP_AUDIO_PROP_RECORDERS] =
        g_param_spec_pointer("recorders", "recorders", "recorders", G_PARAM_READABLE);
    /**
     * AstalWpAudio:streams: (type GList(AstalWpEndpoint)) (transfer container)
     *
     * A list of AstalWpEndpoint objects
     */
    astal_wp_audio_properties[ASTAL_WP_AUDIO_PROP_STREAMS] =
        g_param_spec_pointer("streams", "streams", "streams", G_PARAM_READABLE);
    /**
     * AstalWpAudio:default-speaker:
     *
     * The AstalWndpoint object representing the default speaker
     */
    astal_wp_audio_properties[ASTAL_WP_AUDIO_PROP_DEFAULT_SPEAKER] =
        g_param_spec_object("default-speaker", "default-speaker", "default-speaker",
                            ASTAL_WP_TYPE_ENDPOINT, G_PARAM_READABLE);
    /**
     * AstalWpAudio:default-microphone:
     *
     * The AstalWndpoint object representing the default speaker
     */
    astal_wp_audio_properties[ASTAL_WP_AUDIO_PROP_DEFAULT_MICROPHONE] =
        g_param_spec_object("default-microphone", "default-microphone", "default-microphone",
                            ASTAL_WP_TYPE_ENDPOINT, G_PARAM_READABLE);

    g_object_class_install_properties(object_class, ASTAL_WP_AUDIO_N_PROPERTIES,
                                      astal_wp_audio_properties);

    astal_wp_audio_signals[ASTAL_WP_AUDIO_SIGNAL_MICROPHONE_ADDED] =
        g_signal_new("microphone-added", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL,
                     NULL, NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_ENDPOINT);
    astal_wp_audio_signals[ASTAL_WP_AUDIO_SIGNAL_MICROPHONE_REMOVED] =
        g_signal_new("microphone-removed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL,
                     NULL, NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_ENDPOINT);
    astal_wp_audio_signals[ASTAL_WP_AUDIO_SIGNAL_SPEAKER_ADDED] =
        g_signal_new("speaker-added", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_ENDPOINT);
    astal_wp_audio_signals[ASTAL_WP_AUDIO_SIGNAL_SPEAKER_REMOVED] =
        g_signal_new("speaker-removed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_ENDPOINT);
    astal_wp_audio_signals[ASTAL_WP_AUDIO_SIGNAL_STREAM_ADDED] =
        g_signal_new("stream-added", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_ENDPOINT);
    astal_wp_audio_signals[ASTAL_WP_AUDIO_SIGNAL_STREAM_REMOVED] =
        g_signal_new("stream-removed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_ENDPOINT);
    astal_wp_audio_signals[ASTAL_WP_AUDIO_SIGNAL_RECORDER_ADDED] =
        g_signal_new("recorder-added", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_ENDPOINT);
    astal_wp_audio_signals[ASTAL_WP_AUDIO_SIGNAL_RECORDER_REMOVED] =
        g_signal_new("recorder-removed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL,
                     NULL, NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_ENDPOINT);
    astal_wp_audio_signals[ASTAL_WP_AUDIO_SIGNAL_CHANGED] =
        g_signal_new("changed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL, NULL,
                     G_TYPE_NONE, 0);
}
