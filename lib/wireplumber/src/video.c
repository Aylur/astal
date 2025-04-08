#include "video.h"

#include <wp/wp.h>

#include "device.h"
#include "endpoint.h"
#include "glib-object.h"
#include "wp.h"

struct _AstalWpVideo {
    GObject parent_instance;
};

typedef struct {
    AstalWpWp *wp;
} AstalWpVideoPrivate;

G_DEFINE_FINAL_TYPE_WITH_PRIVATE(AstalWpVideo, astal_wp_video, G_TYPE_OBJECT);

typedef enum {
    ASTAL_WP_VIDEO_SIGNAL_SOURCE_ADDED,
    ASTAL_WP_VIDEO_SIGNAL_SOURCE_REMOVED,
    ASTAL_WP_VIDEO_SIGNAL_SINK_ADDED,
    ASTAL_WP_VIDEO_SIGNAL_SINK_REMOVED,
    ASTAL_WP_VIDEO_SIGNAL_STREAM_ADDED,
    ASTAL_WP_VIDEO_SIGNAL_STREAM_REMOVED,
    ASTAL_WP_VIDEO_SIGNAL_RECORDER_ADDED,
    ASTAL_WP_VIDEO_SIGNAL_RECORDER_REMOVED,
    ASTAL_WP_VIDEO_SIGNAL_DEVICE_ADDED,
    ASTAL_WP_VIDEO_SIGNAL_DEVICE_REMOVED,
    ASTAL_WP_VIDEO_N_SIGNALS
} AstalWpWpSignals;

static guint astal_wp_video_signals[ASTAL_WP_VIDEO_N_SIGNALS] = {
    0,
};

typedef enum {
    ASTAL_WP_VIDEO_PROP_SOURCE = 1,
    ASTAL_WP_VIDEO_PROP_SINK,
    ASTAL_WP_VIDEO_PROP_STREAMS,
    ASTAL_WP_VIDEO_PROP_RECORDERS,
    ASTAL_WP_VIDEO_PROP_DEVICES,
    ASTAL_WP_VIDEO_N_PROPERTIES,
} AstalWpVideoProperties;

static GParamSpec *astal_wp_video_properties[ASTAL_WP_VIDEO_N_PROPERTIES] = {
    NULL,
};

/**
 *  AstalWpVideo
 *
 *  is instanciated by [class@AstalWp.Wp]. An instance of class can only be received there.
 *
 *  This is a convinience class and acts as a filter for [class@AstalWp.Wp] to filter for video
 * endpoints and devices.
 */

/**
 * astal_wp_video_get_source:
 * @self: the AstalWpVideo object
 * @id: the id of the endpoint
 *
 * the source with the given id
 *
 * Returns: (transfer none) (nullable): the source with the given id
 */
AstalWpEndpoint *astal_wp_video_get_source(AstalWpVideo *self, guint id) {
    AstalWpVideoPrivate *priv = astal_wp_video_get_instance_private(self);

    AstalWpNode *node = astal_wp_wp_get_node(priv->wp, id);
    if (astal_wp_node_get_media_class(node) == ASTAL_WP_MEDIA_CLASS_VIDEO_SOURCE)
        return ASTAL_WP_ENDPOINT(node);
    return NULL;
}

/**
 * astal_wp_video_get_sink:
 * @self: the AstalWpVideo object
 * @id: the id of the endpoint
 *
 * the sink with the given id
 *
 * Returns: (transfer none) (nullable): the sink with the given id
 */
AstalWpEndpoint *astal_wp_video_get_sink(AstalWpVideo *self, guint id) {
    AstalWpVideoPrivate *priv = astal_wp_video_get_instance_private(self);

    AstalWpNode *node = astal_wp_wp_get_node(priv->wp, id);
    if (astal_wp_node_get_media_class(node) == ASTAL_WP_MEDIA_CLASS_VIDEO_SINK)
        return ASTAL_WP_ENDPOINT(node);
    return NULL;
}

/**
 * astal_wp_video_get_stream:
 * @self: the AstalWpVideo object
 * @id: the id of the endpoint
 *
 * the stream with the given id
 *
 * Returns: (transfer none) (nullable): the stream with the given id
 */
AstalWpStream *astal_wp_video_get_stream(AstalWpVideo *self, guint id) {
    AstalWpVideoPrivate *priv = astal_wp_video_get_instance_private(self);

    AstalWpNode *node = astal_wp_wp_get_node(priv->wp, id);
    if (astal_wp_node_get_media_class(node) == ASTAL_WP_MEDIA_CLASS_VIDEO_STREAM)
        return ASTAL_WP_STREAM(node);
    return NULL;
}

/**
 * astal_wp_video_get_recorder:
 * @self: the AstalWpVideo object
 * @id: the id of the endpoint
 *
 * the recorder with the given id
 *
 * Returns: (transfer none) (nullable): the recorder with the given id
 */
AstalWpStream *astal_wp_video_get_recorder(AstalWpVideo *self, guint id) {
    AstalWpVideoPrivate *priv = astal_wp_video_get_instance_private(self);

    AstalWpNode *node = astal_wp_wp_get_node(priv->wp, id);
    if (astal_wp_node_get_media_class(node) == ASTAL_WP_MEDIA_CLASS_VIDEO_RECORDER)
        return ASTAL_WP_STREAM(node);
    return NULL;
}

/**
 * astal_wp_video_get_device:
 * @self: the AstalWpVideo object
 * @id: the id of the device
 *
 * the device with the given id
 *
 * Returns: (transfer none) (nullable): the device with the given id
 */
AstalWpDevice *astal_wp_video_get_device(AstalWpVideo *self, guint id) {
    AstalWpVideoPrivate *priv = astal_wp_video_get_instance_private(self);

    AstalWpDevice *device = astal_wp_wp_get_device(priv->wp, id);
    if (astal_wp_device_get_device_type(device) == ASTAL_WP_DEVICE_TYPE_VIDEO) return device;
    return NULL;
}

/**
 * astal_wp_video_get_sources:
 * @self: the AstalWpVideo object
 *
 * a list containing the video sources
 *
 * Returns: (transfer container) (nullable) (type GList(AstalWpEndpoint)): a GList containing the
 * video sources
 */
GList *astal_wp_video_get_sources(AstalWpVideo *self) {
    AstalWpVideoPrivate *priv = astal_wp_video_get_instance_private(self);
    GList *nodes = astal_wp_wp_get_nodes(priv->wp);
    GList *list = NULL;

    for (GList *l = nodes; l != NULL; l = l->next) {
        if (astal_wp_node_get_media_class(l->data) == ASTAL_WP_MEDIA_CLASS_VIDEO_SOURCE) {
            list = g_list_append(list, ASTAL_WP_ENDPOINT(l->data));
        }
    }
    g_list_free(nodes);
    return list;
}

/**
 * astal_wp_video_get_sinks
 * @self: the AstalWpVideo object
 *
 * a list containing the video sinks
 *
 * Returns: (transfer container) (nullable) (type GList(AstalWpEndpoint)): a GList containing the
 * video sinks
 */
GList *astal_wp_video_get_sinks(AstalWpVideo *self) {
    AstalWpVideoPrivate *priv = astal_wp_video_get_instance_private(self);
    GList *nodes = astal_wp_wp_get_nodes(priv->wp);
    GList *list = NULL;

    for (GList *l = nodes; l != NULL; l = l->next) {
        if (astal_wp_node_get_media_class(l->data) == ASTAL_WP_MEDIA_CLASS_VIDEO_SINK) {
            list = g_list_append(list, ASTAL_WP_ENDPOINT(l->data));
        }
    }
    g_list_free(nodes);
    return list;
}

/**
 * astal_wp_video_get_recorders:
 * @self: the AstalWpVideo object
 *
 * a list containing the video recorders
 *
 * Returns: (transfer container) (nullable) (type GList(AstalWpStream)): a GList containing the
 * video recorders
 */
GList *astal_wp_video_get_recorders(AstalWpVideo *self) {
    AstalWpVideoPrivate *priv = astal_wp_video_get_instance_private(self);
    GList *nodes = astal_wp_wp_get_nodes(priv->wp);
    GList *list = NULL;

    for (GList *l = nodes; l != NULL; l = l->next) {
        if (astal_wp_node_get_media_class(l->data) == ASTAL_WP_MEDIA_CLASS_VIDEO_RECORDER) {
            list = g_list_append(list, ASTAL_WP_STREAM(l->data));
        }
    }
    g_list_free(nodes);
    return list;
}

/**
 * astal_wp_video_get_streams:
 * @self: the AstalWpVideo object
 *
 * a list containing the video streams
 *
 * Returns: (transfer container) (nullable) (type GList(AstalWpStream)): a GList containing the
 * video streams
 */
GList *astal_wp_video_get_streams(AstalWpVideo *self) {
    AstalWpVideoPrivate *priv = astal_wp_video_get_instance_private(self);
    GList *nodes = astal_wp_wp_get_nodes(priv->wp);
    GList *list = NULL;

    for (GList *l = nodes; l != NULL; l = l->next) {
        if (astal_wp_node_get_media_class(l->data) == ASTAL_WP_MEDIA_CLASS_VIDEO_STREAM) {
            list = g_list_append(list, ASTAL_WP_STREAM(l->data));
        }
    }
    g_list_free(nodes);
    return list;
}

/**
 * astal_wp_video_get_devices:
 * @self: the AstalWpAudio object
 *
 * a list containing the devices
 *
 * Returns: (transfer container) (nullable) (type GList(AstalWpDevice)): a GList containing the
 * devices
 */
GList *astal_wp_video_get_devices(AstalWpVideo *self) {
    AstalWpVideoPrivate *priv = astal_wp_video_get_instance_private(self);
    GList *eps = astal_wp_wp_get_devices(priv->wp);
    GList *list = NULL;

    for (GList *l = eps; l != NULL; l = l->next) {
        if (astal_wp_device_get_device_type(l->data) == ASTAL_WP_DEVICE_TYPE_VIDEO) {
            list = g_list_append(list, l->data);
        }
    }
    g_list_free(eps);
    return list;
}

static void astal_wp_video_get_property(GObject *object, guint property_id, GValue *value,
                                        GParamSpec *pspec) {
    AstalWpVideo *self = ASTAL_WP_VIDEO(object);

    switch (property_id) {
        case ASTAL_WP_VIDEO_PROP_SOURCE:
            g_value_set_pointer(value, astal_wp_video_get_sources(self));
            break;
        case ASTAL_WP_VIDEO_PROP_SINK:
            g_value_set_pointer(value, astal_wp_video_get_sinks(self));
            break;
        case ASTAL_WP_VIDEO_PROP_RECORDERS:
            g_value_set_pointer(value, astal_wp_video_get_recorders(self));
            break;
        case ASTAL_WP_VIDEO_PROP_STREAMS:
            g_value_set_pointer(value, astal_wp_video_get_streams(self));
            break;
        case ASTAL_WP_VIDEO_PROP_DEVICES:
            g_value_set_pointer(value, astal_wp_video_get_devices(self));
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

void astal_wp_video_device_added(AstalWpVideo *self, gpointer object) {
    AstalWpDevice *device = ASTAL_WP_DEVICE(object);
    if (astal_wp_device_get_device_type(device) == ASTAL_WP_DEVICE_TYPE_VIDEO) {
        g_signal_emit_by_name(self, "device-added", device);
        g_object_notify(G_OBJECT(self), "devices");
    }
}

static void astal_wp_video_device_removed(AstalWpVideo *self, gpointer object) {
    AstalWpDevice *device = ASTAL_WP_DEVICE(object);
    if (astal_wp_device_get_device_type(device) == ASTAL_WP_DEVICE_TYPE_VIDEO) {
        g_signal_emit_by_name(self, "device-removed", device);
        g_object_notify(G_OBJECT(self), "devices");
    }
}

static void astal_wp_video_object_added(AstalWpVideo *self, gpointer object) {
    AstalWpNode *node = ASTAL_WP_NODE(object);
    switch (astal_wp_node_get_media_class(node)) {
        case ASTAL_WP_MEDIA_CLASS_VIDEO_SOURCE:
            g_signal_emit_by_name(self, "source-added", node);
            g_object_notify(G_OBJECT(self), "sources");
            break;
        case ASTAL_WP_MEDIA_CLASS_VIDEO_SINK:
            g_signal_emit_by_name(self, "sink-added", node);
            g_object_notify(G_OBJECT(self), "sinks");
            break;
        case ASTAL_WP_MEDIA_CLASS_VIDEO_STREAM:
            g_signal_emit_by_name(self, "stream-added", node);
            g_object_notify(G_OBJECT(self), "streams");
            break;
        case ASTAL_WP_MEDIA_CLASS_VIDEO_RECORDER:
            g_signal_emit_by_name(self, "recorder-added", node);
            g_object_notify(G_OBJECT(self), "recorders");
            break;
        default:
            break;
    }
}

static void astal_wp_video_object_removed(AstalWpAudio *self, gpointer object) {
    AstalWpNode *node = ASTAL_WP_NODE(object);
    switch (astal_wp_node_get_media_class(node)) {
        case ASTAL_WP_MEDIA_CLASS_VIDEO_SOURCE:
            g_signal_emit_by_name(self, "source-removed", node);
            g_object_notify(G_OBJECT(self), "sources");
            break;
        case ASTAL_WP_MEDIA_CLASS_VIDEO_SINK:
            g_signal_emit_by_name(self, "sink-removed", node);
            g_object_notify(G_OBJECT(self), "sinks");
            break;
        case ASTAL_WP_MEDIA_CLASS_VIDEO_STREAM:
            g_signal_emit_by_name(self, "stream-removed", node);
            g_object_notify(G_OBJECT(self), "streams");
            break;
        case ASTAL_WP_MEDIA_CLASS_VIDEO_RECORDER:
            g_signal_emit_by_name(self, "recorder-removed", node);
            g_object_notify(G_OBJECT(self), "recorders");
            break;
        default:
            break;
    }
}

AstalWpVideo *astal_wp_video_new(AstalWpWp *wp) {
    AstalWpVideo *self = g_object_new(ASTAL_WP_TYPE_VIDEO, NULL);
    AstalWpVideoPrivate *priv = astal_wp_video_get_instance_private(self);
    priv->wp = g_object_ref(wp);
    g_signal_connect_swapped(priv->wp, "node-added", G_CALLBACK(astal_wp_video_object_added), self);
    g_signal_connect_swapped(priv->wp, "node-removed", G_CALLBACK(astal_wp_video_object_removed),
                             self);
    g_signal_connect_swapped(priv->wp, "device-added", G_CALLBACK(astal_wp_video_device_added),
                             self);
    g_signal_connect_swapped(priv->wp, "device-removed", G_CALLBACK(astal_wp_video_device_removed),
                             self);

    return self;
}

static void astal_wp_video_dispose(GObject *object) {
    AstalWpVideo *self = ASTAL_WP_VIDEO(object);
    AstalWpVideoPrivate *priv = astal_wp_video_get_instance_private(self);
    g_clear_object(&priv->wp);

    G_OBJECT_CLASS(astal_wp_video_parent_class)->dispose(object);
}

static void astal_wp_video_init(AstalWpVideo *self) {
    // AstalWpVideoPrivate *priv = astal_wp_video_get_instance_private(self);
}

static void astal_wp_video_class_init(AstalWpVideoClass *class) {
    GObjectClass *object_class = G_OBJECT_CLASS(class);
    object_class->get_property = astal_wp_video_get_property;
    object_class->dispose = astal_wp_video_dispose;

    /**
     * AstalWpVideo:sources: (type GList(AstalWpEndpoint)) (transfer container)
     *
     * A list of AstalWpEndpoint objects
     */
    astal_wp_video_properties[ASTAL_WP_VIDEO_PROP_SOURCE] =
        g_param_spec_pointer("sources", "sources", "sources", G_PARAM_READABLE);

    /**
     * AstalWpVideo:sinks: (type GList(AstalWpEndpoint)) (transfer container)
     *
     * A list of AstalWpEndpoint objects
     */
    astal_wp_video_properties[ASTAL_WP_VIDEO_PROP_SINK] =
        g_param_spec_pointer("sinks", "sinks", "sinks", G_PARAM_READABLE);

    /**
     * AstalWpVideo:recorders: (type GList(AstalWpEndpoint)) (transfer container)
     *
     * A list of AstalWpEndpoint objects
     */
    astal_wp_video_properties[ASTAL_WP_VIDEO_PROP_RECORDERS] =
        g_param_spec_pointer("recorders", "recorders", "recorders", G_PARAM_READABLE);

    /**
     * AstalWpVideo:streams: (type GList(AstalWpEndpoint)) (transfer container)
     *
     * A list of AstalWpEndpoint objects
     */
    astal_wp_video_properties[ASTAL_WP_VIDEO_PROP_STREAMS] =
        g_param_spec_pointer("streams", "streams", "streams", G_PARAM_READABLE);

    /**
     * AstalWpVideo:devices: (type GList(AstalWpEndpoint)) (transfer container)
     *
     * A list of AstalWpEndpoint objects
     */
    astal_wp_video_properties[ASTAL_WP_VIDEO_PROP_DEVICES] =
        g_param_spec_pointer("devices", "devices", "devices", G_PARAM_READABLE);

    g_object_class_install_properties(object_class, ASTAL_WP_VIDEO_N_PROPERTIES,
                                      astal_wp_video_properties);

    astal_wp_video_signals[ASTAL_WP_VIDEO_SIGNAL_SOURCE_ADDED] =
        g_signal_new("source-added", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_ENDPOINT);
    astal_wp_video_signals[ASTAL_WP_VIDEO_SIGNAL_SOURCE_REMOVED] =
        g_signal_new("source-removed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_ENDPOINT);
    astal_wp_video_signals[ASTAL_WP_VIDEO_SIGNAL_SINK_ADDED] =
        g_signal_new("sink-added", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_ENDPOINT);
    astal_wp_video_signals[ASTAL_WP_VIDEO_SIGNAL_SINK_REMOVED] =
        g_signal_new("sink-removed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_ENDPOINT);
    astal_wp_video_signals[ASTAL_WP_VIDEO_SIGNAL_STREAM_ADDED] =
        g_signal_new("stream-added", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_STREAM);
    astal_wp_video_signals[ASTAL_WP_VIDEO_SIGNAL_SOURCE_REMOVED] =
        g_signal_new("stream-removed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_STREAM);
    astal_wp_video_signals[ASTAL_WP_VIDEO_SIGNAL_RECORDER_ADDED] =
        g_signal_new("recorder-added", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_STREAM);
    astal_wp_video_signals[ASTAL_WP_VIDEO_SIGNAL_RECORDER_REMOVED] =
        g_signal_new("recorder-removed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL,
                     NULL, NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_STREAM);
    astal_wp_video_signals[ASTAL_WP_VIDEO_SIGNAL_DEVICE_ADDED] =
        g_signal_new("device-added", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_DEVICE);
    astal_wp_video_signals[ASTAL_WP_VIDEO_SIGNAL_DEVICE_REMOVED] =
        g_signal_new("device-removed", G_TYPE_FROM_CLASS(class), G_SIGNAL_RUN_FIRST, 0, NULL, NULL,
                     NULL, G_TYPE_NONE, 1, ASTAL_WP_TYPE_DEVICE);
}
