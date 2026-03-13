#ifndef ASTAL_WIREPLUMBER_ENUMS_H
#define ASTAL_WIREPLUMBER_ENUMS_H

#include <glib.h>

typedef enum {
    ASTAL_WP_SCALE_LINEAR,
    ASTAL_WP_SCALE_CUBIC,
} AstalWpScale;

typedef enum {
    ASTAL_WP_DEVICE_TYPE_UNKNOWN, /*< nick=Unknown >*/
    ASTAL_WP_DEVICE_TYPE_AUDIO,   /*< nick=Audio-Device >*/
    ASTAL_WP_DEVICE_TYPE_VIDEO    /*< nick=Video-Device >*/
} AstalWpDeviceType;

AstalWpDeviceType astal_wp_device_type_from_string(const gchar* string);
gchar* astal_wp_device_type_to_string(AstalWpDeviceType device_type);

typedef enum {
    ASTAL_WP_AVAILABLE_UNKNOWN, /*< nick=unknown >*/
    ASTAL_WP_AVAILABLE_NO,      /*< nick=no >*/
    ASTAL_WP_AVAILABLE_YES,     /*< nick=yes >*/
} AstalWpAvailable;

typedef enum {
    ASTAL_WP_MEDIA_CLASS_UNKNOWN,               /*< nick=Unknown >*/
    ASTAL_WP_MEDIA_CLASS_AUDIO_MICROPHONE,      /*< nick=Audio-Source >*/
    ASTAL_WP_MEDIA_CLASS_AUDIO_SPEAKER,         /*< nick=Audio-Sink >*/
    ASTAL_WP_MEDIA_CLASS_AUDIO_RECORDER,        /*< nick=Stream-Input-Audio >*/
    ASTAL_WP_MEDIA_CLASS_AUDIO_STREAM,          /*< nick=Stream-Output-Audio >*/
    ASTAL_WP_MEDIA_CLASS_VIDEO_SOURCE,          /*< nick=Video-Source >*/
    ASTAL_WP_MEDIA_CLASS_VIDEO_SINK,            /*< nick=Video-Sink >*/
    ASTAL_WP_MEDIA_CLASS_VIDEO_RECORDER,        /*< nick=Stream-Input-Video >*/
    ASTAL_WP_MEDIA_CLASS_VIDEO_STREAM,          /*< nick=Stream-Output-Video >*/
    ASTAL_WP_MEDIA_CLASS_AUDIO_SOURCE_VIRTUAL,  /*< nick=Audio-Source-Virtual >*/
} AstalWpMediaClass;

AstalWpMediaClass astal_wp_media_class_from_string(const gchar* string);
gchar* astal_wp_media_class_to_string(AstalWpMediaClass media_class);

typedef enum {
    ASTAL_WP_NODE_STATE_ERROR = -1,
    ASTAL_WP_NODE_STATE_CREATING = 0,
    ASTAL_WP_NODE_STATE_SUSPENDED = 1,
    ASTAL_WP_NODE_STATE_IDLE = 2,
    ASTAL_WP_NODE_STATE_RUNNING = 3
} AstalWpNodeState;

typedef enum {
    ASTAL_WP_MEDIA_CATEGORY_UNKNOWN,  /*< nick=Unknown >*/
    ASTAL_WP_MEDIA_CATEGORY_PLAYBACK, /*< nick=Playback >*/
    ASTAL_WP_MEDIA_CATEGORY_CAPTURE,  /*< nick=Capture >*/
    ASTAL_WP_MEDIA_CATEGORY_DUPLEX,   /*< nick=Duplex >*/
    ASTAL_WP_MEDIA_CATEGORY_MONITOR,  /*< nick=Monitor >*/
    ASTAL_WP_MEDIA_CATEGORY_MANAGER,  /*< nick=Manager >*/
} AstalWpMediaCategory;

AstalWpMediaCategory astal_wp_media_category_from_string(const gchar* string);
gchar* astal_wp_media_category_to_string(AstalWpMediaCategory category);

typedef enum {
    ASTAL_WP_MEDIA_ROLE_UNKNOWN,       /*< nick=Unknown >*/
    ASTAL_WP_MEDIA_ROLE_MOVIE,         /*< nick=Movie >*/
    ASTAL_WP_MEDIA_ROLE_MUSIC,         /*< nick=Music >*/
    ASTAL_WP_MEDIA_ROLE_CAMERA,        /*< nick=Camera >*/
    ASTAL_WP_MEDIA_ROLE_SCREEN,        /*< nick=Screen >*/
    ASTAL_WP_MEDIA_ROLE_COMMUNICATION, /*< nick=Communication >*/
    ASTAL_WP_MEDIA_ROLE_GAME,          /*< nick=Game >*/
    ASTAL_WP_MEDIA_ROLE_NOTIFICATION,  /*< nick=Notification >*/
    ASTAL_WP_MEDIA_ROLE_DSP,           /*< nick=DSP >*/
    ASTAL_WP_MEDIA_ROLE_PRODUCTION,    /*< nick=Production >*/
    ASTAL_WP_MEDIA_ROLE_ACCESSIBILITY, /*< nick=Accessibility >*/
    ASTAL_WP_MEDIA_ROLE_TEST,          /*< nick=Test >*/
} AstalWpMediaRole;

AstalWpMediaRole astal_wp_media_role_from_string(const gchar* string);
gchar* astal_wp_media_role_to_string(AstalWpMediaRole role);

typedef enum {
    ASTAL_WP_DIRECTION_INPUT, /*< nick=Input >*/
    ASTAL_WP_DIRECTION_OUTPUT /*< nick=Output >*/
} AstalWpDirection;

#endif  // ASTAL_WIREPLUMBER_ENUMS_H
