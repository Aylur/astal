#ifndef ASTAL_WIREPLUMBER_STREAM_H
#define ASTAL_WIREPLUMBER_STREAM_H

#include <glib-object.h>

#include "node.h"

G_BEGIN_DECLS

#define ASTAL_WP_TYPE_STREAM (astal_wp_stream_get_type())

G_DECLARE_FINAL_TYPE(AstalWpStream, astal_wp_stream, ASTAL_WP, STREAM, AstalWpNode)

G_END_DECLS

#endif  // !ASTAL_WIREPLUMBER_STREAM_H
