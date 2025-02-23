#ifndef ASTAL_WIREPLUMBER_ENDPOINT_H
#define ASTAL_WIREPLUMBER_ENDPOINT_H

#include <glib-object.h>

#include "node.h"

G_BEGIN_DECLS

#define ASTAL_WP_TYPE_ENDPOINT (astal_wp_endpoint_get_type())

G_DECLARE_FINAL_TYPE(AstalWpEndpoint, astal_wp_endpoint, ASTAL_WP, ENDPOINT, AstalWpNode)

G_END_DECLS

#endif  // !ASTAL_WIREPLUMBER_ENDPOINT_H
