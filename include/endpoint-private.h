#ifndef ASTAL_WP_ENDPOINT_PRIV_H
#define ASTAL_WP_ENDPOINT_PRIV_H

#include <glib-object.h>
#include <wp/wp.h>

#include "endpoint.h"

G_BEGIN_DECLS

AstalWpEndpoint *astal_wp_endpoint_create(WpNode *node, WpPlugin *mixer);

G_END_DECLS

#endif  // !ASTAL_WP_ENDPOINT_PRIV_H
