
#ifndef ASTAL_WP_CHANNEL_PRIV_H
#define ASTAL_WP_CHANNEL_PRIV_H

#include <glib-object.h>

#include "channel.h"
#include "node.h"

G_BEGIN_DECLS

void astal_wp_channel_update_volume(AstalWpChannel *self, gdouble volume);
AstalWpChannel *astal_wp_channel_new(AstalWpNode *ep, const gchar *name);

#endif  // #ASTAL_WP_CHANNEL_PRIV_H
