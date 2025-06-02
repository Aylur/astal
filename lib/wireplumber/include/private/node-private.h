#ifndef ASTAL_WP_NODE_PRIV_H
#define ASTAL_WP_NODE_PRIV_H

#include <glib-object.h>
#include <wp/wp.h>

#include "endpoint.h"
#include "node.h"
#include "stream.h"
#include "wp.h"

G_BEGIN_DECLS

AstalWpStream *astal_wp_stream_new(WpNode *node, WpPlugin *mixer, AstalWpWp *wp);

AstalWpEndpoint *astal_wp_endpoint_new(WpNode *node, WpPlugin *mixer, WpPlugin *defaults,
                                       AstalWpWp *wp);
AstalWpEndpoint *astal_wp_endpoint_new_default(AstalWpWp *wp);

void astal_wp_endpoint_init_as_default(AstalWpEndpoint *self, WpPlugin *mixer, WpPlugin *defaults,
                                       AstalWpMediaClass type);

void astal_wp_node_update_default(AstalWpNode *self, gboolean is_default);
void astal_wp_node_update_volume(AstalWpNode *self);
void astal_wp_node_set_channel_volume(AstalWpNode *self, const gchar *name, gdouble volume);
void astal_wp_node_set_icon(AstalWpNode *self, const gchar *icon);
void astal_wp_node_set_node(AstalWpNode *self, WpNode *node);
void astal_wp_node_set_mixer(AstalWpNode *self, WpPlugin *mixer);
void astal_wp_node_set_type(AstalWpNode *self, AstalWpMediaClass type);

void astal_wp_node_properties_changed(AstalWpNode *self);
G_END_DECLS

#endif  // !ASTAL_WP_NODE_PRIV_H
