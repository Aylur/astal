#ifndef ASTAL_WP_NODE_H
#define ASTAL_WP_NODE_H

#include <glib-object.h>

#include "channel.h"
#include "enums.h"

G_BEGIN_DECLS

#define ASTAL_WP_TYPE_NODE (astal_wp_node_get_type())

G_DECLARE_DERIVABLE_TYPE(AstalWpNode, astal_wp_node, ASTAL_WP, NODE, GObject)

struct _AstalWpNodeClass {
    GObjectClass parent_class;

    void (*params_changed)(AstalWpNode *self, const gchar *id);

    void (*metadata_changed)(AstalWpNode *self, const gchar *key, const gchar *type,
                             const gchar *value);
};

void astal_wp_node_set_volume(AstalWpNode *self, gdouble volume);
gdouble astal_wp_node_get_volume(AstalWpNode *self);

void astal_wp_node_set_mute(AstalWpNode *self, gboolean mute);
gboolean astal_wp_node_get_mute(AstalWpNode *self);

gboolean astal_wp_node_get_lock_channels(AstalWpNode *self);
void astal_wp_node_set_lock_channels(AstalWpNode *self, gboolean lock_channels);

AstalWpMediaClass astal_wp_node_get_media_class(AstalWpNode *self);
guint astal_wp_node_get_id(AstalWpNode *self);
const gchar *astal_wp_node_get_description(AstalWpNode *self);
const gchar *astal_wp_node_get_name(AstalWpNode *self);
const gchar *astal_wp_node_get_icon(AstalWpNode *self);
const gchar *astal_wp_node_get_volume_icon(AstalWpNode *self);
gint astal_wp_node_get_serial(AstalWpNode *self);
const gchar *astal_wp_node_get_path(AstalWpNode *self);
AstalWpNodeState astal_wp_node_get_state(AstalWpNode *self);

GList *astal_wp_node_get_channels(AstalWpNode *self);

gchar *astal_wp_node_get_pw_property(AstalWpNode *self, const gchar *key);

void astal_wp_node_metadata_changed(AstalWpNode *self, const gchar *key, const gchar *type,
                                    const gchar *value);
void astal_wp_node_params_changed(AstalWpNode *self, const gchar *id);

G_END_DECLS

#endif  // !ASTAL_WP_NODE_H
