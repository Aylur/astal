#ifndef ASTAL_WP_WP_PRIV_H
#define ASTAL_WP_WP_PRIV_H

#include <glib-object.h>

#include "wp.h"

G_BEGIN_DECLS

void astal_wp_wp_set_matadata(AstalWpWp* self, guint subject, const gchar* key, const gchar* type,
                              const gchar* value);

G_END_DECLS

#endif  // !ASTAL_WP_WP_PRIV_H
