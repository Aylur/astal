#ifndef ASTAL_IDLE_INHIBITOR_H
#define ASTAL_IDLE_INHIBITOR_H

#include <glib-object.h>
#include <gtk/gtk.h>
#include "idle-inhibit-unstable-v1-client.h"

G_BEGIN_DECLS

#define ASTAL_TYPE_INHIBIT_MANAGER (astal_inhibit_manager_get_type())

G_DECLARE_FINAL_TYPE(AstalInhibitManager, astal_inhibit_manager, ASTAL, INHIBIT_MANAGER, GObject)

typedef struct zwp_idle_inhibitor_v1 AstalInhibitor;

AstalInhibitManager* astal_inhibit_manager_get_default();
AstalInhibitor* astal_inhibit_manager_inhibit(AstalInhibitManager *self, GtkWindow *window);

G_END_DECLS

#endif  // !ASTAL_IDLE_INHIBITOR_H
