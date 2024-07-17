#ifndef ASTAL_RIVER_OUTPUT_PRIVATE_H
#define ASTAL_RIVER_OUTPUT_PRIVATE_H

#include <wayland-client.h>

#include "river-status-unstable-v1-client.h"
#include "astal-river.h"

G_BEGIN_DECLS

AstalRiverOutput *astal_river_output_new(guint id, struct wl_output *wl_output,
                                         struct zriver_status_manager_v1 *status_manager,
                                         struct wl_display *wl_display);

struct wl_output *astal_river_output_get_wl_output(AstalRiverOutput *self);
void astal_river_output_set_focused_view(AstalRiverOutput *self, gchar *focused_view);

G_END_DECLS

#endif  // !ASTAL_RIVER_OUTPUT_PRIVATE_H
