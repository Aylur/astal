#ifndef ASTAL_RIVER_OUTPUT_PRIVATE_H
#define ASTAL_RIVER_OUTPUT_PRIVATE_H

#include <wayland-client.h>

#include "astal-river.h"
#include "river-control-unstable-v1-client.h"
#include "river-status-unstable-v1-client.h"

G_BEGIN_DECLS

AstalRiverOutput *astal_river_output_new(guint id, struct wl_output *wl_output,
                                         struct zriver_status_manager_v1 *status_manager,
                                         struct zriver_control_v1 *river_control,
                                         struct wl_seat *seat, struct wl_display *wl_display);

struct wl_output *astal_river_output_get_wl_output(AstalRiverOutput *self);
void astal_river_output_set_focused_view(AstalRiverOutput *self, const gchar *focused_view);

G_END_DECLS

#endif  // !ASTAL_RIVER_OUTPUT_PRIVATE_H
