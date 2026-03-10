#include "astal-hyprland-focus-grab-3.h"
#include <gdk/gdkwayland.h>

VALA_EXTERN struct wl_surface *
astal_hyprland_focus_grab_grab_context_get_wl_surface_for_window(GtkWindow *window) {
    GdkWindow *gdk_win = gtk_widget_get_window(GTK_WIDGET(window));
    if (!gdk_win) {
        return NULL;
    }
    if (!GDK_IS_WAYLAND_WINDOW(gdk_win)) {
        g_critical("Not a Wayland surface");
        return NULL;
    }
    return gdk_wayland_window_get_wl_surface(gdk_win);
}
