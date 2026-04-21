#include "astal-hyprland-focus-grab-4.h"
#include <gtk/gtk.h>
#include <gdk/gdk.h>
#include <gdk/wayland/gdkwayland.h>

VALA_EXTERN struct wl_surface *
astal_hyprland_focus_grab_grab_context_get_wl_surface_for_window(GtkWindow *window) {
    GdkSurface *surf = gtk_native_get_surface(GTK_NATIVE(window));
    if (!surf) {
      return NULL;
    }
    if (!GDK_IS_WAYLAND_SURFACE(surf)) {
      g_critical("Not a Wayland surface");
      return NULL;
    }
    return gdk_wayland_surface_get_wl_surface(surf);
}
