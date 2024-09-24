#include <gio/gio.h>
#include <wayland-client-protocol.h>
#include <wayland-client.h>
#include <gtk/gtk.h>
#include <gdk/gdkwayland.h>

#include "idle-inhibit.h"
#include "gdk/gdk.h"
#include "glib-object.h"
#include "glib.h"
#include "idle-inhibit-unstable-v1-client.h"


struct _AstalInhibitManager {
    GObject parent_instance;
};

typedef struct {
    gboolean init;
    struct wl_registry* wl_registry;
    struct wl_display* display;
    struct zwp_idle_inhibit_manager_v1 *idle_inhibit_manager;
} AstalInhibitManagerPrivate;


G_DEFINE_TYPE_WITH_PRIVATE(AstalInhibitManager, astal_inhibit_manager, G_TYPE_OBJECT)

void astal_inhibitor_free(AstalInhibitor* inhibitor) {
  g_print("free inhibitor\n");
  g_assert_nonnull(inhibitor);
  zwp_idle_inhibitor_v1_destroy(inhibitor);
}

AstalInhibitor* astal_inhibit_manager_inhibit(AstalInhibitManager *self, GtkWindow *window) {
  AstalInhibitManagerPrivate* priv = astal_inhibit_manager_get_instance_private(self);
  g_assert_true(priv->init);
  GdkWindow *gdk_window = gtk_widget_get_window(GTK_WIDGET(window));
  struct wl_surface *surface = gdk_wayland_window_get_wl_surface(gdk_window);
  return zwp_idle_inhibit_manager_v1_create_inhibitor(priv->idle_inhibit_manager, surface);
}

static void global_registry_handler(void* data, struct wl_registry* registry, uint32_t id,
                                    const char* interface, uint32_t version) {
    AstalInhibitManager* self = ASTAL_INHIBIT_MANAGER(data);
    AstalInhibitManagerPrivate* priv = astal_inhibit_manager_get_instance_private(self);

    if (strcmp(interface, zwp_idle_inhibit_manager_v1_interface.name) == 0) {
        priv->idle_inhibit_manager =
            wl_registry_bind(registry, id, &zwp_idle_inhibit_manager_v1_interface, 1);
    }
    
}

static void global_registry_remover(void* data, struct wl_registry* registry, uint32_t id) {
  //neither inhibit_manager nor inhibitor is going to be removed by the compositor, so we don't need do anything here.
}

static const struct wl_registry_listener registry_listener = {global_registry_handler,
                                                              global_registry_remover};


static gboolean astal_inhibit_manager_wayland_init(AstalInhibitManager *self) {

    AstalInhibitManagerPrivate* priv = astal_inhibit_manager_get_instance_private(self);

    if (priv->init) return TRUE;

    GdkDisplay *gdk_display = gdk_display_get_default();
    priv->display = gdk_wayland_display_get_wl_display(gdk_display);

    priv->wl_registry = wl_display_get_registry(priv->display);
    wl_registry_add_listener(priv->wl_registry, &registry_listener, self);

    wl_display_roundtrip(priv->display);
 
    if (priv->idle_inhibit_manager == NULL) {
        g_critical("Can not connect idle inhibitor protocol");
        return FALSE;
    }

    priv->init = TRUE;
    return TRUE;
}

AstalInhibitManager* astal_inhibit_manager_get_default() {
    static AstalInhibitManager* self = NULL;

    if (self == NULL) {
      self = g_object_new(ASTAL_TYPE_INHIBIT_MANAGER, NULL);
      if(!astal_inhibit_manager_wayland_init(self)) {
        g_object_unref(self);
        self = NULL;
      }
    }

    return self;
}

static void astal_inhibit_manager_init(AstalInhibitManager* self) {
    AstalInhibitManagerPrivate* priv = astal_inhibit_manager_get_instance_private(self);
    priv->init = FALSE;
    priv->display = NULL;
    priv->wl_registry = NULL;
    priv->idle_inhibit_manager = NULL;
}

static void astal_inhibit_manager_finalize(GObject* object) {

    AstalInhibitManager* self = ASTAL_INHIBIT_MANAGER(object);
    AstalInhibitManagerPrivate* priv = astal_inhibit_manager_get_instance_private(self);

    if (priv->display != NULL) wl_display_roundtrip(priv->display);

    if (priv->wl_registry != NULL) wl_registry_destroy(priv->wl_registry);
    if (priv->idle_inhibit_manager != NULL) zwp_idle_inhibit_manager_v1_destroy(priv->idle_inhibit_manager);

    G_OBJECT_CLASS(astal_inhibit_manager_parent_class)->finalize(object);
}

static void astal_inhibit_manager_class_init(AstalInhibitManagerClass* class) {
    GObjectClass* object_class = G_OBJECT_CLASS(class);
    object_class->finalize = astal_inhibit_manager_finalize;

}
