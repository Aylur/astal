#ifndef __WAYLAND_SOURCE_H__
#define __WAYLAND_SOURCE_H__

#include <glib-object.h>

G_BEGIN_DECLS

typedef struct _WLSource WLSource;

WLSource* wl_source_new();
void wl_source_free(WLSource* self);
struct wl_display* wl_source_get_display(WLSource* source);

G_END_DECLS

#endif /* __WAYLAND_SOURCE_H__ */
