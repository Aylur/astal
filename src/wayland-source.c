
#include "wayland-source.h"

#include <errno.h>
#include <glib.h>
#include <wayland-client.h>

struct _WLSource {
    GSource source;
    struct wl_display *display;
    gpointer fd;
    int error;
};

static gboolean wl_source_prepare(GSource *source, gint *timeout) {
    WLSource *self = (WLSource *)source;

    *timeout = 0;
    if (wl_display_prepare_read(self->display) != 0)
        return TRUE;
    else if (wl_display_flush(self->display) < 0) {
        self->error = errno;
        return TRUE;
    }
    *timeout = -1;
    return FALSE;
}

static gboolean wl_source_check(GSource *source) {
    WLSource *self = (WLSource *)source;

    if (self->error > 0) return TRUE;

    GIOCondition revents;
    revents = g_source_query_unix_fd(source, self->fd);

    if (revents & G_IO_IN) {
        if (wl_display_read_events(self->display) < 0) self->error = errno;
    } else
        wl_display_cancel_read(self->display);

    return revents > 0;
}

static gboolean wl_source_dispatch(GSource *source, GSourceFunc callback, gpointer user_data) {
    WLSource *self = (WLSource *)source;
    GIOCondition revents;

    revents = g_source_query_unix_fd(source, self->fd);
    if ((self->error > 0) || (revents & (G_IO_ERR | G_IO_HUP))) {
        errno = self->error;
        self->error = 0;
        if (callback != NULL) return callback(user_data);
        return G_SOURCE_REMOVE;
    }

    if (wl_display_dispatch_pending(self->display) < 0) {
        if (callback != NULL) return callback(user_data);
        return G_SOURCE_REMOVE;
    }

    return G_SOURCE_CONTINUE;
}

static void wl_source_finalize(GSource *source) {
    WLSource *self = (WLSource *)source;
    wl_display_disconnect(self->display);
}

static GSourceFuncs wl_source_funcs = {
    .prepare = wl_source_prepare,
    .check = wl_source_check,
    .dispatch = wl_source_dispatch,
    .finalize = wl_source_finalize,
};

WLSource *wl_source_new() {
    struct wl_display *display;
    WLSource *self;
    GSource *source;

    display = wl_display_connect(NULL);
    if (display == NULL) return NULL;

    source = g_source_new(&wl_source_funcs, sizeof(WLSource));
    self = (WLSource *)source;
    self->display = display;

    self->fd = g_source_add_unix_fd(source, wl_display_get_fd(self->display),
                                    G_IO_IN | G_IO_ERR | G_IO_HUP);

    g_source_attach(source, NULL);

    return self;
}

void wl_source_free(WLSource *self) {
    GSource *source = (GSource *)self;
    g_return_if_fail(source != NULL);
    g_source_destroy(source);
    g_source_unref(source);
}

struct wl_display *wl_source_get_display(WLSource *self) { return self->display; }
