#ifndef ASTAL_WP_DEVICE_PRIVATE_H
#define ASTAL_WP_DEVICE_PRIVATE_H

#include <glib-object.h>
#include <wp/wp.h>

#include "device.h"

G_BEGIN_DECLS

AstalWpDevice *astal_wp_device_create(WpDevice *device);

G_END_DECLS

#endif  // !ASTAL_WP_DEVICE_PRIATE_H
