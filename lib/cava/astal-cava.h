#ifndef ASTAL_CAVA_H
#define ASTAL_CAVA_H

#include <glib-object.h>

G_BEGIN_DECLS

#define ASTAL_CAVA_TYPE_CAVA (astal_cava_cava_get_type())

G_DECLARE_FINAL_TYPE(AstalCavaCava, astal_cava_cava, ASTAL_CAVA, CAVA, GObject)

AstalCavaCava* astal_cava_cava_get_default();

GArray* astal_cava_cava_get_values(AstalCavaCava* self);

G_END_DECLS

#endif  // !ASTAL_CAVA_H
