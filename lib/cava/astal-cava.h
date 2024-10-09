#ifndef ASTAL_CAVA_H
#define ASTAL_CAVA_H

#include <glib-object.h>

G_BEGIN_DECLS

#define ASTAL_CAVA_TYPE_CAVA (astal_cava_cava_get_type())

G_DECLARE_FINAL_TYPE(AstalCavaCava, astal_cava_cava, ASTAL_CAVA, CAVA, GObject)

AstalCavaCava* astal_cava_cava_get_default();

GArray* astal_cava_cava_get_values(AstalCavaCava* self);
gint astal_cava_cava_get_bars(AstalCavaCava* self);
gboolean astal_cava_cava_get_autosens(AstalCavaCava* self);
gboolean astal_cava_cava_get_stereo(AstalCavaCava* self);
gboolean astal_cava_cava_get_monstercat(AstalCavaCava* self);
gdouble astal_cava_cava_get_sensitivity(AstalCavaCava* self);
gdouble astal_cava_cava_get_noise_reduction(AstalCavaCava* self); 
gint astal_cava_cava_get_framerate(AstalCavaCava* self);


G_END_DECLS

#endif  // !ASTAL_CAVA_H
