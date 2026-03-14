#include "astal-river.h"

VALA_EXTERN void astal_river_layout_set_layout_demand_closure(AstalRiverLayout* self, GClosure* closure) {
	g_return_if_fail (self != NULL);

    if(self->layout_demand_closure != NULL)
        g_closure_unref(self->layout_demand_closure);

    if(closure != NULL) {
        self->layout_demand_closure = g_closure_ref(closure);
        g_closure_sink(closure);
        if (G_CLOSURE_NEEDS_MARSHAL (closure)) {
            g_closure_set_marshal (closure, g_cclosure_marshal_generic);
        }
    }
}
