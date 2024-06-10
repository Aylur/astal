#ifndef ASTAL_RIVER_H
#define ASTAL_RIVER_H

#include <glib-object.h>

G_BEGIN_DECLS

#define ASTAL_RIVER_TYPE_OUTPUT (astal_river_output_get_type())

G_DECLARE_FINAL_TYPE (AstalRiverOutput, astal_river_output, ASTAL_RIVER, OUTPUT, GObject)

/**
 * astal_river_output_get_nid
 * @self: the AstalRiverOutput object
 *
 * Returns: the id of the output
 */
guint astal_river_output_get_id(AstalRiverOutput *self);

/**
 * astal_river_output_get_name
 * @self: the AstalRiverOutput object
 *
 * Returns: (transfer none) (nullable): the name of the output
 */
gchar* astal_river_output_get_name(AstalRiverOutput *self);

/**
 * astal_river_output_get_layout_name
 * @self: the AstalRiverOutput object
 *
 * Returns: (transfer none) (nullable): the currently used layout name of the output
 */
gchar* astal_river_output_get_layout_name(AstalRiverOutput *self);

/**
 * astal_river_output_get_focused_view
 * @self: the AstalRiverOutput object
 *
 * Returns: (transfer none) (nullable): the focused view on the output
 */
gchar* astal_river_output_get_focused_view(AstalRiverOutput *self);

/**
 * astal_river_output_get_focused_tags
 * @self: the AstalRiverOutput object
 *
 * Returns: the focused tags of the output
 */
guint astal_river_output_get_focused_tags(AstalRiverOutput *self);

/**
 * astal_river_output_get_urgent_tags
 * @self: the AstalRiverOutput object
 *
 * Returns: the urgent tags of the output
 */
guint astal_river_output_get_urgent_tags(AstalRiverOutput *self);


/**
 * astal_river_output_get_occupied_tags
 * @self: the AstalRiverOutput object
 *
 * Returns: the occupied tags of the output
 */
guint astal_river_output_get_occupied_tags(AstalRiverOutput *self);



#define ASTAL_RIVER_TYPE_RIVER (astal_river_river_get_type())

G_DECLARE_FINAL_TYPE (AstalRiverRiver, astal_river_river, ASTAL_RIVER, RIVER, GObject)



/**
 * astal_river_river_new
 *
 * Returns: (nullable): a newly created connection to river
 */

AstalRiverRiver *astal_river_river_new();

/**
 * astal_river_river_get_outputs
 * @self: the AstalRiverRiver object
 *
 * Returns: (transfer none) (element-type AstalRiver.Output): a list of all outputs
 *
 */
GList* astal_river_river_get_outputs(AstalRiverRiver *self);

/**
 * astal_river_river_get_output
 * @self: the AstalRiverRiver object
 * @name: the name of the output
 *
 * Returns: (transfer none) (nullable): the output with the given name or null
 */
AstalRiverOutput* astal_river_river_get_output(AstalRiverRiver *self, gchar* name);


/**
 * astal_river_river_get_focused_view
 * @self: the AstalRiverOutput object
 *
 * Returns: (transfer none) (nullable): the currently focused view
 */
gchar* astal_river_river_get_focused_view(AstalRiverRiver *self);

/**
 * astal_river_river_get_focused_output
 * @self: the AstalRiverOutput object
 *
 * Returns: (transfer none) (nullable): the name of the currently focused output
 */
gchar* astal_river_river_get_focused_output(AstalRiverRiver *self);


/**
 * astal_river_river_get_mode
 * @self: the AstalRiverOutput object
 *
 * Returns: (transfer none) (nullable): the currently active mode
 */
gchar* astal_river_river_get_mode(AstalRiverRiver *self);





G_END_DECLS

#endif // !ASTAL_RIVER_H
