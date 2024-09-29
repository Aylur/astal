#include "astal-cava.h"
#include "cava/common.h"
#include "cava/config.h"
#include "glib-object.h"
#include "glib.h"

#include <gio/gio.h>
#include <cava/common.h>
#include <linux/limits.h>

struct _AstalCavaCava {
    GObject parent_instance;

    gint bars;
    gchar* config_path; 

    GArray* values;
};

typedef struct {
    struct cava_plan plan;
    struct config_params cfg;
    struct audio_data audio_data;
    struct audio_raw audio_raw;
    ptr input_src;

    GThread* input_thread;
    guint timer_id;

} AstalCavaCavaPrivate;

G_DEFINE_TYPE_WITH_PRIVATE(AstalCavaCava, astal_cava_cava, G_TYPE_OBJECT)

typedef enum {
    ASTAL_CAVA_CAVA_PROP_VALUES = 1,
    ASTAL_CAVA_CAVA_PROP_BARS,
    ASTAL_CAVA_CAVA_PROP_CONFIG_PATH,
    ASTAL_CAVA_CAVA_N_PROPERTIES
} AstalCavaProperties;

static GParamSpec* astal_cava_cava_properties[ASTAL_CAVA_CAVA_N_PROPERTIES] = {
    NULL,
};

/**
 * astal_cava_cava_get_values
 * @self: the AstalCavaCava object
 *
 * Returns: (transfer none) (element-type gdouble): a list of values
 *
 */
GArray* astal_cava_cava_get_values(AstalCavaCava* self) { return self->values; }

static void astal_cava_cava_set_property(GObject* object, guint property_id, const GValue* value,
                                            GParamSpec* pspec) {
    AstalCavaCava* self = ASTAL_CAVA_CAVA(object);

    switch (property_id) {
        case ASTAL_CAVA_CAVA_PROP_BARS:
            self->bars = g_value_get_int(value);
            break;
        case ASTAL_CAVA_CAVA_PROP_CONFIG_PATH:
            self->config_path = g_strdup(g_value_get_string(value));
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_cava_cava_get_property(GObject* object, guint property_id, GValue* value,
                                            GParamSpec* pspec) {
    AstalCavaCava* self = ASTAL_CAVA_CAVA(object);

    switch (property_id) {
        case ASTAL_CAVA_CAVA_PROP_BARS:
            g_value_set_int(value, self->bars);
            break;
        case ASTAL_CAVA_CAVA_PROP_VALUES:
            g_value_set_pointer(value, self->values);
            break;
        case ASTAL_CAVA_CAVA_PROP_CONFIG_PATH:
            g_value_set_string(value, self->config_path);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static gboolean exec_cava(AstalCavaCava* self) {
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);

    pthread_mutex_lock(&priv->audio_data.lock);
    cava_execute(priv->audio_data.cava_in, priv->audio_data.samples_counter,
                 priv->audio_raw.cava_out, &priv->plan);
    if (priv->audio_data.samples_counter > 0) priv->audio_data.samples_counter = 0;
    pthread_mutex_unlock(&priv->audio_data.lock);

    g_array_remove_range(self->values, 0, priv->audio_raw.number_of_bars);
    g_array_insert_vals(self->values, 0, priv->audio_raw.cava_out, priv->audio_raw.number_of_bars);

    g_object_notify(G_OBJECT(self), "values");

    return G_SOURCE_CONTINUE;
}

static void astal_cava_cava_constructed(GObject* object) {
    AstalCavaCava* self = ASTAL_CAVA_CAVA(object);
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);

    struct error_s error = {};

    if (!load_config(self->config_path, &priv->cfg, false, &error)) {
      g_critical("Error loading config. %s", error.message);
      return;
    }

    priv->cfg.inAtty = 0;
    priv->cfg.output = OUTPUT_RAW;
    priv->cfg.raw_target = strdup("/dev/stdout");
    priv->cfg.data_format = strdup("binary");

    if(self->bars > 0) priv->cfg.fixedbars = self->bars;
    else self->bars = priv->cfg.fixedbars;

    priv->audio_data = (struct audio_data) {
      .cava_in = calloc(BUFFER_SIZE * priv->cfg.channels * 8, sizeof(gdouble)),
      .input_buffer_size = BUFFER_SIZE * priv->cfg.channels,
      .cava_buffer_size = BUFFER_SIZE * priv->cfg.channels * 8,
      .format = -1,
      .rate = 0,
      .channels = priv->cfg.channels,
      .source = g_strdup(priv->cfg.audio_source),
      .terminate = 0,
      .samples_counter = 0,
      .IEEE_FLOAT = 0,
      .suspendFlag = false,
    };
    
    priv->input_src = get_input(&priv->audio_data, &priv->cfg);

    audio_raw_init(&priv->audio_data, &priv->audio_raw, &priv->cfg, &priv->plan); 

    self->values = g_array_sized_new(TRUE, TRUE, sizeof(gdouble), priv->audio_raw.number_of_bars);
    g_array_set_size(self->values, priv->audio_raw.number_of_bars);

    priv->input_thread = g_thread_new("cava_input", priv->input_src, &priv->audio_data);

    priv->timer_id = g_timeout_add(1000 / priv->cfg.framerate, G_SOURCE_FUNC(exec_cava), self);
}

static void astal_cava_cava_init(AstalCavaCava* self) {
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);
}

/**
 * astal_cava_cava_get_default
 *
 * Returns: (nullable) (transfer none): gets the default Cava object.
 */
AstalCavaCava* astal_cava_cava_get_default() {
    static AstalCavaCava* self = NULL;

    if (self == NULL) self = g_object_new(ASTAL_CAVA_TYPE_CAVA, NULL);

    return self;
}

static void astal_cava_cava_dispose(GObject* object) {
    AstalCavaCava* self = ASTAL_CAVA_CAVA(object);
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);

    g_source_remove(priv->timer_id);
    pthread_mutex_lock(&priv->audio_data.lock);
    priv->audio_data.terminate = 1;
    pthread_mutex_unlock(&priv->audio_data.lock);
    g_thread_join(priv->input_thread);
}

static void astal_cava_cava_finalize(GObject* object) {
    AstalCavaCava* self = ASTAL_CAVA_CAVA(object);
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);

    cava_destroy(&priv->plan);
    g_array_free(self->values, TRUE);

    G_OBJECT_CLASS(astal_cava_cava_parent_class)->finalize(object);
}

static void astal_cava_cava_class_init(AstalCavaCavaClass* class) {
    GObjectClass* object_class = G_OBJECT_CLASS(class);
    object_class->get_property = astal_cava_cava_get_property;
    object_class->set_property = astal_cava_cava_set_property;
    object_class->constructed = astal_cava_cava_constructed;
    object_class->dispose = astal_cava_cava_dispose;
    object_class->finalize = astal_cava_cava_finalize;

    /**
     * AstalCava:values: (type GList(gdouble))
     *
     * A list of values
     */
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_VALUES] =
        g_param_spec_pointer("values", "values", "a list of values", G_PARAM_READABLE);
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_BARS] =
        g_param_spec_int("bars", "bars", "number of bars per channel", 0, G_MAXINT, 0,
                         G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_CONFIG_PATH] =
        g_param_spec_string("config-path", "config-path", "config-path", "", G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    g_object_class_install_properties(object_class, ASTAL_CAVA_CAVA_N_PROPERTIES,
                                      astal_cava_cava_properties);
}
