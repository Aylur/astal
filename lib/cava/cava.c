#include "astal-cava.h"
#include "cava/common.h"
#include "cava/config.h"
#include "glib-object.h"
#include "glib.h"
#include "glibconfig.h"

#include <gio/gio.h>
#include <cava/common.h>
#include <linux/limits.h>
#include <string.h>

struct _AstalCavaCava {
    GObject parent_instance;

    gint bars;
    gboolean autosens;
    gdouble sens;
    gboolean stereo;
    gboolean monstercat;
    gdouble noise_reduction;
    gint framerate;

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
    ASTAL_CAVA_CAVA_PROP_AUTOSENS,
    ASTAL_CAVA_CAVA_PROP_SENS,
    ASTAL_CAVA_CAVA_PROP_STEREO,
    ASTAL_CAVA_CAVA_PROP_MONSTERCAT,
    ASTAL_CAVA_CAVA_PROP_NOISE,
    ASTAL_CAVA_CAVA_PROP_FRAMERATE,
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

gint astal_cava_cava_get_bars(AstalCavaCava* self) { return self->bars; }
gboolean astal_cava_cava_get_autosens(AstalCavaCava* self) { return self->autosens; }
gboolean astal_cava_cava_get_stereo(AstalCavaCava* self) { return self->stereo; }
gboolean astal_cava_cava_get_monstercat(AstalCavaCava* self) { return self->monstercat; }
gdouble astal_cava_cava_get_sensitivity(AstalCavaCava* self) { return self->sens; }
gdouble astal_cava_cava_get_noise_reduction(AstalCavaCava* self) { return self->noise_reduction; }
gint astal_cava_cava_get_framerate(AstalCavaCava* self) { return self->framerate; }

static void astal_cava_cava_set_property(GObject* object, guint property_id, const GValue* value,
                                            GParamSpec* pspec) {
    AstalCavaCava* self = ASTAL_CAVA_CAVA(object);

    switch (property_id) {
        case ASTAL_CAVA_CAVA_PROP_BARS:
            self->bars = g_value_get_int(value);
            break;
        case ASTAL_CAVA_CAVA_PROP_AUTOSENS:
            self->autosens = g_value_get_boolean(value);
            break;
        case ASTAL_CAVA_CAVA_PROP_SENS:
            self->sens = g_value_get_double(value);
            break;
        case ASTAL_CAVA_CAVA_PROP_NOISE:
            self->noise_reduction = g_value_get_double(value);
            break;
        case ASTAL_CAVA_CAVA_PROP_STEREO:
            self->stereo = g_value_get_boolean(value);
            break;
        case ASTAL_CAVA_CAVA_PROP_MONSTERCAT:
            self->monstercat = g_value_get_boolean(value);
            break;
        case ASTAL_CAVA_CAVA_PROP_FRAMERATE:
            self->framerate = g_value_get_int(value);
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
        case ASTAL_CAVA_CAVA_PROP_AUTOSENS:
            g_value_set_boolean(value, self->autosens);
            break;
        case ASTAL_CAVA_CAVA_PROP_SENS:
            g_value_set_double(value, self->sens);
            break;
        case ASTAL_CAVA_CAVA_PROP_NOISE:
            g_value_set_double(value, self->noise_reduction);
            break;
        case ASTAL_CAVA_CAVA_PROP_STEREO:
            g_value_set_boolean(value, self->stereo);
            break;
        case ASTAL_CAVA_CAVA_PROP_MONSTERCAT:
            g_value_set_boolean(value, self->monstercat);
            break;
        case ASTAL_CAVA_CAVA_PROP_FRAMERATE:
            g_value_set_int(value, self->framerate);
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

    priv->cfg = (struct config_params) {
      .inAtty = 0,
      .output = OUTPUT_RAW,
      .raw_target = strdup("/dev/stdout"),
      .data_format = strdup("binary"),

      .fixedbars = self->bars,
      .autosens = self->autosens,
      .sens = self->sens,
      .stereo = self->stereo,
      .channels = self->stereo + 1,
      .monstercat = self->monstercat,
      .noise_reduction = self->noise_reduction,
      .framerate = self->framerate,
      
      //TODO: make these configurable
      .audio_source = strdup("auto"),
      .input = INPUT_PIPEWIRE,

      //maybe make some of them configurable
      .lower_cut_off = 50,
      .upper_cut_off = 10000,
      .mono_opt = AVERAGE,
      .waves = 0, 
      .userEQ = NULL,
      .userEQ_keys = 0,
      .userEQ_enabled = 0,
      .samplerate = 44100,
      .samplebits = 16,
      .waveform = 0,

      //not needed in this lib
      .autoconnect = 2,
      .reverse = 0,
      .sleep_timer = 0,
      .show_idle_bar_heads = 1,
      .continuous_rendering = 0,
      .sdl_width = 1000,
      .sdl_height = 500, 
      .sdl_x = -1,
      .sdl_y = -1,
      .sdl_full_screen = 0,
      .draw_and_quit = 0,
      .zero_test = 0,
      .non_zero_test = 0,
      .sync_updates = 0,
      .disable_blanking = 0,
      .bar_height = 32, 
      .col = 0,
      .bgcol = 0,
      .autobars = 0,
      .raw_format = FORMAT_BINARY,
      .ascii_range = 1000,
      .bit_format = 16,
      .gradient = 0,
      .gradient_count = 0, 
      .bar_width = 2,
      .bar_spacing = 1,
      .xaxis = NONE,
      .orientation = ORIENT_BOTTOM,
      .color = NULL,
      .bcolor = NULL,
      .gradient_colors = NULL,
      .vertex_shader = NULL,
      .fragment_shader = NULL,
      .bar_delim = ';',
      .frame_delim = '\n',
    };

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

static void astal_cava_cava_init(AstalCavaCava* self) { }

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
     * AstalCava:values: (type GArray(gdouble))
     *
     * A list of values
     */
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_VALUES] =
        g_param_spec_pointer("values", "values", "a list of values", G_PARAM_READABLE);
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_BARS] =
        g_param_spec_int("bars", "bars", "number of bars per channel", 1, G_MAXINT, 20,
                         G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_AUTOSENS] =
        g_param_spec_boolean("autosens", "autosens", "dynamically adjust sensitivity", TRUE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_SENS] =
        g_param_spec_double("sensitivity", "sensitivity", "sensitivity", 0, G_MAXDOUBLE, 1,
                         G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_STEREO] =
        g_param_spec_boolean("stereo", "stereo", "stereo", FALSE,
                         G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_MONSTERCAT] =
        g_param_spec_boolean("monstercat", "monstercat", "monstercat smoothing", FALSE,
                         G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_NOISE] =
        g_param_spec_double("noise_reduction", "noise_reduction", "noise reduction", 0, G_MAXDOUBLE, 0.77,
                         G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_FRAMERATE] =
        g_param_spec_int("framerate", "framerate", "framerate", 1, G_MAXINT, 60,
                         G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
    g_object_class_install_properties(object_class, ASTAL_CAVA_CAVA_N_PROPERTIES,
                                      astal_cava_cava_properties);
}
