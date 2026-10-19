#include <cava/common.h>
#include <gio/gio.h>

#include "astal-cava.h"
#include "cava/common.h"
#include "cava/config.h"
#include "glib-object.h"
#include "glib.h"
#include "glibconfig.h"

struct _AstalCavaCava {
    GObject parent_instance;

    gint bars;
    gboolean autosens;
    gboolean stereo;
    gdouble noise_reduction;
    gint framerate;
    AstalCavaInput input;
    gchar* audio_source;
    gboolean active;
    gint channels;
    gint low_cutoff;
    gint high_cutoff;
    gint samplerate;

    GArray* values;
};

typedef struct {
    gatomicrefcount ref_count;
    GWeakRef self;
    struct cava_plan* plan;
    struct config_params cfg;
    struct audio_data audio_data;
    struct audio_raw audio_raw;
    ptr input_src;
} AstalCavaWorker;

typedef enum {
    ASTAL_CAVA_STATE_STOPPED,
    ASTAL_CAVA_STATE_RUNNING,
    ASTAL_CAVA_STATE_STOPPING,
    ASTAL_CAVA_STATE_RESTART_PENDING,
} AstalCavaState;

typedef struct {
    gboolean constructed;
    AstalCavaWorker* worker;
    AstalCavaState state;
    guint shutdown_timeout_id;
    guint timer_id;
} AstalCavaCavaPrivate;

#define CAVA_SHUTDOWN_TIMEOUT_MS 2000

G_DEFINE_ENUM_TYPE(AstalCavaInput, astal_cava_input,
                   G_DEFINE_ENUM_VALUE(ASTAL_CAVA_INPUT_FIFO, "fifo"),
                   G_DEFINE_ENUM_VALUE(ASTAL_CAVA_INPUT_PORTAUDIO, "portaudio"),
                   G_DEFINE_ENUM_VALUE(ASTAL_CAVA_INPUT_PIPEWIRE, "pipewire"),
                   G_DEFINE_ENUM_VALUE(ASTAL_CAVA_INPUT_ALSA, "alsa"),
                   G_DEFINE_ENUM_VALUE(ASTAL_CAVA_INPUT_PULSE, "pulse"),
                   G_DEFINE_ENUM_VALUE(ASTAL_CAVA_INPUT_SNDIO, "sndio"),
                   G_DEFINE_ENUM_VALUE(ASTAL_CAVA_INPUT_OSS, "oss"),
                   G_DEFINE_ENUM_VALUE(ASTAL_CAVA_INPUT_JACK, "jack"),
                   G_DEFINE_ENUM_VALUE(ASTAL_CAVA_INPUT_SHMEM, "shmem"),
                   G_DEFINE_ENUM_VALUE(ASTAL_CAVA_INPUT_WINSCAP, "winscap"),
                   G_DEFINE_ENUM_VALUE(ASTAL_CAVA_INPUT_COREAUDIO, "coreaudio"));

static enum input_method astal_input_to_cava(AstalCavaInput input) {
    switch (input) {
        case ASTAL_CAVA_INPUT_FIFO:
            return INPUT_FIFO;
        case ASTAL_CAVA_INPUT_PORTAUDIO:
            return INPUT_PORTAUDIO;
        case ASTAL_CAVA_INPUT_PIPEWIRE:
            return INPUT_PIPEWIRE;
        case ASTAL_CAVA_INPUT_ALSA:
            return INPUT_ALSA;
        case ASTAL_CAVA_INPUT_PULSE:
            return INPUT_PULSE;
        case ASTAL_CAVA_INPUT_SNDIO:
            return INPUT_SNDIO;
        case ASTAL_CAVA_INPUT_OSS:
            return INPUT_OSS;
        case ASTAL_CAVA_INPUT_JACK:
            return INPUT_JACK;
        case ASTAL_CAVA_INPUT_SHMEM:
            return INPUT_SHMEM;
        case ASTAL_CAVA_INPUT_WINSCAP:
            return INPUT_WINSCAP;
        case ASTAL_CAVA_INPUT_COREAUDIO:
            return INPUT_COREAUDIO;
        default:
            g_critical("could not translate AstalCavaInput enum to cava\n");
    }
    return INPUT_PIPEWIRE;
}

G_DEFINE_TYPE_WITH_PRIVATE(AstalCavaCava, astal_cava_cava, G_TYPE_OBJECT)

static const char* astal_cava_state_name(AstalCavaState state) {
    switch (state) {
        case ASTAL_CAVA_STATE_STOPPED:
            return "stopped";
        case ASTAL_CAVA_STATE_RUNNING:
            return "running";
        case ASTAL_CAVA_STATE_STOPPING:
            return "stopping";
        case ASTAL_CAVA_STATE_RESTART_PENDING:
            return "restart-pending";
        default:
            return "unknown";
    }
}

static void astal_cava_worker_unref(AstalCavaWorker* worker) {
    if (!g_atomic_ref_count_dec(&worker->ref_count)) return;

    g_debug("worker %p: releasing resources", worker);
    g_weak_ref_clear(&worker->self);
    cava_destroy(worker->plan);
    audio_raw_clean(&worker->audio_raw);
    free(worker->audio_data.cava_in);
    g_free(worker->audio_data.source);
    free_config(&worker->cfg);
    g_free(worker);
}

static void astal_cava_cava_start(AstalCavaCava* self);
static void astal_cava_cava_stop(AstalCavaCava* self, AstalCavaState state);

static gboolean astal_cava_worker_finished(gpointer data) {
    AstalCavaWorker* worker = data;
    AstalCavaCava* self = g_weak_ref_get(&worker->self);

    g_debug("worker %p: completion dispatched on main context", worker);
    if (self) {
        AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);
        if (priv->worker == worker &&
            (priv->state == ASTAL_CAVA_STATE_STOPPING ||
             priv->state == ASTAL_CAVA_STATE_RESTART_PENDING)) {
            g_debug("worker %p: shutdown completed in state %s", worker,
                    astal_cava_state_name(priv->state));
            if (priv->shutdown_timeout_id) {
                g_source_remove(priv->shutdown_timeout_id);
                priv->shutdown_timeout_id = 0;
            }

            gboolean restart = priv->state == ASTAL_CAVA_STATE_RESTART_PENDING && self->active;
            priv->worker = NULL;
            priv->state = ASTAL_CAVA_STATE_STOPPED;
            if (restart)
                g_debug("worker %p: starting replacement after clean shutdown", worker);
            astal_cava_worker_unref(worker);

            astal_cava_worker_unref(worker);
            if (restart) astal_cava_cava_start(self);
            g_object_unref(self);
            return G_SOURCE_REMOVE;
        }
        g_debug("worker %p: completion is stale (current worker %p, state %s)", worker,
                priv->worker, astal_cava_state_name(priv->state));
        g_object_unref(self);
    } else {
        g_debug("worker %p: owner no longer exists", worker);
    }

    astal_cava_worker_unref(worker);
    return G_SOURCE_REMOVE;
}

static gpointer astal_cava_worker_run(gpointer data) {
    AstalCavaWorker* worker = data;
    g_debug("worker %p: input thread started", worker);
    worker->input_src(&worker->audio_data);
    g_debug("worker %p: input function returned; scheduling completion", worker);
    g_idle_add(astal_cava_worker_finished, worker);
    return NULL;
}

static gboolean astal_cava_cava_shutdown_timeout(gpointer data) {
    AstalCavaCava* self = ASTAL_CAVA_CAVA(data);
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);
    AstalCavaWorker* worker = priv->worker;
    priv->shutdown_timeout_id = 0;

    if (!worker || (priv->state != ASTAL_CAVA_STATE_STOPPING &&
                    priv->state != ASTAL_CAVA_STATE_RESTART_PENDING)) {
        g_debug("shutdown timeout ignored (worker %p, state %s)", worker,
                astal_cava_state_name(priv->state));
        return G_SOURCE_REMOVE;
    }

    g_debug("worker %p: timeout expired in state %s", worker,
            astal_cava_state_name(priv->state));
    priv->worker = NULL;
    gboolean restart = priv->state == ASTAL_CAVA_STATE_RESTART_PENDING && self->active;
    priv->state = ASTAL_CAVA_STATE_STOPPED;
    astal_cava_worker_unref(worker);

    if (restart) {
        g_warning("input thread did not stop within %u ms; proceeding with restart",
                  CAVA_SHUTDOWN_TIMEOUT_MS);
        astal_cava_cava_start(self);
    } else {
        g_warning("input thread did not stop within %u ms; leaving it detached",
                  CAVA_SHUTDOWN_TIMEOUT_MS);
    }
    return G_SOURCE_REMOVE;
}

typedef enum {
    ASTAL_CAVA_CAVA_PROP_VALUES = 1,
    ASTAL_CAVA_CAVA_PROP_ACTIVE,
    ASTAL_CAVA_CAVA_PROP_BARS,
    ASTAL_CAVA_CAVA_PROP_AUTOSENS,
    ASTAL_CAVA_CAVA_PROP_STEREO,
    ASTAL_CAVA_CAVA_PROP_NOISE,
    ASTAL_CAVA_CAVA_PROP_FRAMERATE,
    ASTAL_CAVA_CAVA_PROP_INPUT,
    ASTAL_CAVA_CAVA_PROP_SOURCE,
    ASTAL_CAVA_CAVA_PROP_CHANNELS,
    ASTAL_CAVA_CAVA_PROP_LOW_CUTOFF,
    ASTAL_CAVA_CAVA_PROP_HIGH_CUTOFF,
    ASTAL_CAVA_CAVA_PROP_SAMPLERATE,
    ASTAL_CAVA_CAVA_N_PROPERTIES
} AstalCavaProperties;

static GParamSpec* astal_cava_cava_properties[ASTAL_CAVA_CAVA_N_PROPERTIES] = {
    NULL,
};

static gboolean exec_cava(AstalCavaCava* self) {
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);
    AstalCavaWorker* worker = priv->worker;

    pthread_mutex_lock(&worker->audio_data.lock);
    cava_execute(worker->audio_data.cava_in, worker->audio_data.samples_counter,
                 worker->audio_raw.cava_out, worker->plan);
    if (worker->audio_data.samples_counter > 0) worker->audio_data.samples_counter = 0;
    pthread_mutex_unlock(&worker->audio_data.lock);

    g_array_remove_range(self->values, 0, self->values->len);
    g_array_insert_vals(self->values, 0, worker->audio_raw.cava_out, worker->audio_raw.number_of_bars);

    g_object_notify(G_OBJECT(self), "values");

    return G_SOURCE_CONTINUE;
}

static void astal_cava_cava_stop(AstalCavaCava* self, AstalCavaState state) {
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);

    if (priv->timer_id) {
        g_source_remove(priv->timer_id);
        priv->timer_id = 0;
    }
    if (!priv->worker) {
        priv->state = ASTAL_CAVA_STATE_STOPPED;
        g_debug("stop requested with no worker; state is stopped");
        return;
    }

    AstalCavaWorker* worker = priv->worker;
    if (priv->state == ASTAL_CAVA_STATE_STOPPING ||
        priv->state == ASTAL_CAVA_STATE_RESTART_PENDING) {
        g_debug("worker %p: shutdown state %s -> %s", worker,
                astal_cava_state_name(priv->state), astal_cava_state_name(state));
        priv->state = state;
        return;
    }

    g_debug("worker %p: shutdown requested; state %s -> %s", worker,
            astal_cava_state_name(priv->state), astal_cava_state_name(state));
    priv->state = state;
    pthread_mutex_lock(&worker->audio_data.lock);
    worker->audio_data.terminate = 1;
    pthread_mutex_unlock(&worker->audio_data.lock);
    priv->shutdown_timeout_id =
        g_timeout_add(CAVA_SHUTDOWN_TIMEOUT_MS, astal_cava_cava_shutdown_timeout, self);
}

static void astal_cava_cava_start(AstalCavaCava* self) {
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);
    g_return_if_fail(priv->worker == NULL);
    g_return_if_fail(priv->state == ASTAL_CAVA_STATE_STOPPED);
    AstalCavaWorker* worker = g_new0(AstalCavaWorker, 1);
    g_atomic_ref_count_init(&worker->ref_count);
    g_weak_ref_init(&worker->self, self);

    if (self->framerate < 1) {
        self->framerate = 1;
    }

    if (self->low_cutoff < 1) {
        self->low_cutoff = 1;
    }

    if (self->high_cutoff < self->low_cutoff) {
        self->high_cutoff = self->low_cutoff + 1;
    }

    if (self->samplerate / 2 <= self->high_cutoff) {
        self->samplerate = self->high_cutoff * 2 + 1;
    }

    if (self->bars < 1) {
        self->bars = 1;
    }

    if (self->channels < 1 || self->channels > 2) {
        self->channels = 2;
    }

    worker->cfg = (struct config_params){
        .inAtty = 0,
        .output = OUTPUT_RAW,
        .raw_target = strdup("/dev/stdout"),
        .data_format = strdup("binary"),

        .fixedbars = self->bars,
        .autosens = self->autosens,
        .stereo = self->stereo,
        .noise_reduction = self->noise_reduction,
        .framerate = self->framerate,
        .input = astal_input_to_cava(self->input),
        .channels = self->channels,
        .lower_cut_off = self->low_cutoff,
        .upper_cut_off = self->high_cutoff,
        .samplerate = self->samplerate,

        // not needed in this lib
        .mono_opt = AVERAGE,
        .waves = 0,
        .userEQ = NULL,
        .userEQ_keys = 0,
        .userEQ_enabled = 0,
        .samplebits = 16,
        .waveform = 0,
        .monstercat = 0,
        .sens = 1,
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
        .disable_blanking = 1,
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

    if (g_strcmp0(self->audio_source, "auto") == 0) {
        switch (worker->cfg.input) {
            case INPUT_ALSA:
                worker->cfg.audio_source = g_strdup("hw:Loopback,1");
                break;
            case INPUT_FIFO:
                worker->cfg.audio_source = g_strdup("/tmp/mpd.fifo");
                break;
            case INPUT_PULSE:
                worker->cfg.audio_source = g_strdup("auto");
                break;
            case INPUT_PIPEWIRE:
                worker->cfg.audio_source = g_strdup("auto");
                break;
            case INPUT_SNDIO:
                worker->cfg.audio_source = g_strdup("default");
                break;
            case INPUT_OSS:
                worker->cfg.audio_source = g_strdup("/dev/dsp");
                break;
            case INPUT_JACK:
                worker->cfg.audio_source = g_strdup("default");
                break;
            case INPUT_SHMEM:
                worker->cfg.audio_source = g_strdup("/squeezelite-00:00:00:00:00:00");
                break;
            case INPUT_PORTAUDIO:
                worker->cfg.audio_source = g_strdup("auto");
                break;
            case INPUT_COREAUDIO:
                worker->cfg.audio_source = g_strdup("auto");
                break;
            default:
                g_critical("unsupported audio source");
        }
    } else {
        worker->cfg.audio_source = g_strdup(self->audio_source);
    }

    worker->audio_data = (struct audio_data){
        .cava_in = NULL,
        .input_buffer_size = BUFFER_SIZE * worker->cfg.channels,
        .cava_buffer_size = BUFFER_SIZE * worker->cfg.channels * 8,
        .format = -1,
        .rate = 0,
        .channels = worker->cfg.channels,
        .source = NULL,
        .terminate = 0,
        .samples_counter = 0,
        .IEEE_FLOAT = 0,
        .suspendFlag = false,
    };

    worker->input_src = get_input(&worker->audio_data, &worker->cfg);
    audio_raw_init(&worker->audio_data, &worker->audio_raw, &worker->cfg, &worker->plan);

    priv->worker = worker;
    priv->state = ASTAL_CAVA_STATE_RUNNING;
    g_debug("worker %p: starting input=%d framerate=%d", worker, worker->cfg.input,
            worker->cfg.framerate);
    g_atomic_ref_count_inc(&worker->ref_count);
    g_thread_unref(g_thread_new("cava_input", astal_cava_worker_run, worker));
    priv->timer_id = g_timeout_add(1000 / worker->cfg.framerate, G_SOURCE_FUNC(exec_cava), self);
}

static void astal_cava_cava_restart(AstalCavaCava* self) {
    if (!self->active) return;
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);
    g_debug("restart requested (state=%s, worker=%p)", astal_cava_state_name(priv->state),
            priv->worker);
    astal_cava_cava_stop(self, ASTAL_CAVA_STATE_RESTART_PENDING);
    if (!priv->worker) {
        astal_cava_cava_start(self);
    }
}

gboolean astal_cava_cava_get_active(AstalCavaCava* self) { return self->active; }

void astal_cava_cava_set_active(AstalCavaCava* self, gboolean active) {
    if (self->active == active) return;
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);

    self->active = active;

    if (!priv->constructed) return;
    if (!active) {
        astal_cava_cava_stop(self, ASTAL_CAVA_STATE_STOPPING);
    } else if (priv->worker) {
        astal_cava_cava_stop(self, ASTAL_CAVA_STATE_RESTART_PENDING);
    } else {
        astal_cava_cava_start(self);
    }
}

/**
 * astal_cava_cava_get_values
 * @self: the AstalCavaCava object
 *
 * Returns: (transfer none) (element-type gdouble): a list of values
 *
 */
GArray* astal_cava_cava_get_values(AstalCavaCava* self) { return self->values; }

gint astal_cava_cava_get_bars(AstalCavaCava* self) { return self->bars; }

void astal_cava_cava_set_bars(AstalCavaCava* self, gint bars) {
    if (self->bars == bars) return;
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);
    self->bars = bars;
    if (priv->constructed) {
        g_array_set_size(self->values, self->bars);
        astal_cava_cava_restart(self);
    }
}

gboolean astal_cava_cava_get_autosens(AstalCavaCava* self) { return self->autosens; }

void astal_cava_cava_set_autosens(AstalCavaCava* self, gboolean autosens) {
    if (self->autosens == autosens) return;
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);
    self->autosens = autosens;
    if (priv->constructed) astal_cava_cava_restart(self);
}

gboolean astal_cava_cava_get_stereo(AstalCavaCava* self) { return self->stereo; }

void astal_cava_cava_set_stereo(AstalCavaCava* self, gboolean stereo) {
    if (self->stereo == stereo) return;
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);
    self->stereo = stereo;
    if (priv->constructed) astal_cava_cava_restart(self);
}

gdouble astal_cava_cava_get_noise_reduction(AstalCavaCava* self) { return self->noise_reduction; }

void astal_cava_cava_set_noise_reduction(AstalCavaCava* self, gdouble noise) {
    if (self->noise_reduction == noise) return;
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);
    self->noise_reduction = noise;
    if (priv->constructed) astal_cava_cava_restart(self);
}

gint astal_cava_cava_get_framerate(AstalCavaCava* self) { return self->framerate; }

void astal_cava_cava_set_framerate(AstalCavaCava* self, gint framerate) {
    if (self->framerate == framerate) return;
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);
    self->framerate = framerate;
    if (priv->constructed) astal_cava_cava_restart(self);
}

AstalCavaInput astal_cava_cava_get_input(AstalCavaCava* self) { return self->input; }

void astal_cava_cava_set_input(AstalCavaCava* self, AstalCavaInput input) {
    if (self->input == input) return;
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);
    self->input = input;
    if (priv->constructed) astal_cava_cava_restart(self);
}

gchar* astal_cava_cava_get_source(AstalCavaCava* self) { return self->audio_source; }

void astal_cava_cava_set_source(AstalCavaCava* self, const gchar* source) {
    if (g_strcmp0(self->audio_source, source) == 0) return;
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);
    g_free(self->audio_source);
    self->audio_source = g_strdup(source);
    if (priv->constructed) astal_cava_cava_restart(self);
}

gint astal_cava_cava_get_channels(AstalCavaCava* self) { return self->channels; }

void astal_cava_cava_set_channels(AstalCavaCava* self, gint channels) {
    if (self->channels == channels) return;
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);
    self->channels = channels;
    if (priv->constructed) astal_cava_cava_restart(self);
}

gint astal_cava_cava_get_low_cutoff(AstalCavaCava* self) { return self->low_cutoff; }

void astal_cava_cava_set_low_cutoff(AstalCavaCava* self, gint low_cutoff) {
    if (self->low_cutoff == low_cutoff) return;
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);
    self->low_cutoff = low_cutoff;
    if (priv->constructed) astal_cava_cava_restart(self);
}

gint astal_cava_cava_get_high_cutoff(AstalCavaCava* self) { return self->high_cutoff; }

void astal_cava_cava_set_high_cutoff(AstalCavaCava* self, gint high_cutoff) {
    if (self->high_cutoff == high_cutoff) return;
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);
    self->high_cutoff = high_cutoff;
    if (priv->constructed) astal_cava_cava_restart(self);
}

gint astal_cava_cava_get_samplerate(AstalCavaCava* self) { return self->samplerate; }

void astal_cava_cava_set_samplerate(AstalCavaCava* self, gint samplerate) {
    if (self->samplerate == samplerate) return;
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);
    self->samplerate = samplerate;
    if (priv->constructed) astal_cava_cava_restart(self);
}

static void astal_cava_cava_set_property(GObject* object, guint property_id, const GValue* value,
                                         GParamSpec* pspec) {
    AstalCavaCava* self = ASTAL_CAVA_CAVA(object);

    switch (property_id) {
        case ASTAL_CAVA_CAVA_PROP_BARS:
            astal_cava_cava_set_bars(self, g_value_get_int(value));
            break;
        case ASTAL_CAVA_CAVA_PROP_ACTIVE:
            astal_cava_cava_set_active(self, g_value_get_boolean(value));
            break;
        case ASTAL_CAVA_CAVA_PROP_AUTOSENS:
            astal_cava_cava_set_autosens(self, g_value_get_boolean(value));
            break;
        case ASTAL_CAVA_CAVA_PROP_NOISE:
            astal_cava_cava_set_noise_reduction(self, g_value_get_double(value));
            break;
        case ASTAL_CAVA_CAVA_PROP_STEREO:
            astal_cava_cava_set_stereo(self, g_value_get_boolean(value));
            break;
        case ASTAL_CAVA_CAVA_PROP_FRAMERATE:
            astal_cava_cava_set_framerate(self, g_value_get_int(value));
            break;
        case ASTAL_CAVA_CAVA_PROP_INPUT:
            astal_cava_cava_set_input(self, g_value_get_enum(value));
            break;
        case ASTAL_CAVA_CAVA_PROP_SOURCE:
            astal_cava_cava_set_source(self, g_value_get_string(value));
            break;
        case ASTAL_CAVA_CAVA_PROP_CHANNELS:
            astal_cava_cava_set_channels(self, g_value_get_int(value));
            break;
        case ASTAL_CAVA_CAVA_PROP_LOW_CUTOFF:
            astal_cava_cava_set_low_cutoff(self, g_value_get_int(value));
            break;
        case ASTAL_CAVA_CAVA_PROP_HIGH_CUTOFF:
            astal_cava_cava_set_high_cutoff(self, g_value_get_int(value));
            break;
        case ASTAL_CAVA_CAVA_PROP_SAMPLERATE:
            astal_cava_cava_set_samplerate(self, g_value_get_int(value));
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
        case ASTAL_CAVA_CAVA_PROP_ACTIVE:
            g_value_set_boolean(value, self->active);
            break;
        case ASTAL_CAVA_CAVA_PROP_BARS:
            g_value_set_int(value, self->bars);
            break;
        case ASTAL_CAVA_CAVA_PROP_VALUES:
            g_value_set_boxed(value, self->values);
            break;
        case ASTAL_CAVA_CAVA_PROP_AUTOSENS:
            g_value_set_boolean(value, self->autosens);
            break;
        case ASTAL_CAVA_CAVA_PROP_NOISE:
            g_value_set_double(value, self->noise_reduction);
            break;
        case ASTAL_CAVA_CAVA_PROP_STEREO:
            g_value_set_boolean(value, self->stereo);
            break;
        case ASTAL_CAVA_CAVA_PROP_FRAMERATE:
            g_value_set_int(value, self->framerate);
            break;
        case ASTAL_CAVA_CAVA_PROP_INPUT:
            g_value_set_enum(value, self->input);
            break;
        case ASTAL_CAVA_CAVA_PROP_SOURCE:
            g_value_set_string(value, self->audio_source);
            break;
        case ASTAL_CAVA_CAVA_PROP_CHANNELS:
            g_value_set_int(value, self->channels);
            break;
        case ASTAL_CAVA_CAVA_PROP_LOW_CUTOFF:
            g_value_set_int(value, self->low_cutoff);
            break;
        case ASTAL_CAVA_CAVA_PROP_HIGH_CUTOFF:
            g_value_set_int(value, self->high_cutoff);
            break;
        case ASTAL_CAVA_CAVA_PROP_SAMPLERATE:
            g_value_set_int(value, self->samplerate);
            break;
        default:
            G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
            break;
    }
}

static void astal_cava_cava_constructed(GObject* object) {
    AstalCavaCava* self = ASTAL_CAVA_CAVA(object);
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);

    gdouble* data = calloc(self->bars, sizeof(gdouble));
    memset(data, 0, self->bars * sizeof(gdouble));
    self->values = g_array_new_take(data, self->bars, TRUE, sizeof(gdouble));

    priv->constructed = true;

    if (self->active) astal_cava_cava_start(self);
}

static void astal_cava_cava_init(AstalCavaCava* self) {
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);
    priv->constructed = false;
    self->low_cutoff = 50;
    self->high_cutoff = 10000;
    self->samplerate = 44100;
}

/**
 * astal_cava_get_default
 *
 * gets the default Cava object.
 *
 * Returns: (nullable) (transfer none):
 */
AstalCavaCava* astal_cava_get_default() { return astal_cava_cava_get_default(); }

/**
 * astal_cava_cava_get_default
 *
 * gets the default Cava object.
 *
 * Returns: (nullable) (transfer none):
 */
AstalCavaCava* astal_cava_cava_get_default() {
    static AstalCavaCava* self = NULL;

    if (self == NULL) self = g_object_new(ASTAL_CAVA_TYPE_CAVA, NULL);

    return self;
}

static void astal_cava_cava_dispose(GObject* object) {
    AstalCavaCava* self = ASTAL_CAVA_CAVA(object);
    AstalCavaCavaPrivate* priv = astal_cava_cava_get_instance_private(self);

    self->active = FALSE;
    astal_cava_cava_stop(self, ASTAL_CAVA_STATE_STOPPING);
    if (priv->shutdown_timeout_id) {
        g_source_remove(priv->shutdown_timeout_id);
        priv->shutdown_timeout_id = 0;
    }
    if (priv->worker) {
        AstalCavaWorker* worker = priv->worker;
        priv->worker = NULL;
        priv->state = ASTAL_CAVA_STATE_STOPPED;
        astal_cava_worker_unref(worker);
    }
    G_OBJECT_CLASS(astal_cava_cava_parent_class)->dispose(object);
}

static void astal_cava_cava_finalize(GObject* object) {
    AstalCavaCava* self = ASTAL_CAVA_CAVA(object);

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
     * AstalCavaCava:values: (type GArray(gdouble))
     *
     * A list of values, each represent the height of one bar. The values are generally between 0
     * and 1 but can overshoot occasionally, in which case the sensitivity will be decreased
     * automatically if [property@AstalCava.Cava:autosens] is set. The array will have
     * [property@AstalCava.Cava:bars] entries. If [property@AstalCava.Cava:stereo] is set, the first
     * half of the array will represent the left channel and the second half the right channel, so
     * there will be only bars/2 bars per channel. If the number of bars is odd, the last value will
     * be 0.
     */
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_VALUES] =
        g_param_spec_boxed("values", "values", "a list of values", G_TYPE_ARRAY, G_PARAM_READABLE);
    /**
     * AstalCavaCava:active:
     *
     * whether or not the audio capture and visualization is running. if false the values array will
     * not be updated.
     */
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_ACTIVE] = g_param_spec_boolean(
        "active", "active", "active", TRUE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT);
    /**
     * AstalCavaCava:bars:
     *
     * the number of bars the visualizer should create.
     */
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_BARS] =
        g_param_spec_int("bars", "bars", "number of bars per channel", 1, G_MAXINT, 20,
                         G_PARAM_READWRITE | G_PARAM_CONSTRUCT);
    /**
     * AstalCavaCava:autosens:
     *
     * When set, the sensitivity will automatically be adjusted.
     */
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_AUTOSENS] =
        g_param_spec_boolean("autosens", "autosens", "dynamically adjust sensitivity", TRUE,
                             G_PARAM_READWRITE | G_PARAM_CONSTRUCT);
    /**
     * AstalCavaCava:cava:
     *
     * When set the output will contain visualization data for both channels.
     */
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_STEREO] = g_param_spec_boolean(
        "stereo", "stereo", "stereo", FALSE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT);
    /**
     * AstalCavaCava:noise-reduction:
     *
     * adjusts the noise-reduction filter. low values are fast and noisy, large values are slow and
     * smooth.
     */
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_NOISE] =
        g_param_spec_double("noise_reduction", "noise_reduction", "noise reduction", 0, 1, 0.77,
                            G_PARAM_READWRITE | G_PARAM_CONSTRUCT);
    /**
     * AstalCavaCava:framerate:
     *
     * how often the values should be updated
     */
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_FRAMERATE] =
        g_param_spec_int("framerate", "framerate", "framerate", 1, G_MAXINT, 60,
                         G_PARAM_READWRITE | G_PARAM_CONSTRUCT);
    /**
     * AstalCavaCava:channels:
     *
     * how many input channels to consider
     */
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_CHANNELS] = g_param_spec_int(
        "channels", "channels", "channels", 1, 2, 2, G_PARAM_READWRITE | G_PARAM_CONSTRUCT);
    /**
     * AstalCavaCava:low-cutoff:
     *
     * cut off frequencies below this value
     */
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_LOW_CUTOFF] =
        g_param_spec_int("low-cutoff", "low-cutoff", "lower frequency cutoff", 1, G_MAXINT, 50,
                         G_PARAM_READWRITE | G_PARAM_CONSTRUCT);
    /**
     * AstalCavaCava:high-cutoff:
     *
     * cut off frequencies above this value
     */
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_HIGH_CUTOFF] =
        g_param_spec_int("high-cutoff", "high-cutoff", "higher frequency cutoff", 1, G_MAXINT,
                         10000, G_PARAM_READWRITE | G_PARAM_CONSTRUCT);
    /**
     * AstalCavaCava:samplerate:
     *
     * the samplerate of the input
     */
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_SAMPLERATE] =
        g_param_spec_int("samplerate", "samplerate", "samplerate", 1, G_MAXINT, 44100,
                         G_PARAM_READWRITE | G_PARAM_CONSTRUCT);
    /**
     * AstalCavaCava:input: (type AstalCavaInput)
     *
     * specifies which audio server should be used.
     */
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_INPUT] =
        g_param_spec_enum("input", "input", "input", ASTAL_CAVA_TYPE_INPUT,
                          ASTAL_CAVA_INPUT_PIPEWIRE, G_PARAM_READWRITE | G_PARAM_CONSTRUCT);
    /**
     * AstalCavaCava:source:
     *
     * specifies which audio source should be used. Refer to the cava docs on how to use this
     * property.
     */
    astal_cava_cava_properties[ASTAL_CAVA_CAVA_PROP_SOURCE] = g_param_spec_string(
        "source", "source", "source", "auto", G_PARAM_READWRITE | G_PARAM_CONSTRUCT);
    g_object_class_install_properties(object_class, ASTAL_CAVA_CAVA_N_PROPERTIES,
                                      astal_cava_cava_properties);
}
