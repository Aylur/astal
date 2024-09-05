#include "astal-river.h"

#include <getopt.h>
#include <json-glib/json-glib.h>
#include <stdlib.h>

#include "gio/gio.h"

GMainLoop* loop;

void print_json(AstalRiverRiver* river) {
    JsonNode* json = json_gobject_serialize(G_OBJECT(river));

    gchar* json_str = json_to_string(json, FALSE);
    g_print("%s\n", json_str);
    g_free(json);
}

int main(int argc, char** argv) {
    gboolean daemon = FALSE;

    int opt;
    const char* optstring = "d";

    static struct option long_options[] = {{"daemon", no_argument, NULL, 'd'}, {NULL, 0, NULL, 0}};

    while ((opt = getopt_long(argc, argv, optstring, long_options, NULL)) != -1) {
        switch (opt) {
            case 'd':
                daemon = TRUE;
                break;
            default:
                g_print("Usage: %s [-d]\n", argv[0]);
                exit(EXIT_FAILURE);
        }
    }

    GError* error = NULL;
    AstalRiverRiver* river = g_initable_new(ASTAL_RIVER_TYPE_RIVER, NULL, &error, NULL);
    if (error) {
        g_critical("%s\n", error->message);
        exit(EXIT_FAILURE);
    }
    if (daemon) {
        loop = g_main_loop_new(NULL, FALSE);
        g_signal_connect(river, "changed", G_CALLBACK(print_json), NULL);
        g_main_loop_run(loop);
    } else {
        print_json(river);
        g_object_unref(river);
    }
}
