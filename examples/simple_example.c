#include <bsd/readpassphrase.h>

#include "auth.h"

GMainLoop *loop;

void ready_callback(AstalAuthPam *pam, GAsyncResult *res, gpointer user_data) {
    GError *error = NULL;
    astal_auth_pam_authenticate_finish(res, &error);
    if (error == NULL) {
        g_print("success\n");
    } else {
        g_print("failure: %s\n", error->message);
        g_error_free(error);
    }

    g_main_loop_quit(loop);
}

int main(void) {
    GMainContext *loopctx = NULL;
    loop = g_main_loop_new(loopctx, FALSE);

    gchar *passbuf = calloc(1024, sizeof(gchar));
    readpassphrase("Password: ", passbuf, 1024, RPP_ECHO_OFF);
    astal_auth_pam_authenticate(passbuf, (GAsyncReadyCallback)ready_callback, NULL);
    g_free(passbuf);

    g_main_loop_run(loop);
    exit(EXIT_SUCCESS);
}
