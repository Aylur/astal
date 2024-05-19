#include "pam.h"
#include <bsd/readpassphrase.h>

GMainLoop *loop;

static void authenticate(AstalAuthPam *pam) {
    if(!astal_auth_pam_start_authenticate(pam)) {
        g_print("could not start authentication process\n");
        g_object_unref(pam);
        g_main_loop_quit(loop);
    }
}

static void on_visible(AstalAuthPam *pam, const gchar *data) {
    gchar passbuf[1024];
    readpassphrase(data, passbuf, sizeof(passbuf), RPP_ECHO_ON);
    astal_auth_pam_supply_secret(pam, passbuf);
}

static void on_hidden(AstalAuthPam *pam, const gchar *data) {
    gchar passbuf[1024];
    readpassphrase(data, passbuf, sizeof(passbuf), RPP_ECHO_OFF);
    astal_auth_pam_supply_secret(pam, passbuf);
}

static void on_info(AstalAuthPam *pam, const gchar *data) {
    g_print("info: %s\n", data);
    astal_auth_pam_supply_secret(pam, NULL);
}

static void on_error(AstalAuthPam *pam, const gchar *data) {
    g_print("error: %s\n", data);
    astal_auth_pam_supply_secret(pam, NULL);
}

static void on_success(AstalAuthPam *pam) {
    g_print("success\n");
    g_object_unref(pam);
    g_main_loop_quit(loop);
}

static void on_fail(AstalAuthPam *pam, const gchar *data) {
    g_print("fail: %s\n", data);
    authenticate(pam);
}


int main(void) {

    GMainContext *loopctx = NULL;

    loop = g_main_loop_new(loopctx, FALSE);

    AstalAuthPam *pam = g_object_new(ASTAL_AUTH_TYPE_PAM, NULL);

    g_signal_connect(pam, "auth-prompt-visible", G_CALLBACK(on_visible), NULL);
    g_signal_connect(pam, "auth-prompt-hidden", G_CALLBACK(on_hidden), NULL);
    g_signal_connect(pam, "auth-info", G_CALLBACK(on_info), NULL);
    g_signal_connect(pam, "auth-error", G_CALLBACK(on_error), NULL);

    g_signal_connect(pam, "success", G_CALLBACK(on_success), NULL);
    g_signal_connect(pam, "fail", G_CALLBACK(on_fail), NULL);

    authenticate(pam);

    g_main_loop_run(loop);
}
