#include "astal-auth.h"

#include <getopt.h>
#include <stdio.h>
#include <termios.h>

GMainLoop *loop;

static void cleanup_and_quit(AstalAuthPam *pam, int status) {
    g_object_unref(pam);
    g_main_loop_quit(loop);
    exit(status);
}

static char *read_secret(const char *msg, gboolean echo) {
    struct termios oldt, newt;
    char *password = NULL;
    size_t size = 0;
    ssize_t len;

    if (tcgetattr(STDIN_FILENO, &oldt) != 0) {
        return NULL;
    }
    newt = oldt;
    if (echo) {
        newt.c_lflag |= ECHO;
    } else {
        newt.c_lflag &= ~(ECHO);
    }
    if (tcsetattr(STDIN_FILENO, TCSANOW, &newt) != 0) {
        return NULL;
    }
    g_print("%s", msg);
    if ((len = getline(&password, &size, stdin)) == -1) {
        g_free(password);
        return NULL;
    }

    if (password[len - 1] == '\n') {
        password[len - 1] = '\0';
    }

    printf("\n");

    if (tcsetattr(STDIN_FILENO, TCSANOW, &oldt) != 0) {
        return NULL;
    }

    return password;
}

static void authenticate(AstalAuthPam *pam) {
    static int attempts = 0;
    if (attempts >= 3) {
        g_print("%d failed attempts.\n", attempts);
        cleanup_and_quit(pam, EXIT_FAILURE);
    }
    if (!astal_auth_pam_start_authenticate(pam)) {
        g_print("could not start authentication process\n");
        cleanup_and_quit(pam, EXIT_FAILURE);
    }
    attempts++;
}

static void on_visible(AstalAuthPam *pam, const gchar *data) {
    char *secret = read_secret(data, TRUE);
    if (secret == NULL) cleanup_and_quit(pam, EXIT_FAILURE);
    astal_auth_pam_supply_secret(pam, secret);
    g_free(secret);
}

static void on_hidden(AstalAuthPam *pam, const gchar *data, gchar *secret) {
    if (!secret) secret = read_secret(data, FALSE);
    if (secret == NULL) cleanup_and_quit(pam, EXIT_FAILURE);
    astal_auth_pam_supply_secret(pam, secret);
    g_free(secret);
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
    g_print("Authentication successful\n");
    cleanup_and_quit(pam, EXIT_SUCCESS);
}

static void on_fail(AstalAuthPam *pam, const gchar *data, gboolean retry) {
    g_print("%s\n", data);
    if (retry)
        authenticate(pam);
    else
        cleanup_and_quit(pam, EXIT_FAILURE);
}

int main(int argc, char **argv) {
    char *password = NULL;
    char *username = NULL;
    char *service = NULL;

    int opt;
    const char *optstring = "p:u:s:";

    static struct option long_options[] = {{"password", required_argument, NULL, 'p'},
                                           {"username", required_argument, NULL, 'u'},
                                           {"service", required_argument, NULL, 's'},
                                           {NULL, 0, NULL, 0}};

    while ((opt = getopt_long(argc, argv, optstring, long_options, NULL)) != -1) {
        switch (opt) {
            case 'p':
                password = optarg;
                break;
            case 'u':
                username = optarg;
                break;
            case 's':
                service = optarg;
                break;
            default:
                g_print("Usage: %s [-p password] [-u username] [-s service]\n", argv[0]);
                exit(EXIT_FAILURE);
        }
    }

    loop = g_main_loop_new(NULL, FALSE);

    AstalAuthPam *pam = g_object_new(ASTAL_AUTH_TYPE_PAM, NULL);

    if (username) astal_auth_pam_set_username(pam, username);
    if (service) astal_auth_pam_set_service(pam, service);
    if (password) {
        g_signal_connect(pam, "fail", G_CALLBACK(on_fail), (void *)FALSE);
    } else {
        g_signal_connect(pam, "auth-prompt-visible", G_CALLBACK(on_visible), NULL);
        g_signal_connect(pam, "auth-info", G_CALLBACK(on_info), NULL);
        g_signal_connect(pam, "auth-error", G_CALLBACK(on_error), NULL);
        g_signal_connect(pam, "fail", G_CALLBACK(on_fail), (void *)TRUE);
    }

    g_signal_connect(pam, "auth-prompt-hidden", G_CALLBACK(on_hidden), g_strdup(password));
    g_signal_connect(pam, "success", G_CALLBACK(on_success), NULL);

    authenticate(pam);

    g_main_loop_run(loop);
}
