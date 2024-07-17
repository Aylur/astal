#ifndef ASTAL_AUTH_PAM_H
#define ASTAL_AUTH_PAM_H

#include <gio/gio.h>
#include <glib-object.h>

G_BEGIN_DECLS

#define ASTAL_AUTH_TYPE_PAM (astal_auth_pam_get_type())

G_DECLARE_FINAL_TYPE(AstalAuthPam, astal_auth_pam, ASTAL_AUTH, PAM, GObject)

/**
 * astal_auth_pam_set_username
 * @self: a AstalAuthPam object
 * @username: the new username
 *
 * Sets the username to be used for authentication. This must be set to
 * before calling start_authenticate.
 * Changing it afterwards has no effect on the authentication process.
 *
 * Defaults to the owner of the process.
 *
 */
void astal_auth_pam_set_username(AstalAuthPam *self, const gchar *username);

/**
 * astal_auth_pam_get_username
 * @self: a AstalAuthPam object
 *
 * Fetches the username from AsalAuthPam object.
 *
 * Returns: the username of the AsalAuthPam object. This string is
 * owned by the object and must not be modified or freed.
 */
const gchar *astal_auth_pam_get_username(AstalAuthPam *self);

/**
 * astal_auth_pam_set_service
 * @self: a AstalAuthPam object
 * @service: the pam service used for authentication
 *
 * Sets the service to be used for authentication. This must be set to
 * before calling start_authenticate.
 * Changing it afterwards has no effect on the authentication process.
 *
 * Defaults to `astal-auth`.
 *
 */
void astal_auth_pam_set_service(AstalAuthPam *self, const gchar *service);

/**
 * astal_auth_pam_get_service
 * @self: a AstalAuthPam
 *
 * Fetches the service from AsalAuthPam object.
 *
 * Returns: the service of the AsalAuthPam object. This string is
 * owned by the object and must not be modified or freed.
 */
const gchar *astal_auth_pam_get_service(AstalAuthPam *self);

/**
 * astal_auth_pam_start_authentication:
 * @self: a AstalAuthPam Object
 *
 * starts a new authentication process using the PAM (Pluggable Authentication Modules) system.
 * Note that this will cancel an already running authentication process
 * associated with this AstalAuthPam object.
 */
gboolean astal_auth_pam_start_authenticate(AstalAuthPam *self);

/**
 * astal_auth_pam_supply_secret
 * @self: a AstalAuthPam Object
 * @secret: (nullable): the secret to be provided to pam. Can be NULL.
 *
 * provides pam with a secret. This method must be called exactly once after a
 * auth-* signal is emitted.
 */
void astal_auth_pam_supply_secret(AstalAuthPam *self, const gchar *secret);

/**
 * astal_auth_pam_authenticate:
 * @password: the password to be authenticated
 * @result_callback: (scope async) (closure user_data): a GAsyncReadyCallback
 *   to call when the request is satisfied
 * @user_data: the data to pass to callback function
 *
 * Requests authentication of the provided password using the PAM (Pluggable Authentication Modules)
 * system.
 */
gboolean astal_auth_pam_authenticate(const gchar *password, GAsyncReadyCallback result_callback,
                                     gpointer user_data);

gssize astal_auth_pam_authenticate_finish(GAsyncResult *res, GError **error);

G_END_DECLS

#endif  // !ASTAL_AUTH_PAM_H
