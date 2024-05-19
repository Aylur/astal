# libastal-auth
This library provides a way for authentication using pam for the libastal suite.

## Build from source
### Dependencies

- meson
- glib
- gobject-introspection
- pam
- vala (only required for the vapi option)

### Meson options

* `-Dintrospection` (default: `true`): build GObject Introspection data (needed for language bindings)
* `-Dvapi` (default: `true`): build VAPI data (required to make this lib usable in vala). Requires `-Dintrospection=true`
* `-Dexamples` (default: `false`): build examples

```sh
# Clone the repository
git clone https://github.com/astal-sh/libastal-auth
cd libastal-auth

# Setup and build
meson setup build
meson compile -C build

# Install
meson install -C build
```

## Usage
This library can be used from any language supporting GObject Introspection.
Have a look at the [examples](examples) for how it can be used in C and gjs.

The authentication is done asynchronously in its own thread, therefore the GLib mainloop is required to run.
This is already given in all gtk application, but has to be started manually in some cases like in the small examples in this repo.

Until there are better docs, please refer to the [auth.h](include/auth.h) file for detailed usage.

For simple authentication using only a password, using the `Pam.authenticate()` method is recommended.
Look at the simple examples for how to use it.

There is also a way to get access to the pam conversation, to allow for a more complex authentication process, like using multiple factor authentication.
The full examples show how this can be achieved.
Generally it can be used like this:

1. create the Pam object.
2. set username and service if so required. It has sane defaults, so in most cases you can skip this.
3. connect to the signals
   - `auth-prompt-hidden`: is emitted when user input is required, and the input should be hidden (eg, passwords)
   - `auth-prompt-visible`: is emitted when user input is required, and the input should be visible (eg, OTP)
   - `auth-info`: an information message should be displayed (eg, tell the user to touch his security key)
   - `auth-error`: an error message should be displayed
   - `sucess`: emitted on successful authentication
   - `fail`: emitted on failed authentication

   all signals except the `success` signal have a string containing the message as a parameter.
   After an `auth-*` signal is emitted, it hs to be responded with exactly one `pam.supply_secret(secret)` call. The secret is a string containing the user input. For `auth-info` and `auth-error` it can be `NULL`.
   Not connecting those signals, is equivalent to calling `pam.supply_secret(NULL)` immediately after the signal is emitted. 
4. start authentication process using `Pam.start_authentication()`. This function will return whether the authentication was started or not.
5. it is possible to reuse the same Pam object for multiple sequential authentication attempts. Just call `pam.start_authentication()` again after the `success` or `fail` signal was emitted.
 
## License

This project is licensed under the GPL-3 License - see the LICENSE file for details.

