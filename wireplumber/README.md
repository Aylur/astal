# astal-wireplumber

A libwireplumber wrapper.

## Build from source
### Dependencies

- meson
- libwireplumber
- glib
- gobject-introspection
- vala (only required for the vapi option)

### Meson options

* `-Dintrospection` (default: `true`): build GObject Introspection data (needed for language bindings)
* `-Dvapi` (default: `true`): build VAPI data (required to make this lib usable in vala). Requires `-Dintrospection=true`

### build instructions

```sh
# Clone the repository
git clone https://github.com/astal-sh/wireplumber
cd wireplumber

# Setup and build
meson setup build
meson compile -C build

# Install
meson install -C build
```


