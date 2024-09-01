# libastal-tray
a library for managing the systemtray by implementing the StatusNotifierItem dbus protocol.

## Build from source
### Dependencies

- meson
- glib
- gdk-pixbuf
- gtk3
- gobject-introspection
- dbusemenu-gtk3
- vala

```sh
# Clone the repository
git clone https://github.com/astal-sh/tray
cd tray

# Setup and build
meson setup build
meson compile -C build

# Install
meson install -C build
```

## Todo
- docs
- cli tool
