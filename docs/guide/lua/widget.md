# Widget

## Gtk3

### Additional widget properties

These are properties that Astal additionally adds to Gtk.Widgets

- class_name: `string` - List of class CSS selectors separated by white space.
- css: `string` - Inline CSS. e.g `label { color: white; }`. If no selector is specified `*` will be assumed. e.g `color: white;` will be inferred as `* { color: white; }`.
- cursor: `string` - Cursor style when hovering over widgets that have hover states, e.g it won't work on labels. [list of valid values](https://docs.gtk.org/gdk3/ctor.Cursor.new_from_name.html).
- click_through: `boolean` - Lets click events through.

To have a full list of available properties, reference the documentation of the widget.

- [Astal3 widgets](https://aylur.github.io/libastal/astal3/index.html#classes)
- [Gtk widgets](https://docs.gtk.org/gtk3/#classes)

### Additional widget methods

#### setup

`setup` is a convenience prop to remove the need to predefine widgets
before returning them in cases where a reference is needed.

without `setup`

```lua
local function MyWidget()
  local button = Widget.Button()
  -- setup button
  return button
end
```

using `setup`

```lua
local function MyWidget()
  return Widget.Button({
    setup = function(self)
      -- setup button
    end
  })
end
```

#### hook

Shorthand for connection and disconnecting to [Subscribable and Connectable](./binding#subscribable-and-connectable-interface) objects.

without `hook`

```lua
local function MyWidget()
  local id = gobject.on_signal:connect(callback)
  local unsub = variable:subscribe(callback)

  return Widget.Box({
    on_destroy = function()
      GObject.signal_handler_disconnect(gobject, id)
      unsub()
    end
  })
end
```

with `hook`

```lua
local function MyWidget()
  return Widget.Box({
    setup = function(self)
      self:hook(gobject, "signal", callback)
      self:hook(variable, callback)
    end
  })
end
```

#### toggle_class_name

Toggle class_names based on a condition

```lua
local function MyWidget()
  return Widget.Box({
    setup = function(self)
      self:toggle_class_name("classname", some_condition)
    end
  })
end
```

### How to use non builtin Gtk widgets

Using the `Widget.astalify` mixin you can subclass widgets
to behave like builtin widgets.
The `astalify` mixin will apply the following:

- set `visible` to true by default (Gtk3 widgets are invisible by default)
- make gobject properties accept and consume `Binding` objects
- add properties and methods listed above
- sets up signal handlers that are passed as props prefixed with `on`

```lua
local astal = require("astal")
local Widget = require("astal.gtk3.widget")
local Gtk = astal.require("Gtk", "3.0")
local Gdk = astal.require("Gdk", "3.0")

local ColorButton = Widget.astalify(Gtk.ColorButton)

local function MyWidget()
  return ColorButton({
    setup = function(self) end,
    use_alpha = true,
    rgba = Gdk.RGBA({ red = 1, green = 0, blue = 0, alpha = 0.5 }),
    on_color_set = function(self)
        print(self.rgba:to_string())
    end
  })
end
```

### Builtin Widgets

You can check the [source code](https://github.com/Aylur/astal/blob/main/lang/lua/astal/gtk3/init.lua) to have a full list of builtin widgets.

These widgets are available by default in Lua.

- box: [Astal.Box](https://aylur.github.io/libastal/astal3/class.Box.html)
- button: [Astal.Button](https://aylur.github.io/libastal/astal3/class.Button.html)
- centerbox: [Astal.CenterBox](https://aylur.github.io/libastal/astal3/class.CenterBox.html)
- circularprogress: [Astal.CircularProgress](https://aylur.github.io/libastal/astal3/class.CircularProgress.html)
- drawingarea: [Gtk.DrawingArea](https://docs.gtk.org/gtk3/astal3/class.DrawingArea.html)
- entry: [Gtk.Entry](https://docs.gtk.org/gtk3/astal3/class.Entry.html)
- eventbox: [Astal.EventBox](https://aylur.github.io/libastal/astal3/class.EventBox.html)
- icon: [Astal.Icon](https://aylur.github.io/libastal/astal3/class.Icon.html)
- label: [Astal.Label](https://aylur.github.io/libastal/astal3/class.Label.html)
- levelbar: [Astal.LevelBar](https://aylur.github.io/libastal/astal3/class.LevelBar.html)
- overlay: [Astal.Overlay](https://aylur.github.io/libastal/astal3/class.Overlay.html)
- revealer: [Gtk.Revealer](https://docs.gtk.org/gtk3/astal3/class.Revealer.html)
- scrollable: [Astal.Scrollable](https://aylur.github.io/libastal/astal3/class.Scrollable.html)
- slider: [Astal.Slider](https://aylur.github.io/libastal/astal3/class.Slider.html)
- stack: [Astal.Stack](https://aylur.github.io/libastal/astal3/class.Stack.html)
- switch: [Gtk.Switch](https://docs.gtk.org/gtk3/astal3/class.Switch.html)
- window: [Astal.Window](https://aylur.github.io/libastal/astal3/class.Window.html)

## Gtk4

🚧 Work in Progress 🚧
