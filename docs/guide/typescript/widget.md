# Widget

## Gtk3

### Additional widget properties

These are properties that Astal additionally adds to Gtk.Widgets

- className: `string` - List of class CSS selectors separated by white space.
- css: `string` - Inline CSS. e.g `label { color: white; }`. If no selector is specified `*` will be assumed. e.g `color: white;` will be inferred as `* { color: white; }`.
- cursor: `string` - Cursor style when hovering over widgets that have hover states, e.g it won't work on labels. [list of valid values](https://docs.gtk.org/gdk3/ctor.Cursor.new_from_name.html).
- clickThrough: `boolean` - Lets click events through.

To have a full list of available properties, reference the documentation of the widget.

- [Astal3 widgets](https://aylur.github.io/libastal/astal3/index.html#classes)
- [Gtk widgets](https://docs.gtk.org/gtk3/#classes)

### Additional widget methods

#### setup

`setup` is a convenience prop to remove the need to predefine widgets
before returning them in cases where a reference is needed.

without `setup`

```tsx
function MyWidget() {
    const button = Widget.Button()
    // setup button
    return button
}
```

using `setup`

```tsx
function MyWidget() {
    function setup(button: Widget.Button) {
        // setup button
    }

    return <buttons setup={setup} />
}
```

#### hook

Shorthand for connection and disconnecting to [Subscribable and Connectable](./binding#subscribable-and-connectable-interface) objects.

without `hook`

```tsx
function MyWidget() {
    const id = gobject.connect("signal", callback)
    const unsub = variable.subscribe(callback)

    return <box
        onDestroy={() => {
            gobject.disconnect(id)
            unsub()
        }}
    />
}
```

with `hook`

```tsx
function MyWidget() {
    return <box
        setup={(self) => {
            self.hook(gobject, "signal", callback)
            self.hook(variable, callback)
        }}
    />
}
```

#### toggleClassName

Toggle classNames based on a condition

```tsx
function MyWidget() {
    return <box
        setup={(self) => {
            self.toggleClassName("classname", someCondition)
        }}
    />
}
```

### How to use non builtin Gtk widgets

Using the `Widget.astalify` mixin you can subclass widgets
to behave like builtin widgets.
The `astalify` mixin will apply the following:

- set `visible` to true by default (Gtk3 widgets are invisible by default)
- make gobject properties accept and consume `Binding` objects
- add properties and methods listed above
- sets up signal handlers that are passed as props prefixed with `on`

```tsx
import { Widget, Gtk, GObject, Gdk } from "astal/gtk3"

// subclass, register, define constructor props
class ColorButton extends Widget.astalify(Gtk.ColorButton) {
    static { GObject.registerClass(this) }

    constructor(props: Widget.ConstructProps<
        ColorButton,
        Gtk.ColorButton.ConstructorProps,
        { onColorSet: [] } // signals of Gtk.ColorButton have to be manually typed
    >) {
        super(props as any)
    }
}

function MyWidget() {
    function setup(button: ColorButton) {

    }

    return <ColorButton
        setup={setup}
        useAlpha
        rgba={new Gdk.RGBA({
            red: 1,
            green: 0,
            blue: 0,
            alpha: 0.5,
        })}
        onColorSet={(self) => {
            console.log(self.rgba)
        }}
    />
}
```

:::info
Signal properties have to be annotated manually for TypeScript.
You can reference [Gtk3](https://gjs-docs.gnome.org/gtk30~3.0/)
and [Astal](https://aylur.github.io/libastal/index.html#classes) for available signals.
:::

### TypeScript

Type of widgets are available through `Widget`.
Here is an example Widget that takes in and handles a possibly `Binding` prop.

```tsx
import { Binding, Variable, Widget } from "astal"

export interface ToggleButtonProps extends Widget.ButtonProps {
    onToggled?: (self: Widget.Button, on: boolean) => void
    state?: Binding<boolean> | boolean
    child?: JSX.Element
}

export default function ToggleButton(btnprops: ToggleButtonProps) {
    const { state = false, onToggled, setup, child, ...props } = btnprops
    const innerState = Variable(state instanceof Binding ? state.get() : state)

    return <button
        {...props}
        setup={self => {
            setup?.(self)

            self.toggleClassName("active", innerState.get())
            self.hook(innerState, () => self.toggleClassName("active", innerState.get()))

            if (state instanceof Binding) {
                self.hook(state, () => innerState.set(state.get()))
            }
        }}
        onClicked={self => {
            onToggled?.(self, !innerState.get())
        }}
    >
        {child}
    </button>
}
```

### Builtin Widgets

You can check the [source code](https://github.com/aylur/astal/blob/main/lang/gjs/src/gtk3/index.ts) to have a full list of builtin widgets.

These widgets are available by default in JSX.

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
