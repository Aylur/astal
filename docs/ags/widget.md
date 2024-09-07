# Widget

## AGS widget properties

These are properties that Astal.js additionally adds to Gtk.Widgets

- className: `string` - List of class CSS selectors separated by white space.
- css: `string` - Inline CSS. e.g `label { color: white; }`. If no selector is specified `*` will be assumed. e.g `color: white;` will be inferred as `* { color: white; }`.
- cursor: `string` - Cursor style when hovering over widgets that have hover states, e.g it won't work on labels. [list of valid values](https://docs.gtk.org/gdk3/ctor.Cursor.new_from_name.html).
- clickThrough: `boolean` - Lets click events through.

To have a full list of available properties, reference the documentation of the widget.

- [Astal widgets](https://aylur.github.io/libastal/index.html#classes)
- [Gtk widgets](https://docs.gtk.org/gtk3/#classes)

You can check the [source code](https://github.com/aylur/astal/blob/main/gjs/src/widgets.ts) to have a full list of builtin widgets.

## AGS widget methods

Additional methods that Astal.js adds to Gtk.Widget instances

### setup

`setup` is a convenience prop to not have predefine widgets before returning them

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

### hook

Shorthand for connection and disconnecting to gobjects.

without `hook`

```tsx
function MyWidget() {
    const id = gobject.connect("signal", callback)

    return <box
        onDestroy={() => {
            gobject.disconnect(id)
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
        }}
    />
}
```

### toggleClassName

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

## How to use non builtin Gtk widgets

Using `Widget.astalify` you can setup widget constructors to behave like builtin widgets.
The `astalify` function will apply the following:

- set `visible` to true by default (Gtk3 widgets are invisible by default)
- make gobject properties accept and consume `Binding` objects
- add properties and methods listed above
- proxify the constructor so the `new` keyword is not needed
- sets up signal handlers that are passed as props prefixed with `on`

```tsx
import { Widget, Gtk } from "astal"

// define its props, constructor and type
export type ColorButtonProps = Widget.ConstructProps<
    Gtk.ColorButton,
    Gtk.ColorButton.ConstructorProps,
    { onColorSet: [] }
>
export const ColorButton = Widget.astalify<
    typeof Gtk.ColorButton,
    ColorButtonProps,
    "ColorButton"
>(Gtk.ColorButton)
export type ColorButton = ReturnType<typeof ColorButton>

function MyWidget() {
    function setup(button: ColorButton) {}

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

## TypeScript

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
