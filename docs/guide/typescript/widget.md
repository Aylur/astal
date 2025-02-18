# Widget

## Gtk3

### Additional widget properties

These are properties that Astal additionally adds to Gtk.Widgets

- `className`: `string` - List of class CSS selectors separated by white space.
- `css`: `string` - Inline CSS. e.g `label { color: white; }`. If no selector is specified `*` will be assumed. e.g `color: white;` will be inferred as `* { color: white; }`.
- `cursor`: `string` - Cursor style when hovering over widgets that have hover states, e.g it won't work on labels. [list of valid values](https://docs.gtk.org/gdk3/ctor.Cursor.new_from_name.html).
- `clickThrough`: `boolean` - Lets click events through.

To have a full list of available properties, reference the documentation of the widget.

- [Astal3 widgets](https://aylur.github.io/libastal/astal3/index.html#classes)
- [Gtk3 widgets](https://docs.gtk.org/gtk3/#classes)

Most common ones you will use frequently are
  - [halign](https://docs.gtk.org/gtk3/property.Widget.halign.html)
  - [valign](https://docs.gtk.org/gtk3/property.Widget.valign.html)
  - [hexpand](https://docs.gtk.org/gtk3/property.Widget.hexpand.html)
  - [vexpand](https://docs.gtk.org/gtk3/property.Widget.vexpand.html)

### Additional widget methods

#### setup

`setup` is a convenience prop to remove the need to predefine widgets
before returning them in cases where a reference is needed.

without `setup`

```tsx
function MyWidget() {
    const button = new Widget.Button()
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

Using the `astalify` mixin you can subclass widgets
to behave like builtin widgets.
The `astalify` mixin will apply the following:

- set `visible` to true by default (Gtk3 widgets are invisible by default)
- make gobject properties accept and consume `Binding` objects
- add properties and methods listed above
- set up signal handlers that are passed as props prefixed with `on`

```tsx
import GObject from "gi://GObject"
import { Gtk, Gdk, Widget, astalify, type ConstructProps } from "astal/gtk3"

// subclass, register, define constructor props
class ColorButton extends astalify(Gtk.ColorButton) {
    static { GObject.registerClass(this) }

    constructor(props: ConstructProps<
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
            print(self.rgba)
        }}
    />
}
```

:::info
Signal properties have to be annotated manually for TypeScript.
You can reference [Gtk3](https://gjs-docs.gnome.org/gtk30~3.0/)
and [Astal3](https://aylur.github.io/libastal/astal3/#classes) for available signals.
:::

### TypeScript

Type of widgets are available through `Widget`.
Here is an example Widget that takes in and handles a possibly `Binding` prop.

```tsx
import { Binding, Variable } from "astal"
import { Widget } from "astal/gtk3"

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

These widgets are available by default in JSX.

- box: [Astal.Box](https://aylur.github.io/libastal/astal3/class.Box.html)
  ```tsx
  <box>Horizontal Box</box>
  ```
  ```tsx
  <box orientation={1}>Vertical Box</box>
  ```
- button: [Astal.Button](https://aylur.github.io/libastal/astal3/class.Button.html)
  ```tsx
  <button onClicked={self => print(self, "was clicked")}>
      Click Me
  </button>
  ```
- centerbox: [Astal.CenterBox](https://aylur.github.io/libastal/astal3/class.CenterBox.html)
  ```tsx
  <centerbox orientation={1}>
      <label vexpand valign={Gtk.Align.START} label="Start Widget" />
      <label label="Center Widget" />
      <label vexpand valign={Gtk.Align.END} label="End Widget" />
  </box>
  ```
- circularprogress: [Astal.CircularProgress](https://aylur.github.io/libastal/astal3/class.CircularProgress.html)
  ```tsx
  <circularprogress value={.5} startAt={0.75} endAt={0.75}>
      <icon />
  </circularprogress>
  ```
  ```css
  circularprogress {
      color: green;
      background-color: black;
      font-size: 6px;
      margin: 2px;
      min-width: 32px;
  }
  ```

- drawingarea: [Gtk.DrawingArea](https://docs.gtk.org/gtk3/class.DrawingArea.html)
  ```tsx
  <drawingarea onDraw={drawingFunction} />
  ```

- entry: [Gtk.Entry](https://docs.gtk.org/gtk3/class.Entry.html)
  ```tsx
  <window keymode={Astal.Keymode.ON_DEMAND}>
      <entry
          onChanged={self => print("text changed", self.text)}
          onActivate={self => print("enter", self.text)}
      />
  </window>
  ```

- eventbox: [Astal.EventBox](https://aylur.github.io/libastal/astal3/class.EventBox.html)
  ```tsx
  <eventbox
      onClick={(_, event) => {
          print(event.modifier, event.button)
      }}
  />
  ```

- icon: [Astal.Icon](https://aylur.github.io/libastal/astal3/class.Icon.html)
  ```tsx
  <icon icon={GLib.get_os_info("LOGO") || "missing-symbolic"} />
  ```
  ```css
  icon {
    font-size: 16px;
  }
  ```

- label: [Astal.Label](https://aylur.github.io/libastal/astal3/class.Label.html)
  ```tsx
  <label label="hello" maxWidthChars={16} wrap />
  ```

- levelbar: [Astal.LevelBar](https://aylur.github.io/libastal/astal3/class.LevelBar.html)
  ```tsx
  <levelbar value={0.5} widthRequest={200} />
  ```

- overlay: [Astal.Overlay](https://aylur.github.io/libastal/astal3/class.Overlay.html)
  ```tsx
  <overlay>
      <box heightRequest={40} widthRequest={40}>Child</box>
      <box className="overlay" valign={Gtk.Align.START} halign={Gtk.Align.END}>1</box>
  </overlay>
  ```

- revealer: [Gtk.Revealer](https://docs.gtk.org/gtk3/class.Revealer.html)
  ```tsx
  <revealer
      setup={self => timeout(500, () => self.revealChild = true)}
      transitionType={Gtk.RevealerTransitionType.SLIDE_UP}>
      <label label="Child" />
  </revealer>
  ```

- scrollable: [Astal.Scrollable](https://aylur.github.io/libastal/astal3/class.Scrollable.html)
  ```tsx
  <scrollable heightRequest={100}>
      <box orientation={1}>
          {Array.from({ length: 10 }, (_, i) => (
              <button>{i}</button>
          ))}
      </box>
  </scrollable>
  ```

- slider: [Astal.Slider](https://aylur.github.io/libastal/astal3/class.Slider.html)
  ```tsx
  <slider widthRequest={100} onDragged={self => print("new value", self.value)} />
  ```

- stack: [Astal.Stack](https://aylur.github.io/libastal/astal3/class.Stack.html)
  ```tsx
  <stack visibleChildName="child2">
      <label name="child1" label="child1" />
      <label name="child2" label="child2" />
  </stack>
  ```

- switch: [Gtk.Switch](https://docs.gtk.org/gtk3/class.Switch.html)
  ```tsx
  <switch onNotifyActive={self => print(self.active)} />
  ```

- window: [Astal.Window](https://aylur.github.io/libastal/astal3/class.Window.html)
  ```tsx
  <window
      className="Bar"
      name="bar"
      namespace="bar"
      application={App}
      monitor={0}
      anchor={Astal.WindowAnchor.TOP | Astal.WindowAnchor.LEFT}
      exclusivity={Astal.Exclusivity.EXCLUSIVE}
      keymode={Astal.Keymode.ON_DEMAND}
  >
      <centerbox />
  </window>
  ```

## Gtk4

The Gtk4 js library does not add any additional properties to the widgets,
but it still has some additional properties that the constructors handle.

- `type`: `string` an arbitrary string that the [Buildable](https://docs.gtk.org/gtk4/iface.Buildable.html) interface uses.
- event handlers for [EventControllers](https://docs.gtk.org/gtk4/class.EventController.html)
  ```ts
  type EventController<Self extends Gtk.Widget> = {
      onFocusEnter?: (self: Self) => void
      onFocusLeave?: (self: Self) => void

      onKeyPressed?: (self: Self, keyval: number, keycode: number, state: Gdk.ModifierType) => void
      onKeyReleased?: (self: Self, keyval: number, keycode: number, state: Gdk.ModifierType) => void
      onKeyModifier?: (self: Self, state: Gdk.ModifierType) => void

      onLegacy?: (self: Self, event: Gdk.Event) => void
      onButtonPressed?: (self: Self, state: Gdk.ButtonEvent) => void
      onButtonReleased?: (self: Self, state: Gdk.ButtonEvent) => void

      onHoverEnter?: (self: Self, x: number, y: number) => void
      onHoverLeave?: (self: Self) => void
      onMotion?: (self: Self, x: number, y: number) => void

      onScroll?: (self: Self, dx: number, dy: number) => void
      onScrollDecelerate?: (self: Self, vel_x: number, vel_y: number) => void
  }
  ```

- `setup`: `(self): void` setup function that runs after constructor
  ```tsx
  // without `setup`
  function MyWidget() {
      const button = Widget.Button()
      // setup button
      return button
  }

  // using `setup`
  function MyWidget() {
      function setup(button: Widget.Button) {
          // setup button
      }

      return <buttons setup={setup} />
  }
  ```

There is also a `hook` utility

```tsx
// without `hook`
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

// with `hook`
import { hook } from "astal/gtk4"

function MyWidget() {
    return <box
        setup={(self) => {
            hook(self, gobject, "signal", callback)
            hook(self, variable, callback)
        }}
    />
}
```

### How to use non builtin Gtk widgets

Using the `astalify` function you can create wrappers around widget constructors
to make them behave like builtin widgets.
The `astalify` function will do the followings:

- make `gobject` properties accept and consume `Binding` objects
- handle properties listed above
- set up signal handlers that are passed as props prefixed with `on`

```tsx
import GObject from "gi://GObject"
import { Gtk, astalify, type ConstructProps } from "astal/gtk4"

type CalendarProps = ConstructProps<Gtk.Calendar, Gtk.Calendar.ConstructorProps>
const Calendar = astalify<Gtk.Calendar, Gtk.Calendar.ConstructorProps>(Gtk.Calendar, {
    // if it is a container widget, define children setter and getter here
    getChildren(self) { return [] },
    setChildren(self, children) {},
})

function MyWidget() {
    function setup(button: Gtk.Calendar) {

    }

    return <Calendar
        setup={setup}
        onDaySelected={(self) => {
            print(self.day)
        }}
    />
}
```

### Builtin Widgets

These widgets are available by default in JSX.

- box: [Astal.Box](https://aylur.github.io/libastal/astal4/class.Box.html)
  ```tsx
  <box>Horizontal Box</box>
  ```
  ```tsx
  <box orientation={1}>Vertical Box</box>
  ```
- button: [Gtk.Button](https://docs.gtk.org/gtk4/class.Button.html)
  ```tsx
  <button onClicked={self => print(self, "was clicked")}>
      Click Me
  </button>
  ```
- centerbox: [Gtk.CenterBox](https://docs.gtk.org/gtk4/class.CenterBox.html)
  ```tsx
  <centerbox orientation={1}>
      <label label="Start Widget" />
      <label label="Center Widget" />
      <label label="End Widget" />
  </box>
  ```
- entry: [Gtk.Entry](https://docs.gtk.org/gtk4/class.Entry.html)
  ```tsx
  <window keymode={Astal.Keymode.ON_DEMAND}>
      <entry
          onNotifyText={self => print("text changed", self.text)}
          onActivate={self => print("enter", self.text)}
      />
  </window>
  ```

- image: [Gtk.Image](https://docs.gtk.org/gtk4/class.Image.html)
  ```tsx
  <image iconName={GLib.get_os_info("LOGO") || "missing-symbolic"} />
  ```
  ```css
  image {
    -gtk-icon-size: 16px;
  }
  ```

- label: [Gtk.Label](https://docs.gtk.org/gtk4/class.Label.html)
  ```tsx
  <label label="hello" maxWidthChars={16} wrap />
  ```

- levelbar: [Gtk.LevelBar](https://docs.gtk.org/gtk4/class.LevelBar.html)
  ```tsx
  <levelbar value={0.5} widthRequest={200} />
  ```

- overlay: [Gtk.Overlay](https://docs.gtk.org/gtk4/class.Overlay.html)
  ```tsx
  <overlay>
      <box heightRequest={40} widthRequest={40}>Child</box>
      <box type="overlay measure" >1</box>
      <box type="overlay clip" >2</box>
      <box type="overlay clip measure" >3</box>
  </overlay>
  ```

- revealer: [Gtk.Revealer](https://docs.gtk.org/gtk4/class.Revealer.html)
  ```tsx
  <revealer
      setup={self => timeout(500, () => self.revealChild = true)}
      transitionType={Gtk.RevealerTransitionType.SLIDE_UP}>
      <label label="Child" />
  </revealer>
  ```

- slider: [Astal.Slider](https://aylur.github.io/libastal/astal4/class.Slider.html)
  ```tsx
  <slider widthRequest={100} onNotifyValue={self => print("new value", self.value)} />
  ```

- stack: [Gtk.Stack](https://docs.gtk.org/gtk4/class.Stack.html)
  ```tsx
  <stack visibleChildName="child2">
      <label name="child1" label="child1" />
      <label name="child2" label="child2" />
  </stack>
  ```

- switch: [Gtk.Switch](https://docs.gtk.org/gtk4/class.Switch.html)
  ```tsx
  <switch onNotifyActive={self => print(self.active)} />
  ```

- menubutton: [Gtk.MenuButton](https://docs.gtk.org/gtk4/class.MenuButton.html) and popover: [Gtk.Popover](https://docs.gtk.org/gtk4/class.Popover.html)
  ```tsx
  <menubutton>
    <label label="Button Content" />
    <popover>
      <label label="Popover Content" />
    </popover>
  </menubutton>
  ```

- window: [Astal.Window](https://aylur.github.io/libastal/astal4/class.Window.html)
  ```tsx
  <window
      cssClasses={["Bar"]}
      name="bar"
      namespace="bar"
      application={App}
      monitor={0}
      anchor={Astal.WindowAnchor.TOP | Astal.WindowAnchor.LEFT}
      exclusivity={Astal.Exclusivity.EXCLUSIVE}
      keymode={Astal.Keymode.ON_DEMAND}
  >
      <centerbox />
  </window>
  ```
