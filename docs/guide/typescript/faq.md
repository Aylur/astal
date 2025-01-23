# Frequently asked question, common issues, tips and tricks

## Monitor id does not match compositor

The monitor id property that windows expect is mapped by Gdk, which is not always
the same as the compositor. Instead use the `gdkmonitor` property which expects
a `Gdk.Monitor` object.

```tsx
import { App } from "astal/gtk3"

function Bar(gdkmonitor) {
    return <window gdkmonitor={gdkmonitor} />
}

function main() {
    for (const monitor of App.get_monitors()) {
        if (monitor.model == "your-desired-model") {
            Bar(monitor)
        }
    }
}

App.start({ main })
```

## Environment variables

JavaScript is **not** an bash.

```ts
const HOME = exec("echo $HOME") // does not work
```

`exec` and `execAsync` runs the passed program as is, its **not** run in a
shell environment, so the above example just passes `$HOME` as a string literal
to the `echo` program.

:::danger Please don't do this
You could pass it to bash, but that is a horrible approach.

```ts
const HOME = exec("bash -c 'echo $HOME'")
```

:::

You can read environment variables with [GLib.getenv](https://gjs-docs.gnome.org/glib20~2.0/glib.getenv).

```ts
import GLib from "gi://GLib"

const HOME = GLib.getenv("HOME")
```

## Custom SVG symbolic icons

Put the svgs in a directory, name them `<icon-name>-symbolic.svg`
and use `App.add_icons` or `icons` parameter in `App.start`

:::code-group

```ts [app.ts]
App.start({
    icons: `/path/to/icons`, // this dir should include custom-symbolic.svg
    main() {
        Widget.Icon({
            icon: "custom-symbolic", // custom-symbolic.svg
            css: "color: green;", // can be colored, like other named icons
        })
    },
})
```

:::

:::info
If there is a name clash with an icon from your current icon pack
the icon pack will take precedence
:::

## Logging

The `console` API in gjs uses glib logging functions.
If you just want to print some text as is to stdout
use the globally available `print` function or `printerr` for stderr.

```ts
print("print this line to stdout")
printerr("print this line to stderr")
```

## Populate the global scope with frequently accessed variables

It might be annoying to always import Gtk only for the `Gtk.Align` enum.

:::code-group

```ts [globals.ts]
import Gtk from "gi://Gtk"

declare global {
    const START: number
    const CENTER: number
    const END: number
    const FILL: number
}

Object.assign(globalThis, {
    START: Gtk.Align.START,
    CENTER: Gtk.Align.CENTER,
    END: Gtk.Align.END,
    FILL: Gtk.Align.FILL,
})
```

:::

:::code-group

```tsx [Bar.tsx] {3}
export default function Bar() {
    return <window>
        <box halign={START} />
    </window>
}
```

:::

:::code-group

```ts [app.ts]
import "./globals" // don't forget to import it first // [!code ++]
import Bar from "./Bar"

App.start({
    main() {
        Bar()
    }
})
```

:::

:::info
It is considered bad practice to populate the global scope, but its your code, not a public library.
:::

## Auto create Window for each Monitor

To have Window widgets appear on a monitor when its plugged in, listen to `App.monitor_added`.

:::code-group

```tsx [Bar.tsx]
export default function Bar(gdkmonitor: Gdk.Monitor) {
    return <window gdkmonitor={gdkmonitor} />
}
```

:::

:::code-group

```ts [app.ts]
import { Gdk, Gtk } from "astal/gtk3"
import Bar from "./Bar"

function main() {
    const bars = new Map<Gdk.Monitor, Gtk.Widget>()

    // initialize
    for (const gdkmonitor of App.get_monitors()) {
        bars.set(gdkmonitor, Bar(gdkmonitor))
    }

    App.connect("monitor-added", (_, gdkmonitor) => {
        bars.set(gdkmonitor, Bar(gdkmonitor))
    })

    App.connect("monitor-removed", (_, gdkmonitor) => {
        bars.get(gdkmonitor)?.destroy()
        bars.delete(gdkmonitor)
    })
}

App.start({ main })
```

:::

## Error: Can't convert non-null pointer to JS value

These happen when accessing list type properties. Gjs fails to correctly bind
`List` and other array like types of Vala as a property.

```ts
import Notifd from "gi://AstalNotifd"
const notifd = Notifd.get_default()

notifd.notifications // [!code --]
notifd.get_notifications() // [!code ++]
```

:::tip
Open up an issue/PR to add a [workaround](https://github.com/Aylur/astal/blob/main/lang/gjs/src/overrides.ts).
:::

## How to create regular floating windows

Use `Gtk.Window` with [Widget.astalify](/guide/typescript/widget#how-to-use-non-builtin-gtk-widgets).

By default `Gtk.Window` is destroyed on close. To prevent this add a handler for `delete-event`.

```tsx {4-7}
const RegularWindow = Widget.astalify(Gtk.Window)

return <RegularWindow
    onDeleteEvent={(self) => {
        self.hide()
        return true
    }}
>
    {child}
</RegularWindow>
```

## Is there a way to limit the width/height of a widget?

Unfortunately not. You can set a minimum size with `min-width` and `min-heigth` css attributes,
but you can not set max size.

## Custom widgets with bindable properties

In function components you can wrap any primitive to handle both
binding and value cases as one.

```tsx
function MyWidget(props: { prop: string | Binding<string> }) {
    const prop = props.prop instanceof Binding
        ? props.prop
        : bind({ get: () => props.prop, subscribe: () => () => {} })

    function setup(self: Widget.Box) {
        self.hook(prop, () => {
            const value = prop.get()
            // handler
        })
    }

    return <box setup={setup}>
    </box>
}
```

You can pass the prop the super constructor in subclasses

```tsx
@register()
class MyWidget extends Widget.Box {
    @property(String)
    set prop(v: string) {
        // handler
    }

    constructor(props: { prop: string | Binding<string> }) {
        super(props)
    }
}
```

## How do I register keybindings?

If you want global keybindings use your compositor.
Only **focused** windows can capture events. To make a window
focusable set its keymode.

::: code-group
```tsx [gtk3]
<window
    keymode={Astal.Keymode.ON_DEMAND}
    onKeyPressEvent={(self, event: Gdk.Event) => {
        if (event.get_keyval()[1] === Gdk.KEY_Escape) {
            self.hide()
        }
    }}
/>
```

```tsx [gtk4]
<window
    keymode={Astal.Keymode.ON_DEMAND}
    onKeyPressed={(self, keyval) => {
        if (keyval === Gdk.KEY_Escape) {
            self.hide()
        }
    }}
/>
```
:::

## How to create a Popup

In Gtk4 simply use Gtk's builtin [Popover](https://docs.gtk.org/gtk4/class.Popover.html).

In Gtk3 you can create an [Astal.Window](https://aylur.github.io/libastal/astal3/class.Window.html) and handle click events.

Checkout [examples/gtk3/js/popover](https://github.com/Aylur/astal/tree/main/examples/gtk3/js/popover)
