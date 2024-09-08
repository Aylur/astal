# Frequently asked question, common issues, tips and tricks

## Monitor id does not match compositor

The monitor property that windows expect is mapped by Gdk, which is not always
the same as the compositor. Instead use the `gdkmonitor` property which expects
a `Gdk.Monitor` object which you can get from compositor libraries.

Example with Hyprland

```tsx
import Hyprland from "gi://AstalHyprland"

function Bar(gdkmonitor) {
    return <window gdkmonitor={gdkmonitor} />
}

function main() {
    for (const m of Hyprland.get_default().get_monitors()) {
        Bar(m.gdk_monitor)
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

## Custom svg symbolic icons

Put the svgs in a directory, named `<icon-name>-symbolic.svg`
and use `App.add_icons` or `icons` parameter in `App.start`

:::code-group

```ts [app.ts]
App.start({
    icons: `${SRC}/icons`,
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

## Binding custom structures

The `bind` function can take two types of objects.

```ts
interface Subscribable<T = unknown> {
    subscribe(callback: (value: T) => void): () => void
    get(): T
}

interface Connectable {
    connect(signal: string, callback: (...args: any[]) => unknown): number
    disconnect(id: number): void
}
```

`Connectable` is for mostly gobjects, while `Subscribable` is for `Variables`
and custom objects.

For example you can compose `Variables` in using a class.

```ts
type MyVariableValue = {
    number: number
    string: string
}

class MyVariable {
    number = Variable(0)
    string = Variable("")

    get(): MyVariableValue {
        return {
            number: this.number.get(),
            string: this.string.get(),
        }
    }

    subscribe(callback: (v: MyVariableValue) => void) {
        const unsub1 = this.number.subscribe((value) => {
            callback({ string: value, number: this.number.get() })
        })

        const unsub2 = this.string.subscribe((value) => {
            callback({ number: value, string: this.string.get() })
        })

        return () => {
            unsub1()
            unsub2()
        }
    }
}
```

Then it can be used with `bind`.

```tsx
function MyWidget() {
    const myvar = new MyVariableValue()
    const label = bind(myvar).as(({ string, number }) => {
        return `${string} ${number}`
    })

    return <label label={label} />
}
```

## Populate the global scope with frequently accessed variables

It might be annoying to always import Gtk only for `Gtk.Align` enums.

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

```tsx [Bar.tsx]
export default function Bar() {
    return <window>
        <box halign={START} />
    </window>
}
```

:::

:::code-group

```ts [app.ts]
import "./globals"
import Bar from "./Bar"

App.start({
    main: Bar
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
import { Gdk, Gtk } from "astal"
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

notifd.notifications // ❌ // [!code error]

notifd.get_notifications() // ✅
```

## How to create regular floating windows

Use `Gtk.Window` with [Widget.astalify](/ags/widget#how-to-use-non-builtin-gtk-widgets).

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
