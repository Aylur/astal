# Binding

As mentioned before binding an object's state to another -
so in most cases a `Variable` or a `GObject.Object` property to a widget's property -
is done through the `bind` function which returns a `Binding` object.

`Binding` objects simply hold information about the source and how it should be transformed
which Widget constructors can use to setup a connection between themselves and the source.

```ts
class Binding<Value> {
    private transformFn: (v: any) => unknown
    private emitter: Subscribable<Value> | Connectable
    private prop?: string

    as<T>(fn: (v: Value) => T): Binding<T>
    get(): Value
    subscribe(callback: (value: Value) => void): () => void
}
```

A `Binding` can be constructed from an object implementing
the `Subscribable` interface (usually a `Variable`)
or an object implementing the `Connectable` interface and one of its properties
(usually a `GObject.Object` instance).

```ts
function bind<T>(obj: Subscribable<T>): Binding<T>

function bind<
    Obj extends Connectable,
    Prop extends keyof Obj,
>(obj: Obj, prop: Prop): Binding<Obj[Prop]>
```

## Subscribable and Connectable interface

Any object implementing one of these interfaces can be used with `bind`.

```ts
interface Subscribable<T> {
    subscribe(callback: (value: T) => void): () => void
    get(): T
}

interface Connectable {
    connect(signal: string, callback: (...args: any[]) => unknown): number
    disconnect(id: number): void
}
```

## Example Custom Subscribable

When binding the children of a box from an array, usually not all elements
of the array changes each time, so it would make sense to not destroy
the widget which represents the element.

::: code-group

```ts :line-numbers [varmap.ts]
import { type Subscribable } from "astal/binding"
import { Gtk } from "astal"

export class VarMap<K, T = Gtk.Widget> implements Subscribable {
    #subs = new Set<(v: Array<[K, T]>) => void>()
    #map: Map<K, T>

    #notifiy() {
        const value = this.get()
        for (const sub of this.#subs) {
            sub(value)
        }
    }

    #delete(key: K) {
        const v = this.#map.get(key)

        if (v instanceof Gtk.Widget) {
            v.destroy()
        }

        this.#map.delete(key)
    }

    constructor(initial?: Iterable<[K, T]>) {
        this.#map = new Map(initial)
    }

    set(key: K, value: T) {
        this.#delete(key)
        this.#map.set(key, value)
        this.#notifiy()
    }

    delete(key: K) {
        this.#delete(key)
        this.#notifiy()
    }

    get() {
        return [...this.#map.entries()]
    }

    subscribe(callback: (v: Array<[K, T]>) => void) {
        this.#subs.add(callback)
        return () => this.#subs.delete(callback)
    }
}
```

:::

And this `VarMap<key, Widget>` can be used as an alternative to `Variable<Array<Widget>>`.

```tsx
function MappedBox() {
    const map = new VarMap([
        [1, <MyWidget id={id} />]
        [2, <MyWidget id={id} />]
    ])

    const conns = [
        gobject.connect("added", (_, id) => map.set(id, MyWidget({ id }))),
        gobject.connect("removed", (_, id) => map.delete(id, MyWidget({ id }))),
    ]

    return <box onDestroy={() => conns.map(id => gobject.disconnect(id))}>
        {bind(map).as(arr => arr.sort(([a], [b]) => a - b).map(([,w]) => w))}
    </box>
}
```

## Example Custom Connectable

Astal provides [decorator functions](./gobject#example-usage)
that make it easy to subclass gobjects, however
you can read more about GObjects and subclassing
on [gjs.guide](https://gjs.guide/guides/gobject/subclassing.html#gobject-subclassing).

Objects coming from [libraries](../libraries/references#astal-libraries)
usually have a singleton gobject you can access with `.get_default()`.

Here is an example of a Brightness library by wrapping the `brightnessctl` cli utility
and by monitoring `/sys/class/backlight`

::: code-group

```ts :line-numbers [brightness.ts]
import GObject, { register, property } from "astal/gobject"
import { monitorFile, readFileAsync } from "astal/file"
import { exec, execAsync } from "astal/process"

const get = (args: string) => Number(exec(`brightnessctl ${args}`))
const screen = exec(`bash -c "ls -w1 /sys/class/backlight | head -1"`)
const kbd = exec(`bash -c "ls -w1 /sys/class/leds | head -1"`)

@register({ GTypeName: "Brightness" })
export default class Brightness extends GObject.Object {
    static instance: Brightness
    static get_default() {
        if (!this.instance)
            this.instance = new Brightness()

        return this.instance
    }

    #kbdMax = get(`--device ${kbd} max`)
    #kbd = get(`--device ${kbd} get`)
    #screenMax = get("max")
    #screen = get("get") / (get("max") || 1)

    @property(Number)
    get kbd() { return this.#kbd }

    set kbd(value) {
        if (value < 0 || value > this.#kbdMax)
            return

        execAsync(`brightnessctl -d ${kbd} s ${value} -q`).then(() => {
            this.#kbd = value
            this.notify("kbd")
        })
    }

    @property(Number)
    get screen() { return this.#screen }

    set screen(percent) {
        if (percent < 0)
            percent = 0

        if (percent > 1)
            percent = 1

        execAsync(`brightnessctl set ${Math.floor(percent * 100)}% -q`).then(() => {
            this.#screen = percent
            this.notify("screen")
        })
    }

    constructor() {
        super()

        const screenPath = `/sys/class/backlight/${screen}/brightness`
        const kbdPath = `/sys/class/leds/${kbd}/brightness`

        monitorFile(screenPath, async f => {
            const v = await readFileAsync(f)
            this.#screen = Number(v) / this.#screenMax
            this.notify("screen")
        })

        monitorFile(kbdPath, async f => {
            const v = await readFileAsync(f)
            this.#kbd = Number(v) / this.#kbdMax
            this.notify("kbd")
        })
    }
}
```

:::

And it can be used like any other library object.

```tsx
function BrightnessSlider() {
    const brightness = Brightness.get_default()

    return <slider
        value={bind(brightness, "screen")}
        onDragged={({ value }) => brightness.screen = value}
    />
}
```
