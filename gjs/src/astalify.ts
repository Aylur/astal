import Binding, { kebabify, snakeify, type Connectable, type Subscribable } from "./binding.js"
import { Astal, Gtk, Gdk } from "./imports.js"
import { execAsync } from "./process.js"
import Variable from "./variable.js"

Object.defineProperty(Astal.Box.prototype, "children", {
    get() { return this.get_children() },
    set(v) { this.set_children(v) },
})

function setChildren(parent: Gtk.Widget, children: Gtk.Widget[]) {
    children = children.flat(Infinity).map(ch => ch instanceof Gtk.Widget
        ? ch
        : new Gtk.Label({ visible: true, label: String(ch) }))

    // remove
    if (parent instanceof Gtk.Bin) {
        const ch = parent.get_child()
        if (ch)
            parent.remove(ch)
    }

    // FIXME: add rest of the edge cases like Stack
    if (parent instanceof Astal.Box) {
        parent.set_children(children)
    }

    else if (parent instanceof Astal.CenterBox) {
        parent.startWidget = children[0]
        parent.centerWidget = children[1]
        parent.endWidget = children[2]
    }

    else if (parent instanceof Astal.Overlay) {
        const [child, ...overlays] = children
        parent.set_child(child)
        parent.set_overlays(overlays)
    }

    else if (parent instanceof Gtk.Container) {
        for (const ch of children)
            parent.add(ch)
    }
}

function mergeBindings(array: any[]) {
    function getValues(...args: any[]) {
        let i = 0
        return array.map(value => value instanceof Binding
            ? args[i++]
            : value,
        )
    }

    const bindings = array.filter(i => i instanceof Binding)

    if (bindings.length === 0)
        return array

    if (bindings.length === 1)
        return bindings[0].as(getValues)

    return Variable.derive(bindings, getValues)()
}

export type Widget<C extends { new(...args: any): Gtk.Widget }> = InstanceType<C> & {
    className: string
    css: string
    cursor: Cursor
    clickThrough: boolean
    toggleClassName(name: string, on?: boolean): void
    hook(
        object: Connectable,
        signal: string,
        callback: (self: Widget<C>, ...args: any[]) => void,
    ): Widget<C>
    hook(
        object: Subscribable,
        callback: (self: Widget<C>, ...args: any[]) => void,
    ): Widget<C>
}

function hook(
    self: Gtk.Widget,
    object: Connectable | Subscribable,
    signalOrCallback: string | ((self: Gtk.Widget, ...args: any[]) => void),
    callback?: (self: Gtk.Widget, ...args: any[]) => void,
) {
    if (typeof object.connect === "function" && callback) {
        const id = object.connect(signalOrCallback, (_: any, ...args: unknown[]) => {
            callback(self, ...args)
        })
        self.connect("destroy", () => {
            (object.disconnect as Connectable["disconnect"])(id)
        })
    }

    else if (typeof object.subscribe === "function" && typeof signalOrCallback === "function") {
        const unsub = object.subscribe((...args: unknown[]) => {
            signalOrCallback(self, ...args)
        })
        self.connect("destroy", unsub)
    }

    return self
}

function ctor(self: any, config: any = {}, children: any[] = []) {
    const { setup, ...props } = config
    props.visible ??= true

    const bindings = Object.keys(props).reduce((acc: any, prop) => {
        if (props[prop] instanceof Binding) {
            const binding = props[prop]
            self[`set_${snakeify(prop)}`](binding.get())
            delete props[prop]
            return [...acc, [prop, binding]]
        }
        return acc
    }, [])

    const onHandlers = Object.keys(props).reduce((acc: any, key) => {
        if (key.startsWith("on")) {
            const sig = kebabify(key).split("-").slice(1).join("-")
            const handler = props[key]
            delete props[key]
            return [...acc, [sig, handler]]
        }
        return acc
    }, [])

    Object.assign(self, props)

    for (const [signal, callback] of onHandlers) {
        if (typeof callback === "function") {
            self.connect(signal, callback)
        }
        else {
            self.connect(signal, () => execAsync(callback)
                .then(print).catch(console.error))
        }
    }

    for (const [prop, bind] of bindings) {
        if (prop === "child" || prop === "children") {
            self.connect("destroy", bind.subscribe((v: any) => {
                setChildren(self, v)
            }))
        }
        self.connect("destroy", bind.subscribe((v: any) => {
            self[`set_${snakeify(prop)}`](v)
        }))
    }

    children = mergeBindings(children.flat(Infinity))
    if (children instanceof Binding) {
        setChildren(self, children.get())
        self.connect("destroy", children.subscribe(v => {
            setChildren(self, v)
        }))
    }
    else {
        if (children.length > 0)
            setChildren(self, children)
    }

    setup?.(self)
    return self
}

function proxify<
    C extends { new(...args: any[]): any },
>(klass: C) {
    klass.prototype.hook = function(obj: any, sig: any, callback: any) {
        return hook(this, obj, sig, callback)
    }

    klass.prototype.toggleClassName = function(name: string, on = true) {
        Astal.widget_toggle_class_name(this, name, on)
    }

    Object.defineProperty(klass.prototype, "className", {
        get() { return Astal.widget_get_class_names(this).join(" ") },
        set(v) { Astal.widget_set_class_names(this, v.split(/\s+/)) },
    })

    klass.prototype.set_class_name = function(name: string) {
        this.className = name
    }

    Object.defineProperty(klass.prototype, "css", {
        get() { return Astal.widget_get_css(this) },
        set(v) { Astal.widget_set_css(this, v) },
    })

    klass.prototype.set_css = function(css: string) {
        this.css = css
    }

    Object.defineProperty(klass.prototype, "cursor", {
        get() { return Astal.widget_get_cursor(this) },
        set(v) { Astal.widget_set_cursor(this, v) },
    })

    klass.prototype.set_cursor = function(cursor: string) {
        this.cursor = cursor
    }

    Object.defineProperty(klass.prototype, "clickThrough", {
        get() { return Astal.widget_get_click_through(this) },
        set(v) { Astal.widget_set_click_through(this, v) },
    })

    klass.prototype.set_click_through = function(clickThrough: boolean) {
        this.clickThrough = clickThrough
    }

    const proxy = new Proxy(klass, {
        construct(_, [conf, ...children]) {
            const self = new klass
            return ctor(self, conf, children)
        },
        apply(_t, _a, [conf, ...children]) {
            const self = new klass
            return ctor(self, conf, children)
        },
    })

    return proxy
}

export default function astalify<
    C extends typeof Gtk.Widget,
    P extends Record<string, any>,
    N extends string = "Widget",
>(klass: C) {
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    type Astal<N> = Omit<C, "new"> & {
        new(props?: P, ...children: Gtk.Widget[]): Widget<C>
        (props?: P, ...children: Gtk.Widget[]): Widget<C>
    }

    return proxify(klass) as unknown as Astal<N>
}


type BindableProps<T> = {
    [K in keyof T]: Binding<NonNullable<T[K]>> | T[K];
}

type SigHandler<
    W extends { new(...args: any): Gtk.Widget },
    Args extends Array<unknown>,
> = ((self: Widget<W>, ...args: Args) => unknown) | string | string[]

export type ConstructProps<
    Self extends { new(...args: any[]): any },
    Props = unknown,
    Signals extends Record<`on${string}`, Array<unknown>> = Record<`on${string}`, any[]>
> = Partial<{
    // @ts-expect-error can't assign to unknown, but it works as expected though
    [S in keyof Signals]: SigHandler<Self, Signals[S]>
}> & Partial<{
    [Key in `on${string}`]: SigHandler<Self, any[]>
}> & BindableProps<Props & {
    className?: string
    css?: string
    cursor?: string
    clickThrough?: boolean
}> & {
    onDestroy?: (self: Widget<Self>) => unknown
    onDraw?: (self: Widget<Self>) => unknown
    onKeyPressEvent?: (self: Widget<Self>, event: Gdk.Event) => unknown
    onKeyReleaseEvent?: (self: Widget<Self>, event: Gdk.Event) => unknown
    onButtonPressEvent?: (self: Widget<Self>, event: Gdk.Event) => unknown
    onButtonReleaseEvent?: (self: Widget<Self>, event: Gdk.Event) => unknown
    onRealize?: (self: Widget<Self>) => unknown
    setup?: (self: Widget<Self>) => void
}

type Cursor =
    | "default"
    | "help"
    | "pointer"
    | "context-menu"
    | "progress"
    | "wait"
    | "cell"
    | "crosshair"
    | "text"
    | "vertical-text"
    | "alias"
    | "copy"
    | "no-drop"
    | "move"
    | "not-allowed"
    | "grab"
    | "grabbing"
    | "all-scroll"
    | "col-resize"
    | "row-resize"
    | "n-resize"
    | "e-resize"
    | "s-resize"
    | "w-resize"
    | "ne-resize"
    | "nw-resize"
    | "sw-resize"
    | "se-resize"
    | "ew-resize"
    | "ns-resize"
    | "nesw-resize"
    | "nwse-resize"
    | "zoom-in"
    | "zoom-out"
