import Binding, { kebabify, snakeify, type Connectable, type Subscribable } from "./binding.js"
import { Astal, Gtk } from "./imports.js"
import { execAsync } from "./process.js"

Object.defineProperty(Astal.Box.prototype, "children", {
    get() { return this.get_children() },
    set(v) { this.set_children(v) },
})

export type Widget<C extends { new(...args: any): Gtk.Widget }> = InstanceType<C> & {
    className: string
    css: string
    cursor: Cursor
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

function ctor(self: any, config: any = {}, ...children: Gtk.Widget[]) {
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

    const pchildren = props.children
    delete props.children

    Object.assign(self, props)
    Object.assign(self, {
        hook(obj: any, sig: any, callback: any) {
            return hook(self, obj, sig, callback)
        },
    })

    for (const [signal, callback] of onHandlers) {
        if (typeof callback === "function") {
            self.connect(signal, callback)
        }
        else {
            self.connect(signal, () => execAsync(callback)
                .then(print).catch(console.error))
        }
    }

    if (self instanceof Gtk.Container) {
        if (children) {
            for (const child of children)
                self.add(child)
        }
        if (pchildren && Array.isArray(pchildren)) {
            for (const child of pchildren)
                self.add(child)
        }
    }

    for (const [prop, bind] of bindings) {
        self.connect("destroy", bind.subscribe((v: any) => {
            self[`set_${snakeify(prop)}`](v)
        }))
    }

    setup?.(self)
    return self
}

function proxify<
    C extends { new(...args: any[]): any },
>(klass: C) {
    Object.defineProperty(klass.prototype, "className", {
        get() { return Astal.widget_get_class_names(this).join(" ") },
        set(v) { Astal.widget_set_class_names(this, v.split(/\s+/)) },
    })

    Object.defineProperty(klass.prototype, "css", {
        get() { return Astal.widget_get_css(this) },
        set(v) { Astal.widget_set_css(this, v) },
    })

    Object.defineProperty(klass.prototype, "cursor", {
        get() { return Astal.widget_get_cursor(this) },
        set(v) { Astal.widget_set_cursor(this, v) },
    })

    klass.prototype.set_child = function(widget: Gtk.Widget) {
        if (this instanceof Gtk.Bin) {
            const rm = this.get_child()
            if (rm)
                this.remove(rm)
        }
        if (this instanceof Gtk.Container)
            this.add(widget)
    }

    Object.defineProperty(klass.prototype, "child", {
        get() { return this.get_child?.() },
        set(v) { this.set_child(v) },
    })

    const proxy = new Proxy(klass, {
        construct(_, [conf, ...children]) {
            const self = new klass
            return ctor(self, conf, ...children)
        },
        apply(_t, _a, [conf, ...children]) {
            const self = new klass
            return ctor(self, conf, ...children)
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
}> & {
    onDestroy?: (self: Widget<Self>) => unknown
    onDraw?: (self: Widget<Self>) => unknown
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
