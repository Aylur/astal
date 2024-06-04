import Binding, { kebabify, type Connectable, type Subscribable } from "./binding.js"
import { Astal, Gtk } from "./imports.js"
import { execAsync } from "./process.js"

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

function setChild(parent: Gtk.Widget, child: Gtk.Widget) {
    if (parent instanceof Gtk.Bin) {
        const rm = parent.get_child()
        if (rm)
            parent.remove(rm)
    }
    if (parent instanceof Gtk.Container)
        parent.add(child)
}

function ctor(self: any, config: any, ...children: Gtk.Widget[]) {
    const { setup, child, ...props } = config
    props.visible ??= true

    const pchildren = props.children
    delete props.children

    const bindings = Object.keys(props).reduce((acc: any, prop) => {
        if (props[prop] instanceof Binding) {
            const bind = [prop, props[prop]]
            prop === "child"
                ? setChild(self, props[prop].get())
                : self[`set_${kebabify(prop)}`](props[prop].get())

            delete props[prop]
            return [...acc, bind]
        }
        return acc
    }, [])

    const onHandlers = Object.keys(props).reduce((acc: any, key) => {
        if (key.startsWith("on")) {
            const sig = kebabify(key).split("-").slice(1).join("-")
            const handler = [sig, props[key]]
            delete props[key]
            return [...acc, handler]
        }
        return acc
    }, [])

    Object.assign(self, props)
    Object.assign(self, {
        hook(obj: any, sig: any, callback: any) {
            return hook(self, obj, sig, callback)
        },
    })

    if (child instanceof Binding) {
        setChild(self, child.get())
        self.connect("destroy", child.subscribe(v => {
            setChild(self, v)
        }))
    } else if (self instanceof Gtk.Container && child instanceof Gtk.Widget) {
        self.add(child)
    }

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
        if (pchildren) {
            for (const child of pchildren)
                self.add(child)
        }
        if (children) {
            for (const child of children)
                self.add(child)
        }
    }

    for (const [prop, bind] of bindings) {
        self.connect("destroy", bind.subscribe((v: any) => {
            self[`set_${kebabify(prop)}`](v)
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
