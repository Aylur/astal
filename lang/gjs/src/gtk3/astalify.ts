import Astal from "gi://Astal?version=3.0"
import Gtk from "gi://Gtk?version=3.0"
import Gdk from "gi://Gdk?version=3.0"
import GObject from "gi://GObject"
import { execAsync } from "../process.js"
import Variable from "../variable.js"
import Binding, { kebabify, snakeify, type Connectable, type Subscribable } from "../binding.js"

export function mergeBindings(array: any[]) {
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

function setProp(obj: any, prop: string, value: any) {
    try {
        // the setter method has to be used because
        // array like properties are not bound correctly as props
        const setter = `set_${snakeify(prop)}`
        if (typeof obj[setter] === "function")
            return obj[setter](value)

        return (obj[prop] = value)
    }
    catch (error) {
        console.error(`could not set property "${prop}" on ${obj}:`, error)
    }
}

export default function astalify<
    C extends { new(...args: any[]): Gtk.Widget },
>(cls: C, clsName = cls.name) {
    class Widget extends cls {
        get css(): string { return Astal.widget_get_css(this) }
        set css(css: string) { Astal.widget_set_css(this, css) }
        get_css(): string { return this.css }
        set_css(css: string) { this.css = css }

        get className(): string { return Astal.widget_get_class_names(this).join(" ") }
        set className(className: string) { Astal.widget_set_class_names(this, className.split(/\s+/)) }
        get_class_name(): string { return this.className }
        set_class_name(className: string) { this.className = className }

        get cursor(): Cursor { return Astal.widget_get_cursor(this) as Cursor }
        set cursor(cursor: Cursor) { Astal.widget_set_cursor(this, cursor) }
        get_cursor(): Cursor { return this.cursor }
        set_cursor(cursor: Cursor) { this.cursor = cursor }

        get clickThrough(): boolean { return Astal.widget_get_click_through(this) }
        set clickThrough(clickThrough: boolean) { Astal.widget_set_click_through(this, clickThrough) }
        get_click_through(): boolean { return this.clickThrough }
        set_click_through(clickThrough: boolean) { this.clickThrough = clickThrough }

        declare private __no_implicit_destroy: boolean
        get noImplicitDestroy(): boolean { return this.__no_implicit_destroy }
        set noImplicitDestroy(value: boolean) { this.__no_implicit_destroy = value }

        _setChildren(children: Gtk.Widget[]) {
            children = children.flat(Infinity).map(ch => ch instanceof Gtk.Widget
                ? ch
                : new Gtk.Label({ visible: true, label: String(ch) }))

            // remove
            if (this instanceof Gtk.Bin) {
                const ch = this.get_child()
                if (ch)
                    this.remove(ch)
                if (ch && !children.includes(ch) && !this.noImplicitDestroy)
                    ch?.destroy()
            }
            else if (this instanceof Gtk.Container) {
                for (const ch of this.get_children()) {
                    this.remove(ch)
                    if (!children.includes(ch) && !this.noImplicitDestroy)
                        ch?.destroy()
                }
            }

            // TODO: add more container types
            if (this instanceof Astal.Box) {
                this.set_children(children)
            }

            else if (this instanceof Astal.Stack) {
                this.set_children(children)
            }

            else if (this instanceof Astal.CenterBox) {
                this.startWidget = children[0]
                this.centerWidget = children[1]
                this.endWidget = children[2]
            }

            else if (this instanceof Astal.Overlay) {
                const [child, ...overlays] = children
                this.set_child(child)
                this.set_overlays(overlays)
            }

            else if (this instanceof Gtk.Container) {
                for (const ch of children)
                    this.add(ch)
            }

            else {
                throw Error(`can not add children to ${this.constructor.name}, it is not a container widget`)
            }
        }

        toggleClassName(cn: string, cond = true) {
            Astal.widget_toggle_class_name(this, cn, cond)
        }

        hook(
            object: Connectable,
            signal: string,
            callback: (self: this, ...args: any[]) => void,
        ): this
        hook(
            object: Subscribable,
            callback: (self: this, ...args: any[]) => void,
        ): this
        hook(
            object: Connectable | Subscribable,
            signalOrCallback: string | ((self: this, ...args: any[]) => void),
            callback?: (self: this, ...args: any[]) => void,
        ) {
            if (typeof object.connect === "function" && callback) {
                const id = object.connect(signalOrCallback, (_: any, ...args: unknown[]) => {
                    callback(this, ...args)
                })
                this.connect("destroy", () => {
                    (object.disconnect as Connectable["disconnect"])(id)
                })
            }

            else if (typeof object.subscribe === "function" && typeof signalOrCallback === "function") {
                const unsub = object.subscribe((...args: unknown[]) => {
                    signalOrCallback(this, ...args)
                })
                this.connect("destroy", unsub)
            }

            return this
        }

        constructor(...params: any[]) {
            super()
            const [config] = params

            const { setup, child, children = [], ...props } = config
            props.visible ??= true

            if (child)
                children.unshift(child)

            // collect bindings
            const bindings = Object.keys(props).reduce((acc: any, prop) => {
                if (props[prop] instanceof Binding) {
                    const binding = props[prop]
                    delete props[prop]
                    return [...acc, [prop, binding]]
                }
                return acc
            }, [])

            // collect signal handlers
            const onHandlers = Object.keys(props).reduce((acc: any, key) => {
                if (key.startsWith("on")) {
                    const sig = kebabify(key).split("-").slice(1).join("-")
                    const handler = props[key]
                    delete props[key]
                    return [...acc, [sig, handler]]
                }
                return acc
            }, [])

            // set children
            const mergedChildren = mergeBindings(children.flat(Infinity))
            if (mergedChildren instanceof Binding) {
                this._setChildren(mergedChildren.get())
                this.connect("destroy", mergedChildren.subscribe((v) => {
                    this._setChildren(v)
                }))
            }
            else {
                if (mergedChildren.length > 0) {
                    this._setChildren(mergedChildren)
                }
            }

            // setup signal handlers
            for (const [signal, callback] of onHandlers) {
                if (typeof callback === "function") {
                    this.connect(signal, callback)
                }
                else {
                    this.connect(signal, () => execAsync(callback)
                        .then(print).catch(console.error))
                }
            }

            // setup bindings handlers
            for (const [prop, binding] of bindings) {
                if (prop === "child" || prop === "children") {
                    this.connect("destroy", binding.subscribe((v: any) => {
                        this._setChildren(v)
                    }))
                }
                this.connect("destroy", binding.subscribe((v: any) => {
                    setProp(this, prop, v)
                }))
                setProp(this, prop, binding.get())
            }

            Object.assign(this, props)
            setup?.(this)
        }
    }

    GObject.registerClass({
        GTypeName: `Astal_${clsName}`,
        Properties: {
            "class-name": GObject.ParamSpec.string(
                "class-name", "", "", GObject.ParamFlags.READWRITE, "",
            ),
            "css": GObject.ParamSpec.string(
                "css", "", "", GObject.ParamFlags.READWRITE, "",
            ),
            "cursor": GObject.ParamSpec.string(
                "cursor", "", "", GObject.ParamFlags.READWRITE, "default",
            ),
            "click-through": GObject.ParamSpec.boolean(
                "click-through", "", "", GObject.ParamFlags.READWRITE, false,
            ),
            "no-implicit-destroy": GObject.ParamSpec.boolean(
                "no-implicit-destroy", "", "", GObject.ParamFlags.READWRITE, false,
            ),
        },
    }, Widget)

    return Widget
}

export type BindableProps<T> = {
    [K in keyof T]: Binding<T[K]> | T[K];
}

type SigHandler<
    W extends InstanceType<typeof Gtk.Widget>,
    Args extends Array<unknown>,
> = ((self: W, ...args: Args) => unknown) | string | string[]

export type ConstructProps<
    Self extends InstanceType<typeof Gtk.Widget>,
    Props extends Gtk.Widget.ConstructorProps,
    Signals extends Record<`on${string}`, Array<unknown>> = Record<`on${string}`, any[]>,
> = Partial<{
    // @ts-expect-error can't assign to unknown, but it works as expected though
    [S in keyof Signals]: SigHandler<Self, Signals[S]>
}> & Partial<{
    [Key in `on${string}`]: SigHandler<Self, any[]>
}> & BindableProps<Partial<Props> & {
    className?: string
    css?: string
    cursor?: string
    clickThrough?: boolean
}> & {
    onDestroy?: (self: Self) => unknown
    onDraw?: (self: Self) => unknown
    onKeyPressEvent?: (self: Self, event: Gdk.Event) => unknown
    onKeyReleaseEvent?: (self: Self, event: Gdk.Event) => unknown
    onButtonPressEvent?: (self: Self, event: Gdk.Event) => unknown
    onButtonReleaseEvent?: (self: Self, event: Gdk.Event) => unknown
    onRealize?: (self: Self) => unknown
    setup?: (self: Self) => void
}

export type BindableChild = Gtk.Widget | Binding<Gtk.Widget>

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
