import { hook, noImplicitDestroy, setChildren, mergeBindings, type BindableProps, construct } from "../_astal.js"
import Astal from "gi://Astal?version=3.0"
import Gtk from "gi://Gtk?version=3.0"
import Gdk from "gi://Gdk?version=3.0"
import GObject from "gi://GObject"
import Gio from "gi://Gio?version=2.0"
import Binding, { type Connectable, type Subscribable } from "../binding.js"

export { BindableProps, mergeBindings }

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

        declare private [noImplicitDestroy]: boolean
        get noImplicitDestroy(): boolean { return this[noImplicitDestroy] }
        set noImplicitDestroy(value: boolean) { this[noImplicitDestroy] = value }

        set actionGroup([prefix, group]: ActionGroup) { this.insert_action_group(prefix, group) }
        set_action_group(actionGroup: ActionGroup) { this.actionGroup = actionGroup }

        protected getChildren(): Array<Gtk.Widget> {
            if (this instanceof Gtk.Bin) {
                return this.get_child() ? [this.get_child()!] : []
            } else if (this instanceof Gtk.Container) {
                return this.get_children()
            }
            return []
        }

        protected setChildren(children: any[]) {
            children = children.flat(Infinity).map(ch => ch instanceof Gtk.Widget
                ? ch
                : new Gtk.Label({ visible: true, label: String(ch) }))

            if (this instanceof Gtk.Container) {
                for (const ch of children)
                    this.add(ch)
            } else {
                throw Error(`can not add children to ${this.constructor.name}`)
            }
        }

        [setChildren](children: any[]) {
            // remove
            if (this instanceof Gtk.Container) {
                for (const ch of this.getChildren()) {
                    this.remove(ch)
                    if (!children.includes(ch) && !this.noImplicitDestroy)
                        ch?.destroy()
                }
            }

            // append
            this.setChildren(children)
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
            hook(this, object, signalOrCallback, callback)
            return this
        }

        constructor(...params: any[]) {
            super()
            const props = params[0] || {}
            props.visible ??= true
            construct(this, props)
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

type SigHandler<
    W extends InstanceType<typeof Gtk.Widget>,
    Args extends Array<unknown>,
> = ((self: W, ...args: Args) => unknown) | string | string[]

export type BindableChild = Gtk.Widget | Binding<Gtk.Widget>

export type ConstructProps<
    Self extends InstanceType<typeof Gtk.Widget>,
    Props extends Gtk.Widget.ConstructorProps,
    Signals extends Record<`on${string}`, Array<unknown>> = Record<`on${string}`, any[]>,
> = Partial<{
    // @ts-expect-error can't assign to unknown, but it works as expected though
    [S in keyof Signals]: SigHandler<Self, Signals[S]>
}> & Partial<{
    [Key in `on${string}`]: SigHandler<Self, any[]>
}> & BindableProps<Partial<Props & {
    className?: string
    css?: string
    cursor?: string
    clickThrough?: boolean
    actionGroup?: ActionGroup
}>> & Partial<{
    onDestroy: (self: Self) => unknown
    onDraw: (self: Self) => unknown
    onKeyPressEvent: (self: Self, event: Gdk.Event) => unknown
    onKeyReleaseEvent: (self: Self, event: Gdk.Event) => unknown
    onButtonPressEvent: (self: Self, event: Gdk.Event) => unknown
    onButtonReleaseEvent: (self: Self, event: Gdk.Event) => unknown
    onRealize: (self: Self) => unknown
    setup: (self: Self) => void
}>

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

type ActionGroup = [prefix: string, actionGroup: Gio.ActionGroup]
