import { noImplicitDestroy, setChildren, type BindableProps, construct } from "../_astal.js"
import Gtk from "gi://Gtk?version=4.0"
import Gdk from "gi://Gdk?version=4.0"
import Binding from "../binding.js"

export const type = Symbol("child type")
const dummyBulder = new Gtk.Builder

function _getChildren(widget: Gtk.Widget): Array<Gtk.Widget> {
    if ("get_child" in widget && typeof widget.get_child == "function") {
        return widget.get_child() ? [widget.get_child()] : []
    }

    const children: Array<Gtk.Widget> = []
    let ch = widget.get_first_child()
    while (ch !== null) {
        children.push(ch)
        ch = ch.get_next_sibling()
    }
    return children
}

function _setChildren(widget: Gtk.Widget, children: any[]) {
    children = children.flat(Infinity).map(ch => ch instanceof Gtk.Widget
        ? ch
        : new Gtk.Label({ visible: true, label: String(ch) }))


    for (const child of children) {
        widget.vfunc_add_child(
            dummyBulder,
            child,
            type in child ? child[type] : null,
        )
    }
}

type Config<T extends Gtk.Widget> = {
    setChildren(widget: T, children: any[]): void
    getChildren(widget: T): Array<Gtk.Widget>
}

export default function astalify<
    Widget extends Gtk.Widget,
    Props extends Gtk.Widget.ConstructorProps = Gtk.Widget.ConstructorProps,
    Signals extends Record<`on${string}`, Array<unknown>> = Record<`on${string}`, any[]>,
>(cls: { new(...args: any[]): Widget }, config: Partial<Config<Widget>> = {}) {
    Object.assign(cls.prototype, {
        [setChildren](children: any[]) {
            const w = this as unknown as Widget
            for (const child of (config.getChildren?.(w) || _getChildren(w))) {
                if (child instanceof Gtk.Widget) {
                    child.unparent()
                    if (!children.includes(child) && noImplicitDestroy in this)
                        child.run_dispose()
                }
            }

            if (config.setChildren) {
                config.setChildren(w, children)
            } else {
                _setChildren(w, children)
            }
        },
    })

    return {
        [cls.name]: (
            props: ConstructProps<Widget, Props, Signals> = {},
            ...children: any[]
        ): Widget => {
            const widget = new cls("cssName" in props ? { cssName: props.cssName } : {})

            if ("cssName" in props) {
                delete props.cssName
            }

            if (props.noImplicitDestroy) {
                Object.assign(widget, { [noImplicitDestroy]: true })
                delete props.noImplicitDestroy
            }

            if (props.type) {
                Object.assign(widget, { [type]: props.type })
                delete props.type
            }

            if (children.length > 0) {
                Object.assign(props, { children })
            }

            return construct(widget as any, setupControllers(widget, props as any))
        },
    }[cls.name]
}

type SigHandler<
    W extends InstanceType<typeof Gtk.Widget>,
    Args extends Array<unknown>,
> = ((self: W, ...args: Args) => unknown) | string | string[]

export { BindableProps }
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
}> & Partial<BindableProps<Omit<Props, "cssName" | "css_name">>> & {
    noImplicitDestroy?: true
    type?: string
    cssName?: string
} & EventController<Self> & {
    onDestroy?: (self: Self) => unknown
    setup?: (self: Self) => void
}

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

function setupControllers<T>(widget: Gtk.Widget, {
    onFocusEnter,
    onFocusLeave,
    onKeyPressed,
    onKeyReleased,
    onKeyModifier,
    onLegacy,
    onButtonPressed,
    onButtonReleased,
    onHoverEnter,
    onHoverLeave,
    onMotion,
    onScroll,
    onScrollDecelerate,
    ...props
}: EventController<Gtk.Widget> & T) {
    if (onFocusEnter || onFocusLeave) {
        const focus = new Gtk.EventControllerFocus
        widget.add_controller(focus)

        if (onFocusEnter)
            focus.connect("enter", () => onFocusEnter(widget))

        if (onFocusLeave)
            focus.connect("leave", () => onFocusLeave(widget))
    }

    if (onKeyPressed || onKeyReleased || onKeyModifier) {
        const key = new Gtk.EventControllerKey
        widget.add_controller(key)

        if (onKeyPressed)
            key.connect("key-pressed", (_, val, code, state) => onKeyPressed(widget, val, code, state))

        if (onKeyReleased)
            key.connect("key-released", (_, val, code, state) => onKeyReleased(widget, val, code, state))

        if (onKeyModifier)
            key.connect("modifiers", (_, state) => onKeyModifier(widget, state))
    }

    if (onLegacy || onButtonPressed || onButtonReleased) {
        const legacy = new Gtk.EventControllerLegacy
        widget.add_controller(legacy)

        legacy.connect("event", (_, event) => {
            if (event.get_event_type() === Gdk.EventType.BUTTON_PRESS) {
                onButtonPressed?.(widget, event as Gdk.ButtonEvent)
            }

            if (event.get_event_type() === Gdk.EventType.BUTTON_RELEASE) {
                onButtonReleased?.(widget, event as Gdk.ButtonEvent)
            }

            onLegacy?.(widget, event)
        })
    }

    if (onMotion || onHoverEnter || onHoverLeave) {
        const hover = new Gtk.EventControllerMotion
        widget.add_controller(hover)

        if (onHoverEnter)
            hover.connect("enter", (_, x, y) => onHoverEnter(widget, x, y))

        if (onHoverLeave)
            hover.connect("leave", () => onHoverLeave(widget))

        if (onMotion)
            hover.connect("motion", (_, x, y) => onMotion(widget, x, y))
    }

    if (onScroll || onScrollDecelerate) {
        const scroll = new Gtk.EventControllerScroll
        scroll.flags = Gtk.EventControllerScrollFlags.BOTH_AXES | Gtk.EventControllerScrollFlags.KINETIC
        widget.add_controller(scroll)

        if (onScroll)
            scroll.connect("scroll", (_, x, y) => onScroll(widget, x, y))

        if (onScrollDecelerate)
            scroll.connect("decelerate", (_, x, y) => onScrollDecelerate(widget, x, y))
    }

    return props
}
