import { Gtk } from "../imports.js"
import * as Widget from "../widgets.js"
import Binding from "../binding.js"

function w(e: any) {
    return e instanceof Gtk.Widget || e instanceof Binding
        ? e
        : Widget.Label({ label: String(e) })
}

export function jsx(
    ctor: keyof typeof ctors | typeof Gtk.Widget,
    { children, ...props }: any,
) {
    children ??= []

    if (!Array.isArray(children))
        children = [children]
    else
        children = children.flat()

    // <box children={Binding} /> and <box>{Binding}</box>
    if (ctor === "box" && children.length === 1 && children[0] instanceof Binding) {
        props.children = children[0]
    }

    // TODO: handle array of Binding
    // is there a usecase?

    else if (ctor === "centerbox") {
        if (children[0])
            props.startWidget = w(children[0])
        if (children[1])
            props.centerWidget = w(children[1])
        if (children[2])
            props.endWidget = w(children[2])
    }

    else if (ctor === "overlay") {
        const [child, ...overlays] = children
        if (child)
            props.child = child

        props.overlays = overlays
    }

    else if (children.length === 1) {
        props.child = w(children[0])
        delete props.children
    }

    else {
        props.children = children.map(w)
    }

    return typeof ctor === "string"
        ? (ctors as any)[ctor](props)
        : new ctor(props)
}

const ctors = {
    "box": Widget.Box,
    "button": Widget.Button,
    "centerbox": Widget.CenterBox,
    // TODO: circularprogress
    "drawingarea": Widget.DrawingArea,
    "entry": Widget.Entry,
    "eventbox": Widget.EventBox,
    // TODO: fixed
    // TODO: flowbox
    "icon": Widget.Icon,
    "label": Widget.Label,
    "levelbar": Widget.LevelBar,
    // TODO: listbox
    "overlay": Widget.Overlay,
    "revealer": Widget.Revealer,
    "scrollable": Widget.Scrollable,
    "slider": Widget.Slider,
    // TODO: stack
    "switch": Widget.Switch,
    "window": Widget.Window,
}

// eslint-disable-next-line @typescript-eslint/no-namespace
export namespace JSX {
    export type Element = Gtk.Widget
    export interface IntrinsicElements {
        "box": Widget.BoxProps,
        "button": Widget.ButtonProps,
        "centerbox": Widget.CenterBoxProps,
        // TODO: circularprogress
        "drawingarea": Widget.DrawingAreaProps,
        "entry": Widget.EntryProps,
        "eventbox": Widget.EventBoxProps,
        // TODO: fixed
        // TODO: flowbox
        "icon": Widget.IconProps,
        "label": Widget.LabelProps,
        "levelbar": Widget.LevelBarProps,
        // TODO: listbox
        "overlay": Widget.OverlayProps,
        "revealer": Widget.RevealerProps,
        "scrollable": Widget.ScrollableProps,
        "slider": Widget.SliderProps,
        // TODO: stack
        "switch": Widget.SwitchProps,
        "window": Widget.WindowProps,
    }
}

export const jsxs = jsx
