import { Gtk } from "../imports.js"
import * as Widget from "../widgets.js"
import Binding from "../binding.js"

function w(e: any) {
    if (e instanceof Gtk.Widget || e instanceof Binding)
        return e

    return Widget.Label({ label: String(e) })
}

export function jsx(
    ctor: keyof typeof ctors | typeof Gtk.Widget,
    { children, ...props }: any,
) {
    children ??= []

    if (!Array.isArray(children))
        children = [children]

    if (ctor === "centerbox") {
        if (children[0])
            props.startWidget = w(children[0])
        if (children[1])
            props.centerWidget = w(children[1])
        if (children[2])
            props.endWidget = w(children[2])
    }

    else if (ctor === "label" && children[0]) {
        props.label = children[0]
        delete props.children
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
    "eventbox": Widget.EventBox,
    "icon": Widget.Icon,
    "label": Widget.Label,
    "window": Widget.Window,
}

// eslint-disable-next-line @typescript-eslint/no-namespace
export namespace JSX {
    export type Element = Gtk.Widget
    export interface IntrinsicElements {
        "box": Widget.BoxProps
        "button": Widget.ButtonProps
        "centerbox": Widget.CenterBoxProps
        "eventbox": Widget.EventBoxProps
        "icon": Widget.IconProps
        "label": Widget.LabelProps
        "window": Widget.WindowProps
    }
}

export const jsxs = jsx
