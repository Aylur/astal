import { Gtk } from "../imports.js"
import * as Widget from "../widgets.js"

export function jsx(
    ctor: keyof typeof ctors | typeof Gtk.Widget,
    { children, ...props }: any,
) {
    children ??= []

    if (!Array.isArray(children))
        children = [children]

    if (children.length === 1) {
        props.child = children[0]
        delete props.children
    }

    children = children.map((v: any) => {
        if (v instanceof Gtk.Widget)
            return v

        return Widget.Label({ label: String(v) })
    })

    if (ctor === Widget.CenterBox) {
        if (children[0])
            props.startWidget = children[0]
        if (children[1])
            props.centerWidget = children[1]
        if (children[2])
            props.endWidget = children[2]
    }

    if (ctor === Widget.Label) {
        if (children[0])
            props.label = children[0]
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
