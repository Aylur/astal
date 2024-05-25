import { Gtk, Astal } from "../imports.js"
import * as Widget from "../widgets.js"

export function jsx(
    ctor: keyof typeof ctors | typeof Gtk.Widget,
    { children, ...props }: any,
) {
    let ch: any[] = []
    if (Array.isArray(children)) {
        ch = children.map(v => {
            if (v instanceof Gtk.Widget)
                return v

            return Widget.Label({ label: String(v) })
        })
    }
    else if (children instanceof Gtk.Widget) {
        ch = [children]
    }

    const widget = typeof ctor === "string"
        ? (ctors as any)[ctor](props)
        : new ctor(props)

    if (widget instanceof Astal.CenterBox) {
        if (ch[0])
            widget.startWidget = ch[0]
        if (ch[1])
            widget.centerWidget = ch[1]
        if (ch[2])
            widget.endWidget = ch[2]
    } else {
        for (const w of ch) {
            if (widget instanceof Gtk.Container)
                widget.add(w)
        }
    }
    return widget
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
