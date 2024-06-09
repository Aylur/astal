import { Gtk } from "../imports.js"
import * as Widget from "../widgets.js"

export function jsx(
    ctor: keyof typeof ctors | typeof Gtk.Widget,
    { children, ...props }: any,
) {
    children ??= []

    if (!Array.isArray(children))
        children = [children]

    if (typeof ctor === "string")
        return (ctors as any)[ctor](props, children)

    if (children.length === 1)
        props.child = children[0]
    else if (children.length > 1)
        props.children = children

    return new ctor(props)
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
