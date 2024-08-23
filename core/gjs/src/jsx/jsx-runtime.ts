import { Gtk } from "../imports.js"
import * as Widget from "../widgets.js"

function isArrowFunction(func: any): func is (args: any) => any {
    return !Object.hasOwn(func, "prototype")
}

export function jsx(
    ctor: keyof typeof ctors | typeof Gtk.Widget,
    { children, ...props }: any,
) {
    children ??= []

    if (!Array.isArray(children))
        children = [children]

    children = children.filter(Boolean)

    if (typeof ctor === "string")
        return (ctors as any)[ctor](props, children)

    if (children.length === 1)
        props.child = children[0]
    else if (children.length > 1)
        props.children = children

    if (isArrowFunction(ctor))
        return ctor(props)

    // @ts-expect-error can be class or function
    return new ctor(props)
}

const ctors = {
    box: Widget.Box,
    button: Widget.Button,
    centerbox: Widget.CenterBox,
    circularprogress: Widget.CircularProgress,
    drawingarea: Widget.DrawingArea,
    entry: Widget.Entry,
    eventbox: Widget.EventBox,
    // TODO: fixed
    // TODO: flowbox
    icon: Widget.Icon,
    label: Widget.Label,
    levelbar: Widget.LevelBar,
    // TODO: listbox
    overlay: Widget.Overlay,
    revealer: Widget.Revealer,
    scrollable: Widget.Scrollable,
    slider: Widget.Slider,
    // TODO: stack
    switch: Widget.Switch,
    window: Widget.Window,
}

declare global {
    // eslint-disable-next-line @typescript-eslint/no-namespace
    namespace JSX {
        type Element = Gtk.Widget
        type ElementClass = Gtk.Widget
        interface IntrinsicElements {
            box: Widget.BoxProps
            button: Widget.ButtonProps
            centerbox: Widget.CenterBoxProps
            circularprogress: Widget.CircularProgressProps,
            drawingarea: Widget.DrawingAreaProps
            entry: Widget.EntryProps
            eventbox: Widget.EventBoxProps
            // TODO: fixed
            // TODO: flowbox
            icon: Widget.IconProps
            label: Widget.LabelProps
            levelbar: Widget.LevelBarProps
            // TODO: listbox
            overlay: Widget.OverlayProps
            revealer: Widget.RevealerProps
            scrollable: Widget.ScrollableProps
            slider: Widget.SliderProps
            // TODO: stack
            switch: Widget.SwitchProps
            window: Widget.WindowProps
        }
    }
}

export const jsxs = jsx
