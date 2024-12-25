import Gtk from "gi://Gtk?version=3.0"
import { type BindableChild } from "./astalify.js"
import { mergeBindings, jsx as _jsx } from "../_astal.js"
import * as Widget from "./widget.js"

export function Fragment({ children = [], child }: {
    child?: BindableChild
    children?: Array<BindableChild>
}) {
    if (child) children.push(child)
    return mergeBindings(children)
}

export function jsx(
    ctor: keyof typeof ctors | typeof Gtk.Widget,
    props: any,
) {
    return _jsx(ctors, ctor as any, props)
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
    menubutton: Widget.MenuButton,
    overlay: Widget.Overlay,
    revealer: Widget.Revealer,
    scrollable: Widget.Scrollable,
    slider: Widget.Slider,
    stack: Widget.Stack,
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
            circularprogress: Widget.CircularProgressProps
            drawingarea: Widget.DrawingAreaProps
            entry: Widget.EntryProps
            eventbox: Widget.EventBoxProps
            // TODO: fixed
            // TODO: flowbox
            icon: Widget.IconProps
            label: Widget.LabelProps
            levelbar: Widget.LevelBarProps
            // TODO: listbox
            menubutton: Widget.MenuButtonProps
            overlay: Widget.OverlayProps
            revealer: Widget.RevealerProps
            scrollable: Widget.ScrollableProps
            slider: Widget.SliderProps
            stack: Widget.StackProps
            switch: Widget.SwitchProps
            window: Widget.WindowProps
        }
    }
}

export const jsxs = jsx
