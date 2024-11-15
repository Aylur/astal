import Gtk from "gi://Gtk?version=4.0"
import { mergeBindings, type BindableChild } from "./astalify.js"
import * as Widget from "./widget.js"

function isArrowFunction(func: any): func is (args: any) => any {
    return !Object.hasOwn(func, "prototype")
}

export function Fragment({ children = [], child }: {
    child?: BindableChild
    children?: Array<BindableChild>
}) {
    return mergeBindings([...children, child])
}

export function jsx(
    ctor: keyof typeof ctors | typeof Gtk.Widget,
    { children, ...props }: any,
) {
    children ??= []

    if (!Array.isArray(children))
        children = [children]

    children = children.filter(Boolean)

    if (children.length === 1)
        props.child = children[0]
    else if (children.length > 1)
        props.children = children

    if (typeof ctor === "string") {
        return new ctors[ctor](props)
    }

    if (isArrowFunction(ctor))
        return ctor(props)

    // @ts-expect-error can be class or function
    return new ctor(props)
}

const ctors = {
    window: Widget.Window,
}

declare global {
    // eslint-disable-next-line @typescript-eslint/no-namespace
    namespace JSX {
        type Element = Gtk.Widget
        type ElementClass = Gtk.Widget
        interface IntrinsicElements {
            window: Widget.WindowProps
        }
    }
}

export const jsxs = jsx
