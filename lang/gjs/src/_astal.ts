import Variable from "./variable.js"
import { execAsync } from "./process.js"
import Binding, { Connectable, kebabify, snakeify, Subscribable } from "./binding.js"

export const noImplicitDestroy = Symbol("no no implicit destroy")
export const setChildren = Symbol("children setter method")

export function mergeBindings(array: any[]) {
    function getValues(...args: any[]) {
        let i = 0
        return array.map(value => value instanceof Binding
            ? args[i++]
            : value,
        )
    }

    const bindings = array.filter(i => i instanceof Binding)

    if (bindings.length === 0)
        return array

    if (bindings.length === 1)
        return bindings[0].as(getValues)

    return Variable.derive(bindings, getValues)()
}

export function setProp(obj: any, prop: string, value: any) {
    try {
        const setter = `set_${snakeify(prop)}`
        if (typeof obj[setter] === "function")
            return obj[setter](value)

        return (obj[prop] = value)
    } catch (error) {
        console.error(`could not set property "${prop}" on ${obj}:`, error)
    }
}

export type BindableProps<T> = {
    [K in keyof T]: Binding<T[K]> | T[K];
}

export function hook<Widget extends Connectable>(
    widget: Widget,
    object: Connectable | Subscribable,
    signalOrCallback: string | ((self: Widget, ...args: any[]) => void),
    callback?: (self: Widget, ...args: any[]) => void,
) {
    if (typeof object.connect === "function" && callback) {
        const id = object.connect(signalOrCallback, (_: any, ...args: unknown[]) => {
            return callback(widget, ...args)
        })
        widget.connect("destroy", () => {
            (object.disconnect as Connectable["disconnect"])(id)
        })
    } else if (typeof object.subscribe === "function" && typeof signalOrCallback === "function") {
        const unsub = object.subscribe((...args: unknown[]) => {
            signalOrCallback(widget, ...args)
        })
        widget.connect("destroy", unsub)
    }
}

export function construct<Widget extends Connectable & { [setChildren]: (children: any[]) => void }>(widget: Widget, config: any) {
    // eslint-disable-next-line prefer-const
    let { setup, child, children = [], ...props } = config

    if (children instanceof Binding) {
        children = [children]
    }

    if (child) {
        children.unshift(child)
    }

    // remove undefined values
    for (const [key, value] of Object.entries(props)) {
        if (value === undefined) {
            delete props[key]
        }
    }

    // collect bindings
    const bindings: Array<[string, Binding<any>]> = Object
        .keys(props)
        .reduce((acc: any, prop) => {
            if (props[prop] instanceof Binding) {
                const binding = props[prop]
                delete props[prop]
                return [...acc, [prop, binding]]
            }
            return acc
        }, [])

    // collect signal handlers
    const onHandlers: Array<[string, string | (() => unknown)]> = Object
        .keys(props)
        .reduce((acc: any, key) => {
            if (key.startsWith("on")) {
                const sig = kebabify(key).split("-").slice(1).join("-")
                const handler = props[key]
                delete props[key]
                return [...acc, [sig, handler]]
            }
            return acc
        }, [])

    // set children
    const mergedChildren = mergeBindings(children.flat(Infinity))
    if (mergedChildren instanceof Binding) {
        widget[setChildren](mergedChildren.get())
        widget.connect("destroy", mergedChildren.subscribe((v) => {
            widget[setChildren](v)
        }))
    } else {
        if (mergedChildren.length > 0) {
            widget[setChildren](mergedChildren)
        }
    }

    // setup signal handlers
    for (const [signal, callback] of onHandlers) {
        const sig = signal.startsWith("notify")
            ? signal.replace("-", "::")
            : signal

        if (typeof callback === "function") {
            widget.connect(sig, callback)
        } else {
            widget.connect(sig, () => execAsync(callback)
                .then(print).catch(console.error))
        }
    }

    // setup bindings handlers
    for (const [prop, binding] of bindings) {
        if (prop === "child" || prop === "children") {
            widget.connect("destroy", binding.subscribe((v: any) => {
                widget[setChildren](v)
            }))
        }
        widget.connect("destroy", binding.subscribe((v: any) => {
            setProp(widget, prop, v)
        }))
        setProp(widget, prop, binding.get())
    }

    // filter undefined values
    for (const [key, value] of Object.entries(props)) {
        if (value === undefined) {
            delete props[key]
        }
    }

    Object.assign(widget, props)
    setup?.(widget)
    return widget
}

function isArrowFunction(func: any): func is (args: any) => any {
    return !Object.hasOwn(func, "prototype")
}

export function jsx(
    ctors: Record<string, { new(props: any): any } | ((props: any) => any)>,
    ctor: string | ((props: any) => any) | { new(props: any): any },
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
        if (isArrowFunction(ctors[ctor]))
            return ctors[ctor](props)

        return new ctors[ctor](props)
    }

    if (isArrowFunction(ctor))
        return ctor(props)

    return new ctor(props)
}
