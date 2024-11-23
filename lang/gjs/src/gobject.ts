import GObject from "gi://GObject"

export { default as GLib } from "gi://GLib?version=2.0"
export { GObject, GObject as default }

const meta = Symbol("meta")
const priv = Symbol("priv")

const { ParamSpec, ParamFlags } = GObject

const kebabify = (str: string) => str
    .replace(/([a-z])([A-Z])/g, "$1-$2")
    .replaceAll("_", "-")
    .toLowerCase()

type SignalDeclaration = {
    flags?: GObject.SignalFlags
    accumulator?: GObject.AccumulatorType
    return_type?: GObject.GType
    param_types?: Array<GObject.GType>
}

type PropertyDeclaration =
    | InstanceType<typeof GObject.ParamSpec>
    | { $gtype: GObject.GType }
    | typeof String
    | typeof Number
    | typeof Boolean
    | typeof Object

type GObjectConstructor = {
    [meta]?: {
        Properties?: { [key: string]: GObject.ParamSpec }
        Signals?: { [key: string]: GObject.SignalDefinition }
    }
    new(...args: any[]): any
}

type MetaInfo = GObject.MetaInfo<never, Array<{ $gtype: GObject.GType }>, never>

export function register(options: MetaInfo = {}) {
    return function (cls: GObjectConstructor) {
        const t = options.Template
        if (typeof t === "string" && !t.startsWith("resource://") && !t.startsWith("file://")) {
            // assume xml template
            options.Template = new TextEncoder().encode(t)
        }

        GObject.registerClass({
            Signals: { ...cls[meta]?.Signals },
            Properties: { ...cls[meta]?.Properties },
            ...options,
        }, cls)

        delete cls[meta]
    }
}

export function property(declaration: PropertyDeclaration = Object) {
    return function (target: any, prop: any, desc?: PropertyDescriptor) {
        target.constructor[meta] ??= {}
        target.constructor[meta].Properties ??= {}

        const name = kebabify(prop)

        if (!desc) {
            Object.defineProperty(target, prop, {
                get() {
                    return this[priv]?.[prop] ?? defaultValue(declaration)
                },
                set(v: any) {
                    if (v !== this[prop]) {
                        this[priv] ??= {}
                        this[priv][prop] = v
                        this.notify(name)
                    }
                },
            })

            Object.defineProperty(target, `set_${name.replace("-", "_")}`, {
                value(v: any) {
                    this[prop] = v
                },
            })

            Object.defineProperty(target, `get_${name.replace("-", "_")}`, {
                value() {
                    return this[prop]
                },
            })

            target.constructor[meta].Properties[kebabify(prop)] = pspec(name, ParamFlags.READWRITE, declaration)
        }

        else {
            let flags = 0
            if (desc.get) flags |= ParamFlags.READABLE
            if (desc.set) flags |= ParamFlags.WRITABLE

            target.constructor[meta].Properties[kebabify(prop)] = pspec(name, flags, declaration)
        }
    }
}

export function signal(...params: Array<{ $gtype: GObject.GType } | typeof Object>):
(target: any, signal: any, desc?: PropertyDescriptor) => void

export function signal(declaration?: SignalDeclaration):
(target: any, signal: any, desc?: PropertyDescriptor) => void

export function signal(
    declaration?: SignalDeclaration | { $gtype: GObject.GType } | typeof Object,
    ...params: Array<{ $gtype: GObject.GType } | typeof Object>
) {
    return function (target: any, signal: any, desc?: PropertyDescriptor) {
        target.constructor[meta] ??= {}
        target.constructor[meta].Signals ??= {}

        const name = kebabify(signal)

        if (declaration || params.length > 0) {
            // @ts-expect-error TODO: type assert
            const arr = [declaration, ...params].map(v => v.$gtype)
            target.constructor[meta].Signals[name] = {
                param_types: arr,
            }
        }
        else {
            target.constructor[meta].Signals[name] = declaration || {
                param_types: [],
            }
        }

        if (!desc) {
            Object.defineProperty(target, signal, {
                value: function (...args: any[]) {
                    this.emit(name, ...args)
                },
            })
        }
        else {
            const og: ((...args: any[]) => void) = desc.value
            desc.value = function (...args: any[]) {
                // @ts-expect-error not typed
                this.emit(name, ...args)
            }
            Object.defineProperty(target, `on_${name.replace("-", "_")}`, {
                value: function (...args: any[]) {
                    return og(...args)
                },
            })
        }
    }
}

function pspec(name: string, flags: number, declaration: PropertyDeclaration) {
    if (declaration instanceof ParamSpec)
        return declaration

    switch (declaration) {
        case String:
            return ParamSpec.string(name, "", "", flags, "")
        case Number:
            return ParamSpec.double(name, "", "", flags, -Number.MAX_VALUE, Number.MAX_VALUE, 0)
        case Boolean:
            return ParamSpec.boolean(name, "", "", flags, false)
        case Object:
            return ParamSpec.jsobject(name, "", "", flags)
        default:
            // @ts-expect-error misstyped
            return ParamSpec.object(name, "", "", flags, declaration.$gtype)
    }
}

function defaultValue(declaration: PropertyDeclaration) {
    if (declaration instanceof ParamSpec)
        return declaration.get_default_value()

    switch (declaration) {
        case String:
            return "default-string"
        case Number:
            return 0
        case Boolean:
            return false
        case Object:
        default:
            return null
    }
}
