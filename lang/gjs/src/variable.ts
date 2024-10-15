import Astal from "gi://AstalIO"
import Binding, { type Connectable, type Subscribable } from "./binding.js"
import { interval } from "./time.js"
import { execAsync, subprocess } from "./process.js"

class VariableWrapper<T> extends Function {
    private variable!: Astal.VariableBase
    private errHandler? = console.error

    private _value: T
    private _poll?: Astal.Time
    private _watch?: Astal.Process

    private pollInterval = 1000
    private pollExec?: string[] | string
    private pollTransform?: (stdout: string, prev: T) => T
    private pollFn?: (prev: T) => T | Promise<T>

    private watchTransform?: (stdout: string, prev: T) => T
    private watchExec?: string[] | string

    constructor(init: T) {
        super()
        this._value = init
        this.variable = new Astal.VariableBase()
        this.variable.connect("dropped", () => {
            this.stopWatch()
            this.stopPoll()
        })
        this.variable.connect("error", (_, err) => this.errHandler?.(err))
        return new Proxy(this, {
            apply: (target, _, args) => target._call(args[0]),
        })
    }

    private _call<R = T>(transform?: (value: T) => R): Binding<R> {
        const b = Binding.bind(this)
        return transform ? b.as(transform) : b as unknown as Binding<R>
    }

    toString() {
        return String(`Variable<${this.get()}>`)
    }

    get(): T { return this._value }
    set(value: T) {
        if (value !== this._value) {
            this._value = value
            this.variable.emit("changed")
        }
    }

    startPoll() {
        if (this._poll)
            return

        if (this.pollFn) {
            this._poll = interval(this.pollInterval, () => {
                const v = this.pollFn!(this.get())
                if (v instanceof Promise) {
                    v.then(v => this.set(v))
                        .catch(err => this.variable.emit("error", err))
                }
                else {
                    this.set(v)
                }
            })
        }
        else if (this.pollExec) {
            this._poll = interval(this.pollInterval, () => {
                execAsync(this.pollExec!)
                    .then(v => this.set(this.pollTransform!(v, this.get())))
                    .catch(err => this.variable.emit("error", err))
            })
        }
    }

    startWatch() {
        if (this._watch)
            return

        this._watch = subprocess({
            cmd: this.watchExec!,
            out: out => this.set(this.watchTransform!(out, this.get())),
            err: err => this.variable.emit("error", err),
        })
    }

    stopPoll() {
        this._poll?.cancel()
        delete this._poll
    }

    stopWatch() {
        this._watch?.kill()
        delete this._watch
    }

    isPolling() { return !!this._poll }
    isWatching() { return !!this._watch }

    drop() {
        this.variable.emit("dropped")
    }

    onDropped(callback: () => void) {
        this.variable.connect("dropped", callback)
        return this as unknown as Variable<T>
    }

    onError(callback: (err: string) => void) {
        delete this.errHandler
        this.variable.connect("error", (_, err) => callback(err))
        return this as unknown as Variable<T>
    }

    subscribe(callback: (value: T) => void) {
        const id = this.variable.connect("changed", () => {
            callback(this.get())
        })
        return () => this.variable.disconnect(id)
    }

    poll(
        interval: number,
        exec: string | string[],
        transform?: (stdout: string, prev: T) => T
    ): Variable<T>

    poll(
        interval: number,
        callback: (prev: T) => T | Promise<T>
    ): Variable<T>

    poll(
        interval: number,
        exec: string | string[] | ((prev: T) => T | Promise<T>),
        transform: (stdout: string, prev: T) => T = out => out as T,
    ) {
        this.stopPoll()
        this.pollInterval = interval
        this.pollTransform = transform
        if (typeof exec === "function") {
            this.pollFn = exec
            delete this.pollExec
        }
        else {
            this.pollExec = exec
            delete this.pollFn
        }
        this.startPoll()
        return this as unknown as Variable<T>
    }

    watch(
        exec: string | string[],
        transform: (stdout: string, prev: T) => T = out => out as T,
    ) {
        this.stopWatch()
        this.watchExec = exec
        this.watchTransform = transform
        this.startWatch()
        return this as unknown as Variable<T>
    }

    observe(
        objs: Array<[obj: Connectable, signal: string]>,
        callback: (...args: any[]) => T,
    ): Variable<T>

    observe(
        obj: Connectable,
        signal: string,
        callback: (...args: any[]) => T,
    ): Variable<T>

    observe(
        objs: Connectable | Array<[obj: Connectable, signal: string]>,
        sigOrFn: string | ((obj: Connectable, ...args: any[]) => T),
        callback?: (obj: Connectable, ...args: any[]) => T,
    ) {
        const f = typeof sigOrFn === "function" ? sigOrFn : callback ?? (() => this.get())
        const set = (obj: Connectable, ...args: any[]) => this.set(f(obj, ...args))

        if (Array.isArray(objs)) {
            for (const obj of objs) {
                const [o, s] = obj
                const id = o.connect(s, set)
                this.onDropped(() => o.disconnect(id))
            }
        }
        else {
            if (typeof sigOrFn === "string") {
                const id = objs.connect(sigOrFn, set)
                this.onDropped(() => objs.disconnect(id))
            }
        }

        return this as unknown as Variable<T>
    }

    static derive<
        const Deps extends Array<Subscribable<any>>,
        Args extends {
            [K in keyof Deps]: Deps[K] extends Subscribable<infer T> ? T : never
        },
        V = Args,
    >(deps: Deps, fn: (...args: Args) => V = (...args) => args as unknown as V) {
        const update = () => fn(...deps.map(d => d.get()) as Args)
        const derived = new Variable(update())
        const unsubs = deps.map(dep => dep.subscribe(() => derived.set(update())))
        derived.onDropped(() => unsubs.map(unsub => unsub()))
        return derived
    }
}

export interface Variable<T> extends Omit<VariableWrapper<T>, "bind"> {
    <R>(transform: (value: T) => R): Binding<R>
    (): Binding<T>
}

export const Variable = new Proxy(VariableWrapper as any, {
    apply: (_t, _a, args) => new VariableWrapper(args[0]),
}) as {
    derive: typeof VariableWrapper["derive"]
    <T>(init: T): Variable<T>
    new<T>(init: T): Variable<T>
}

export default Variable
