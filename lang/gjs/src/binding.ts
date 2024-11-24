export const snakeify = (str: string) => str
    .replace(/([a-z])([A-Z])/g, "$1_$2")
    .replaceAll("-", "_")
    .toLowerCase()

export const kebabify = (str: string) => str
    .replace(/([a-z])([A-Z])/g, "$1-$2")
    .replaceAll("_", "-")
    .toLowerCase()

export interface Subscribable<T = unknown> {
    subscribe(callback: (value: T) => void): () => void
    get(): T
    [key: string]: any
}

export interface Connectable {
    connect(signal: string, callback: (...args: any[]) => unknown): number
    disconnect(id: number): void
    [key: string]: any
}

type UnwrapBinding<T> = T extends Binding<infer Value> ? Value : T

export default abstract class Binding<T> implements Subscribable<T> {
    abstract toString(): string
    abstract get(): T
    abstract subscribe(callback: (value: T) => void): () => void

    as<R>(
        fn: (v: T) => R,
    ): TransformBinding<T, R> {
        return new TransformBinding(this, fn)
    }

    // This wizardry is simultaneously the least and most cursed bit of TypeScript I've ever written.
    prop<T extends Connectable, K extends keyof T>(this: Binding<T>, key: K): Binding<T[K]> {
        return this.as((obj) => {
            if (typeof obj.connect !== "function") {
                throw new Error("Binding.prop only works on bindings containing Connectables")
            }
            return bind(obj, key)
        })
    }
}

export class TransformBinding<Input, Output> extends Binding<UnwrapBinding<Output>> {
    #source: Subscribable<Input>
    /** `this.#source`'s `subscribe()` return value. Called when `this.#subscribers` becomes empty. */
    #outerCleanup: (() => void) | null
    /** `this.#value`'s `subscribe()` return value. Called when `this.#value` is replaced. */
    #innerCleanup: (() => void) | null
    #transformFn: (v: Input) => Output
    /** To track the bound value's reactivity, this object only has one subscription to `this.#source`.
     * This means that tracking its subscribers can't be delegated to the source object, like in a regular `Binding`.
     */
    #subscribers: Set<(value: UnwrapBinding<Output>) => void>
    #value!: Output

    constructor(source: Subscribable<Input>, fn: (v: Input) => Output) {
        super()
        this.#source = source
        this.#outerCleanup = null
        this.#innerCleanup = null
        this.#transformFn = fn
        this.#subscribers = new Set()
        this.#recomputeValue()
    }

    toString() {
        return `TransformBinding<${this.#source}, ${this.#transformFn}>`
    }

    get(): UnwrapBinding<Output> {
        return this.#value instanceof Binding ? this.#value.get() : this.#value
    }

    subscribe(callback: (value: UnwrapBinding<Output>) => void) {
        // Set up the source subscription if someone's subscribing for the first time.
        if (this.#subscribers.size === 0) {
            this.#outerCleanup = this.#source.subscribe(() => {
                this.#invalidateOuter()
            })
        }

        this.#subscribers.add(callback)

        return () => {
            this.#subscribers.delete(callback)
            // After deleting the last subscription, clean up the source's subscription
            if (this.#subscribers.size === 0) {
                this.#cleanup()
            }
        }
    }

    #notify() {
        const value = this.get()
        for (const sub of this.#subscribers) {
            sub(value)
        }
    }

    #recomputeValue() {
        this.#value = this.#transformFn(this.#source.get())
        if (this.#value instanceof Binding) {
            this.#innerCleanup = this.#value.subscribe(() =>
                this.#invalidateInner(),
            )
        }
    }

    #invalidateOuter() {
        // this.#value has been replaced with a new one.
        // Remove it and clean up the inner value's reactivity if there is any.
        if (this.#innerCleanup) {
            this.#innerCleanup()
            this.#innerCleanup = null
        }
        this.#recomputeValue()
        this.#notify()
    }

    #invalidateInner() {
        // this.#value is a binding and has changed its value. Notify all the subscribers.
        console.assert(
            this.#value instanceof Binding,
            "Inner value invalidated when it's not a Binding",
        )
        this.#notify()
    }

    #cleanup() {
        if (!this.#outerCleanup) {
            throw new Error("Can't cleanup reactivity")
        }
        this.#outerCleanup()
        this.#outerCleanup = null
    }
}

export class DataBinding<Value> extends Binding<Value> {
    #emitter: Subscribable<Value> | Connectable
    #prop?: string

    static bind<
        T extends Connectable,
        P extends keyof T,
    >(object: T, property: P): DataBinding<T[P]>

    static bind<T>(object: Subscribable<T>): DataBinding<T>

    static bind(emitter: Connectable | Subscribable, prop?: string) {
        return new DataBinding(emitter, prop)
    }

    private constructor(emitter: Connectable | Subscribable<Value>, prop?: string) {
        super()
        this.#emitter = emitter
        this.#prop = prop && kebabify(prop)
    }

    toString() {
        return `DataBinding<${this.#emitter}${this.#prop ? `, "${this.#prop}"` : ""}>`
    }

    get(): Value {
        if (typeof this.#emitter.get === "function") return this.#emitter.get()

        if (typeof this.#prop === "string") {
            const getter = `get_${snakeify(this.#prop)}`
            if (typeof this.#emitter[getter] === "function") {
                this.#emitter[getter]()
            }

            return this.#emitter[this.#prop]
        }

        throw Error("can not get value of binding")
    }

    subscribe(callback: (value: Value) => void): () => void {
        if (typeof this.#emitter.subscribe === "function") {
            return this.#emitter.subscribe(() => {
                callback(this.get())
            })
        }
        else if (typeof this.#emitter.connect === "function") {
            const signal = `notify::${this.#prop}`
            const id = this.#emitter.connect(signal, () => {
                callback(this.get())
            })
            return () => {
                (this.#emitter.disconnect as Connectable["disconnect"])(id)
            }
        }
        throw Error(`${this.#emitter} is not bindable`)
    }
}

export const { bind } = DataBinding
