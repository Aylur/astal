export const snakeify = (str: string) => str
    .replace(/([a-z])([A-Z])/g, "$1_$2")
    .replaceAll("-", "_")
    .toLowerCase()

export const kebabify = (str: string) => str
    .replace(/([a-z])([A-Z])/g, "$1-$2")
    .replaceAll("_", "-")
    .toLowerCase()

/**
 * A reactive source of a single value.
 */
export interface Subscribable<T = unknown> {
    /**
     * Subscribe to updates on the value.
     * @param callback The function to call when the value changes
     * @returns A function to cancel the subscription
     */
    subscribe(callback: (value: T) => void): () => void
    /**
     * Get the current value (non-reactively).
     */
    get(): T
    [key: string]: any
}

/**
 * A reactive object with many signals that can be connected to individually.
 * Usually, these are going to be GObjects.
 */
export interface Connectable {
    connect(signal: string, callback: (...args: any[]) => unknown): number
    disconnect(id: number): void
    [key: string]: any
}

type UnwrapBinding<T> = T extends Binding<infer Value> ? Value : T

export abstract class Binding<T> implements Subscribable<T> {
    /**
     * This function is mostly here to aid in debugging.
     * It returns a regular, non-reactive string,
     * and will not work to reactively use a binding somewhere that expects a plain string.
     */
    abstract toString(): string
    /**
     * Get the binding's current value (non-reactively).
     */
    abstract get(): T
    abstract subscribe(callback: (value: T) => void): () => void

    /**
     * Create a new binding that additionally applies a function on its value.
     * If `fn` returns a `Binding`, it will also be unwrapped.
     * @param fn The transformation to apply. This should be a pure function, as it can be called at any time.
     */
    as<R>(
        fn: (v: T) => R,
    ): TransformBinding<T, R> {
        return new TransformBinding(this, fn)
    }

    /**
     * Create a new binding that accesses one of the value's fields.
     * Note that the binding must be to a `Connectable` for this to work.
     * @param key The field name
     */
    prop<T extends Connectable, K extends keyof T>(this: Binding<T>, key: K): Binding<T[K]> {
        return this.as((obj) => {
            if (typeof obj.connect !== "function") {
                throw new Error("Binding.prop only works on bindings containing Connectables")
            }
            return bind(obj, key)
        })
    }

    [Symbol.toPrimitive]() {
        console.warn("Binding implicitly converted to a primitive value. This is almost always a mistake.");
        return this.toString();
    }
}

export class TransformBinding<Input, Output> extends Binding<UnwrapBinding<Output>> {
    #source: Subscribable<Input>
    /** `this.#source`'s `subscribe()` return value. Called when `this.#subscribers` becomes empty. */
    #outerCleanup: (() => void) | null
    /** `this.#value`'s `subscribe()` return value. Called when `this.#value` is replaced. */
    #innerCleanup: (() => void) | null
    /**
     * This function is largely untyped so that TransformBinding<?, T> can be assigned to Binding<T | undefined>.
     * Otherwise, TypeScript complains because it can't guarantee that this function won't later be called with an undefined.
     * But bindings don't work like that (the parameters to this function are only obtained internally),
     * so it's safe (I think) to bypass TS here.
     */
    #transformFn: (v: any) => any
    /**
     * To track the bound value's reactivity, this object only has one subscription to `this.#source`.
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
        // Set up the source and value subscriptions if someone's subscribing for the first time
        if (this.#subscribers.size === 0) {
            console.assert(this.#outerCleanup === null, "Outer cleanup function is about to be lost!")
            this.#outerCleanup = this.#source.subscribe(() => {
                this.#invalidateOuter()
            })
            if (this.#value instanceof Binding) {
                console.assert(this.#innerCleanup === null, "Inner cleanup function is about to be lost!")
                this.#innerCleanup = this.#value.subscribe(() => this.#invalidateInner())
            }
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
        // Ensure that reactivity is tracked lazily
        if (this.#value instanceof Binding && this.#subscribers.size > 0) {
            console.assert(this.#innerCleanup === null, "Inner cleanup function is about to be lost!")
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

        if (this.#innerCleanup) {
            this.#innerCleanup()
            this.#innerCleanup = null
        }
    }
}

export class DataBinding<Value> extends Binding<Value> {
    #emitter: Subscribable<Value> | Connectable
    #prop?: string

    /**
     * Bind to a `Connectable`'s property, preserving its reactivity to be used somewhere else.
     */
    static bind<
        T extends Connectable,
        P extends keyof T,
    >(object: T, property: P): DataBinding<T[P]>

    /**
     * Bind to a `Subscribable`, preserving its reactivity to be used somewhere else.
     */
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
                return this.#emitter[getter]()
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
        } else if (typeof this.#emitter.connect === "function") {
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
