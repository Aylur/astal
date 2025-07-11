import GObject from "gi://GObject";

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

export class Binding<Value> implements Subscribable<Value> {
    private transformFn = (v: any) => v

    #emitter: Subscribable<Value> | Connectable
    #prop?: string

    /**
     * Bind to a `Connectable`'s property, preserving its reactivity to be used somewhere else.
     */
    static bind<
        T extends Connectable,
        P extends keyof T,
    >(object: T, property: P): Binding<T[P]>

    /**
     * Bind to a `Subscribable`, preserving its reactivity to be used somewhere else.
     */
    static bind<T>(object: Subscribable<T>): Binding<T>

    static bind(emitter: Connectable | Subscribable, prop?: string) {
        return new Binding(emitter, prop)
    }

    private constructor(emitter: Connectable | Subscribable<Value>, prop?: string) {
        this.#emitter = emitter
        this.#prop = prop && kebabify(prop)
    }

    [Symbol.toPrimitive]() {
        console.warn("Binding implicitly converted to a primitive value. This is almost always a mistake.")
        return this.toString()
    }

    /**
     * This function is mostly here to aid in debugging.
     * It returns a regular, non-reactive string,
     * and will not work to reactively use a binding somewhere that expects a plain string.
     */
    toString() {
        return `Binding<${this.#emitter}${this.#prop ? `, "${this.#prop}"` : ""}>`
    }

    /**
     * Create a new binding that additionally applies a function on its value.
     * @param fn The transformation to apply. This should be a pure function, as it can be called at any time.
     */
    as<T>(fn: (v: Value) => T): Binding<T> {
        const bind = new Binding(this.#emitter, this.#prop)
        bind.transformFn = (v: Value) => fn(this.transformFn(v))
        return bind as unknown as Binding<T>
    }

    /**
     * Get the binding's current value (non-reactively).
     */
    get(): Value {
        if (typeof this.#emitter.get === "function")
            return this.transformFn(this.#emitter.get())

        if (typeof this.#prop === "string") {
            const getter = `get_${snakeify(this.#prop)}`
            if (typeof this.#emitter[getter] === "function")
                return this.transformFn(this.#emitter[getter]())

            return this.transformFn(this.#emitter[this.#prop])
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
                if (GObject.signal_handler_is_connected(this.#emitter, id))
                    (this.#emitter.disconnect as Connectable["disconnect"])(id)
            }
        }
        throw Error(`${this.#emitter} is not bindable`)
    }
}

export const { bind } = Binding
export default Binding
