export const kebabify = (str: string) => str
    .replace(/([a-z])([A-Z])/g, "$1-$2")
    .replaceAll("_", "-")
    .toLowerCase()

export interface Subscribable<T = unknown> {
    subscribe(callback: () => void): () => void
    get(): T
    [key: string]: any
}

export interface Connectable {
    connect(signal: string, callback: (...args: any[]) => unknown): number
    disconnect(id: number): void
    [key: string]: any
}

export default class Binding<Value> {
    private emitter: Subscribable<Value> | Connectable
    private prop?: string
    private transformFn = (v: any) => v

    static bind<
        T extends Connectable,
        P extends keyof T,
    >(object: T, property: P): Binding<T[P]>

    static bind<T>(object: Subscribable<T>): Binding<T>

    static bind(emitter: Connectable | Subscribable, prop?: string) {
        return new Binding(emitter, prop)
    }

    private constructor(emitter: Connectable | Subscribable<Value>, prop?: string) {
        this.emitter = emitter
        this.prop = prop && kebabify(prop)
    }

    toString() {
        return `Binding<${this.emitter}${this.prop ? `, "${this.prop}"` : ""}>`
    }

    as<T>(fn: (v: Value) => T): Binding<T> {
        const bind = new Binding(this.emitter, this.prop)
        bind.transformFn = (v: Value) => fn(this.transformFn(v))
        return bind as unknown as Binding<T>
    }

    get(): Value {
        if (typeof this.emitter.get === "function")
            return this.transformFn(this.emitter.get())

        if (typeof this.prop === "string")
            return this.transformFn(this.emitter[this.prop])

        throw Error("can not get value of binding")
    }

    subscribe(callback: (value: Value) => void): () => void {
        if (typeof this.emitter.subscribe === "function") {
            return this.emitter.subscribe(() => {
                callback(this.get())
            })
        }
        else if (typeof this.emitter.connect === "function") {
            const signal = `notify::${this.prop}`
            const id = this.emitter.connect(signal, () => {
                callback(this.get())
            })
            return () => {
                (this.emitter.disconnect as Connectable["disconnect"])(id)
            }
        }
        throw Error(`${this.emitter} is not bindable`)
    }
}

export const { bind } = Binding
