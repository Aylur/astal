import { Astal } from "./imports.js"


export function interval(interval: number, callback: () => void) {
    const t = Astal.Time.interval(interval, null)
    t.connect("now", callback)
    return t
}

export function timeout(timeout: number, callback: () => void) {
    const t = Astal.Time.timeout(timeout, null)
    t.connect("now", callback)
    return t
}

export function idle(callback: () => void) {
    const t = Astal.Time.idle(null)
    t.connect("now", callback)
    return t
}
