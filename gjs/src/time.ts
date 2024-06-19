import { Astal } from "./imports.js"

export function interval(interval: number, callback?: () => void) {
    return Astal.Time.interval(interval, callback || null)
}

export function timeout(timeout: number, callback?: () => void) {
    return Astal.Time.timeout(timeout, callback || null)
}

export function idle(callback?: () => void) {
    return Astal.Time.idle(callback || null)
}
