import Astal from "gi://Astal"

export function interval(interval: number, callback: () => void) {
    return Astal.Time.interval(interval, callback)
}

export function timeout(timeout: number, callback: () => void) {
    return Astal.Time.timeout(timeout, callback)
}

export function idle(callback: () => void) {
    return Astal.Time.idle(callback)
}
