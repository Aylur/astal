import Astal from "gi://AstalIO"

export const { Time } = Astal

export function interval(interval: number, callback?: () => void) {
    return Astal.Time.interval(interval, () => void callback?.())
}

export function timeout(timeout: number, callback?: () => void) {
    return Astal.Time.timeout(timeout, () => void callback?.())
}

export function idle(callback?: () => void) {
    return Astal.Time.idle(() => void callback?.())
}
