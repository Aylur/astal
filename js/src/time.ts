interface Time {
    connect(sig: "now", fn: () => void): number
    cancel(): void
}

export default function Time<T extends Time>(Time: {
    interval(interval: number, closure: any): T
    timeout(timeout: number, closure: any): T
    idle(closure: any): T
}) {
    function interval(interval: number, callback: () => void) {
        const t = Time.interval(interval, null)
        t.connect("now", callback)
        return t
    }
    function timeout(timeout: number, callback: () => void) {
        const t = Time.timeout(timeout, null)
        t.connect("now", callback)
        return t
    }
    function idle(callback: () => void) {
        const t = Time.idle(null)
        t.connect("now", callback)
        return t
    }
    return { interval, timeout, idle }
}
