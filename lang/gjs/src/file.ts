import Astal from "gi://AstalIO"
import Gio from "gi://Gio?version=2.0"

export { Gio }

export function readFile(path: string): string {
    return Astal.read_file(path) || ""
}

export function readFileAsync(path: string): Promise<string> {
    return new Promise((resolve, reject) => {
        Astal.read_file_async(path, (_, res) => {
            try {
                resolve(Astal.read_file_finish(res) || "")
            } catch (error) {
                reject(error)
            }
        })
    })
}

export function writeFile(path: string, content: string, follow_symlinks: boolean = true): void {
    Astal.write_file(path, content, follow_symlinks)
}

export function writeFileAsync(path: string, content: string, follow_symlinks: boolean = true): Promise<void> {
    return new Promise((resolve, reject) => {
        Astal.write_file_async(path, content, follow_symlinks, (_, res) => {
            try {
                resolve(Astal.write_file_finish(res))
            } catch (error) {
                reject(error)
            }
        })
    })
}

export function monitorFile(
    path: string,
    callback: (file: string, event: Gio.FileMonitorEvent) => void,
    follow_symlinks: boolean = true,
): Gio.FileMonitor {
    return Astal.monitor_file(path, (file: string, event: Gio.FileMonitorEvent) => {
        callback(file, event)
    }, follow_symlinks)!
}
