import { Astal } from "./imports.js"

type Args<Out = void, Err = void> = {
    cmd: string | string[]
    out?: (stdout: string) => Out
    err?: (stderr: string) => Err
}

function args<O, E>(argsOrCmd: Args | string | string[], onOut: O, onErr: E) {
    const params = Array.isArray(argsOrCmd) || typeof argsOrCmd === "string"
    return {
        cmd: params ? argsOrCmd : argsOrCmd.cmd,
        err: params ? onErr : argsOrCmd.err || onErr,
        out: params ? onOut : argsOrCmd.out || onOut,
    }
}

export function subprocess(args: Args): Astal.Process
export function subprocess(
    cmd: string | string[],
    onOut?: (stdout: string) => void,
    onErr?: (stderr: string) => void,
): Astal.Process
export function subprocess(
    argsOrCmd: Args | string | string[],
    onOut: (stdout: string) => void = print,
    onErr: (stderr: string) => void = printerr,
) {
    const { cmd, err, out } = args(argsOrCmd, onOut, onErr)
    const proc = Array.isArray(cmd)
        ? Astal.Process.subprocessv(cmd)
        : Astal.Process.subprocess(cmd)

    proc.connect("stdout", (_, stdout: string) => out(stdout))
    proc.connect("stderr", (_, stderr: string) => err(stderr))
    return proc
}

/** @throws {GLib.Error} Throws stderr */
export function exec(cmd: string | string[]) {
    return Array.isArray(cmd)
        ? Astal.Process.execv(cmd)
        : Astal.Process.exec(cmd)
}

export function execAsync(cmd: string | string[]): Promise<string> {
    return new Promise((resolve, reject) => {
        if (Array.isArray(cmd)) {
            Astal.Process.exec_asyncv(cmd, (_, res) => {
                try {
                    resolve(Astal.Process.exec_asyncv_finish(res))
                }
                catch (error) {
                    reject(error)
                }
            })
        }
        else {
            Astal.Process.exec_async(cmd, (_, res) => {
                try {
                    resolve(Astal.Process.exec_finish(res))
                }
                catch (error) {
                    reject(error)
                }
            })
        }
    })
}
