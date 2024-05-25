import { Astal } from "./imports.js"

type Process = ReturnType<typeof Astal.Process.subprocess>

type Args<Out = void, Err = void> = {
    cmd: string | string[],
    out?: (stdout: string) => Out,
    err?: (stderr: string) => Err,
}

export function args<O, E>(argsOrCmd: Args | string | string[], onOut: O, onErr: E) {
    const params = Array.isArray(argsOrCmd) || typeof argsOrCmd === "string"
    return {
        cmd: params ? argsOrCmd : argsOrCmd.cmd,
        err: params ? onErr : argsOrCmd.err || onErr,
        out: params ? onOut : argsOrCmd.out || onOut,
    }
}

export function subprocess(args: Args): Process
export function subprocess(
    cmd: string | string[],
    onOut?: (stdout: string) => void,
    onErr?: (stderr: string) => void,
): Process
export function subprocess(
    argsOrCmd: Args | string | string[],
    onOut: (stdout: string) => void = console.log,
    onErr: (stderr: string) => void = console.error,
) {
    const { cmd, err, out } = args(argsOrCmd, onOut, onErr)
    const proc = Array.isArray(cmd)
        ? Astal.Process.subprocessv(cmd)
        : Astal.Process.subprocess(cmd)

    proc.connect("stdout", (_: any, stdout: string) => out(stdout))
    proc.connect("stderr", (_: any, stderr: string) => err(stderr))
    return proc
}

export function exec<Out = string, Err = string>(
    args: Args<Out, Err>
): Out | Err
export function exec<Out = string, Err = string>(
    cmd: string | string[],
    onOut?: (stdout: string) => Out,
    onErr?: (stderr: string) => Err,
): Out | Err
export function exec<Out = string, Err = string>(
    argsOrCmd: Args<Out, Err> | string | string[],
    onOut: (stdout: string) => Out = out => out as Out,
    onErr: (stderr: string) => Err = out => out as Err,
): Out | Err {
    const { cmd, err, out } = args(argsOrCmd, onOut, onErr)
    return Array.isArray(cmd)
        ? out(Astal.Process.execv(cmd)!) as Out
        : err(Astal.Process.exec(cmd)!) as Err
}

export function execAsync(cmd: string | string[]): Promise<string> {
    const proc = Array.isArray(cmd)
        ? Astal.Process.execAsyncv(cmd)
        : Astal.Process.execAsync(cmd)
    return new Promise((resolve, reject) => {
        proc.connect("stdout", (_: any, out: string) => resolve(out))
        proc.connect("stderr", (_: any, err: string) => reject(err))
    })
}
