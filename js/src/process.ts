type Proc = {
    connect(sig: "stdout" | "stderr", fn: (_: any, out: string) => void): number
}

type Config<P extends Proc> = {
    defaultOut(stdout: string): void
    defaultErr(stdout: string): void
    subprocess(cmd: string): P
    subprocessv(cmd: string[]): P
    exec(cmd: string): string | null
    execv(cmd: string[]): string | null
    execAsync(cmd: string): P
    execAsyncv(cmd: string[]): P
}

type Args<Out = void, Err = void> = {
    cmd: string | string[],
    out?: (stdout: string) => Out,
    err?: (stderr: string) => Err,
}

export default function <P extends Proc>(config: Config<P>) {
    function args<O, E>(argsOrCmd: Args | string | string[], onOut: O, onErr: E) {
        const params = Array.isArray(argsOrCmd) || typeof argsOrCmd === "string"
        return {
            cmd: params ? argsOrCmd : argsOrCmd.cmd,
            err: params ? onErr : argsOrCmd.err || onErr,
            out: params ? onOut : argsOrCmd.out || onOut,
        }
    }

    function subprocess(args: Args): P
    function subprocess(
        cmd: string | string[],
        onOut?: (stdout: string) => void,
        onErr?: (stderr: string) => void,
    ): P
    function subprocess(
        argsOrCmd: Args | string | string[],
        onOut: (stdout: string) => void = config.defaultOut,
        onErr: (stderr: string) => void = config.defaultErr,
    ) {
        const { cmd, err, out } = args(argsOrCmd, onOut, onErr)
        const proc = Array.isArray(cmd)
            ? config.subprocessv(cmd)
            : config.subprocess(cmd)

        proc.connect("stdout", (_, stdout: string) => out(stdout))
        proc.connect("stderr", (_, stderr: string) => err(stderr))
        return proc
    }

    function exec<Out = string, Err = string>(
        args: Args<Out, Err>
    ): Out | Err
    function exec<Out = string, Err = string>(
        cmd: string | string[],
        onOut?: (stdout: string) => Out,
        onErr?: (stderr: string) => Err,
    ): Out | Err
    function exec<Out = string, Err = string>(
        argsOrCmd: Args<Out, Err> | string | string[],
        onOut: (stdout: string) => Out = out => out as Out,
        onErr: (stderr: string) => Err = out => out as Err,
    ): Out | Err {
        const { cmd, err, out } = args(argsOrCmd, onOut, onErr)
        return Array.isArray(cmd)
            ? out(config.execv(cmd)!) as Out
            : err(config.exec(cmd)!) as Err
    }

    function execAsync(cmd: string | string[]): Promise<string> {
        const proc = Array.isArray(cmd)
            ? config.execAsyncv(cmd)
            : config.execAsync(cmd)
        return new Promise((resolve, reject) => {
            proc.connect("stdout", (_, out: string) => resolve(out))
            proc.connect("stderr", (_, err: string) => reject(err))
        })
    }

    return { subprocess, exec, execAsync }
}
