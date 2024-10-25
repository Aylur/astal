# Utilities

## File functions

Import from `astal` or `astal/file`

```ts
import { readFile, readFileAsync, writeFile, writeFileAsync, monitorFile, } from "astal/file"
```

### Reading files

```ts
function readFile(path: string): string
function readFileAsync(path: string): Promise<string>
```

### Writing files

```ts
function writeFile(path: string, content: string): void
function writeFileAsync(path: string, content: string): Promise<void>
```

### Monitoring files

If `path` is a directory it will be recursively monitored.

```ts
function monitorFile(
    path: string,
    callback: (file: string, event: Gio.FileMonitorEvent) => void,
): Gio.FileMonitor
```

## Timeouts and Intervals

Import from `astal` or `astal/time`

```ts
import { interval, timeout, idle } from "astal/time"
```

You can use javascript native `setTimeout` or `setInterval`
they return a [GLib.Source](https://docs.gtk.org/glib/struct.Source.html) instance.
Alternatively you can use these functions provided by Astal,
which return an [AstalIO.Time](https://aylur.github.io/libastal/io/class.Time.html) instance.

`AstalIO.Time` has a `now` signal and a `cancelled` signal.

### Interval

Will immediately execute the function and every `interval` millisecond.

```ts
function interval(interval: number, callback?: () => void): AstalIO.Time
```

### Timeout

Will execute the `callback` after `timeout` millisecond.

```ts
function timeout(timeout: number, callback?: () => void): AstalIO.Time
```

### Idle

Executes `callback` whenever there are no higher priority events pending.

```ts
function idle(callback?: () => void): AstalIO.Time
```

Example:

```ts
const timer = interval(1000, () => {
    console.log("optional callback")
})

timer.connect("now", () => {
    console.log("tick")
})

timer.connect("cancelled", () => {
    console.log("cancelled")
})

timer.cancel()
```

## Process functions

Import from `astal` or `astal/process`

```ts
import { subprocess, exec, execAsync } from "astal/process"
```

### Subprocess

You can start a subprocess and run callback functions whenever it outputs to
stdout or stderr. [AstalIO.Process](https://aylur.github.io/libastal/io/class.Process.html) has a `stdout` and `stderr` signal.

```ts
function subprocess(args: {
    cmd: string | string[]
    out?: (stdout: string) => void
    err?: (stderr: string) => void
}): AstalIO.Process

function subprocess(
    cmd: string | string[],
    onOut?: (stdout: string) => void,
    onErr?: (stderr: string) => void,
): AstalIO.Process
```

Example:

```ts
const proc = subprocess(
    "some-command",
    (out) => console.log(out), // optional
    (err) => console.error(out), // optional
)

// or with signals
const proc = subprocess("some-command")
proc.connect("stdout", (_, out) => console.log(out))
proc.connect("stderr", (_, err) => console.error(err))
```

### Executing external commands and scripts

```ts
function exec(cmd: string | string[]): string
function execAsync(cmd: string | string[]): Promise<string>
```

Example:

```ts
try {
    const out = exec("/path/to/script")
    console.log(out)
} catch (err) {
    console.error(err)
}

execAsync(["bash", "-c", "/path/to/script.sh"])
    .then((out) => console.log(out))
    .catch((err) => console.error(err))
```

:::warning
`subprocess`, `exec`, and `execAsync` executes the passed executable as is.
They are **not** executed in a shell environment,
they do **not** expand ENV variables like `$HOME`,
and they do **not** handle logical operators like `&&` and `||`.

If you want bash, run them with bash.

```ts
exec(["bash", "-c", "command $VAR && command"])
exec("bash -c 'command $VAR' && command")
```

:::
