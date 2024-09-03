# Utilities

## File functions

Import them from `astal` or `astal/file`

```js
import {
    readFile,
    readFileAsync,
    writeFile,
    writeFileAsync,
    monitorFile,
} from "astal"
```

### Reading files

```typescript
function readFile(path: string): string
function readFileAsync(path: string): Promise<string>
```

### Writing files

```typescript
function writeFile(path: string, content: string): void
function writeFileAsync(path: string, content: string): Promise<void>
```

### Monitoring files

```typescript
function monitorFile(
    path: string,
    callback: (file: string, event: Gio.FileMonitorEvent) => void,
): Gio.FileMonitor
```

## Timeouts and Intervals

Import them from `astal` or `astal/time`

```js
import { interval, timeout, idle } from "astal"
```

You can use javascript native `setTimeout` or `setInterval`
they return a [GLib.Source](https://docs.gtk.org/glib/struct.Source.html) instance.
Alternatively you can use these functions provided by Astal,
which return an [Astal.Time]() instance.

`Astal.Time` has a `now` signal and a `cancelled` signal.

### Interval

Will immediately execute the function and every `interval` millisecond.

```typescript
function interval(interval: number, callback?: () => void): Astal.Time
```

### Timeout

Will execute the `callback` after `timeout` millisecond.

```typescript
function timeout(timeout: number, callback?: () => void): Astal.Time
```

### Idle

Executes `callback` whenever there are no higher priority events pending.

```typescript
function idle(callback?: () => void): Astal.Time
```

Example:

```typescript
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

Import them from `astal` or `astal/proc`

```js
import { subprocess, exec, execAsync } from "astal"
```

### Subprocess

You can start a subprocess and run callback functions whenever it outputs to
stdout or stderr. [Astal.Process]() has a `stdout` and `stderr` signal.

```typescript
function subprocess(args: {
    cmd: string | string[]
    out?: (stdout: string) => void
    err?: (stderr: string) => void
}): Astal.Process

function subprocess(
    cmd: string | string[],
    onOut?: (stdout: string) => void,
    onErr?: (stderr: string) => void,
): Astal.Process
```

Example:

```typescript
const proc = subprocess(
    "some-command",
    (out) => console.log(out), // optional
    (err) => console.error(out), // optional
)

// or with signals
const proc = subprocess("some-command")
proc.connect("stdout", (out) => console.log(out))
proc.connect("stderr", (err) => console.error(err))
```

### Executing external commands and scripts

```typescript
function exec(cmd: string | string[]): string
function execAsync(cmd: string | string[]): Promise<string>
```

Example:

```typescript
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
they do **not** expand env variables like `$HOME`,
and they do **not** handle logical operators like `&&` and `||`.

If you want bash, run them with bash.

```js
exec(["bash", "-c", "command $VAR && command"])
exec("bash -c 'command $VAR' && command")
```

:::
