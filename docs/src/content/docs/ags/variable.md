---
title: Variable
description: Reference of the builtin Variable type
sidebar:
    order: 6
---

```js
import { Variable } from "astal"
```

Variable is just a simple `GObject` that holds a value.
And has shortcuts for hooking up subprocesses.

## Variable as state

```typescript
const myvar = Variable<string>("initial-value")

// whenever its value changes, callback will be executed
myvar.subscribe((value: string) => {
    console.log(value)
})

// settings its value
myvar.set("new value")

// getting its value
const value = myvar.get()

// binding them to widgets
Widget.Label({
    label: bind(myvar).as((value) => `transformed ${value}`),
    label: myvar((value) => `transformed ${value}`), // shorthand for the above
})
```

:::caution
Make sure to make the transform functions pure. The `.get()` function can be called
anytime by `astal` especially when `deriving`, so make sure there are no sideeffects.
:::

## Composing variables

Using `Variable.derive` we can compose both Variables and Bindings.

```typescript
const v1: Variable<number> = Variable(2)
const v2: Variable<number> = Variable(3)

// first argument is a list of dependencies
// second argument is a transform function,
// where the parameters are the values of the dependencies in the order they were passed
const v3: Variable<number> = Variable.derive([v1, v2], (v1, v2) => {
    return v1 * v2
})

const b1: Binding<string> = bind(obj, "prop")
const b2: Binding<string> = bind(obj, "prop")

const b3: Variable<string> = Variable.derive([b1, b2], (b1, b2) => {
    return `${b1}-${b2}`
})
```

## Subprocess shorthands

Using `.poll` and `.watch` we can start subprocess and capture their
output in `Variables`. They can poll and watch at the same time, but they
can only poll/watch one subprocess.

:::caution
The command parameter is passed to [execAsync](/astal/ags/utilities/#executing-external-commands-and-scripts)
which means they are **not** executed in a shell environment,
they do **not** expand env variables like `$HOME`,
and they do **not** handle logical operators like `&&` and `||`.

If you want bash, run them with bash.

```js
Variable("").poll(1000, ["bash", "-c", "command $VAR && command"])
```

:::

```typescript
const myVar = Variable<number>(0)
    .poll(1000, "command", (out: string, prev: number) => parseInt(out))
    .poll(1000, ["bash", "-c", "command"], (out, prev) => parseInt(out))
    .poll(1000, (prev) => prev + 1)
```

```typescript
const myVar = Variable<number>(0)
    .watch("command", (out: string, prev: number) => parseInt(out))
    .watch(["bash", "-c", "command"], (out, prev) => parseInt(out))
```

You can temporarily stop them and restart them whenever.

```js
myvar.stopWatch() // this kills the subprocess
myvar.stopPoll()

myvar.startListen() // launches the subprocess again
myvar.startPoll()

console.log(myvar.isListening())
console.log(myvar.isPolling())
```

## Gobject connection shorthands

Using `.observe` you can connect gobject signals and capture their value.

```typescript
const myvar = Variable("")
    .observe(obj1, "signal", () => "")
    .observe(obj2, "signal", () => "")
```

## Dispose if no longer needed

This will stop the interval and force exit the subprocess and disconnect gobjects.

```js
myVar.drop()
```

:::caution
Don't forget to drop them when they are defined inside widgets
with either `.poll`, `.watch` or `.observe`

```tsx
function MyWidget() {
    const myvar = Variable().poll()

    return <box onDestroy={() => myvar.drop()} />
}
```

:::
