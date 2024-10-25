# Variable

```js
import { Variable } from "astal"
```

Variable is just a simple object which holds a single value.
It also has some shortcuts for hooking up subprocesses, intervals and other gobjects.

:::info
The `Variable` object imported from the `"astal"` package is **not** [Astal.Variable](https://aylur.github.io/libastal/io/class.Variable.html).
:::

## Example Usage

```typescript
const myvar = Variable("initial-value")

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

:::warning
Make sure to the transform functions you pass to `:as()` are pure.
The `.get()` function can be called anytime by `astal` especially when `deriving`,
so make sure there are no sideeffects.
:::

## Variable Composition

Using `Variable.derive` any `Subscribable` object can be composed.

```typescript
const v1: Variable<number> = Variable(1)
const v2: Binding<number> = bind(obj, "prop")
const v3: Subscribable<number> = {
    get: () => 3,
    subscribe: () => () => {},
}

// first argument is a list of dependencies
// second argument is a transform function,
// where the parameters are the values of the dependencies in the order they were passed
const v4: Variable<number> = Variable.derive(
    [v1, v2, v3],
    (v1: number, v2: number, v3: number) => {
        return v1 * v2 * v3
    }
)
```

:::info
The types are only for demonstration purposes, you do not have to declare
the type of a Variable, they will be inferred from their initial value.
:::

## Subprocess shorthands

Using `.poll` and `.watch` we can start subprocesses and capture their
output. They can poll and watch at the same time, but they
can only poll/watch once.

:::warning
The command parameter is passed to [execAsync](/guide/typescript/utilities#executing-external-commands-and-scripts)
which means they are **not** executed in a shell environment,
they do **not** expand ENV variables like `$HOME`,
and they do **not** handle logical operators like `&&` and `||`.

If you want bash, run them with bash.

```js
Variable("").poll(1000, ["bash", "-c", "command $VAR && command"])
```

:::

```typescript
const myVar = Variable(0)
    .poll(1000, "command", (out: string, prev: number) => parseInt(out))
    .poll(1000, ["bash", "-c", "command"], (out, prev) => parseInt(out))
    .poll(1000, (prev) => prev + 1)
```

```typescript
const myVar = Variable(0)
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

This will stop the interval, force exit the subprocess and disconnect gobjects.

```js
myVar.drop()
```

:::warning
Don't forget to drop derived variables or variables with
either `.poll`, `.watch` or `.observe` when they are defined inside closures.

```tsx
function MyWidget() {
    const myvar = Variable().poll()
    const derived = Variable.derive()

    return <box
        onDestroy={() => {
            myvar.drop()
            derived.drop()
        }}
    />
}
```

:::
