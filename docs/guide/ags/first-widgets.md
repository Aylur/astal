# First Widgets

AGS is the predecessor of Astal, which was written purely in TypeScript and so only supported
JavaScript/TypeScript. Now it serves as a scaffolding tool for Astal projects in TypeScript.
While what made AGS what it is, is now part of the Astal project, for simplicity we will
refer to the Astal TypeScript lib as AGS.

:::tip
If you are not familiar with the JavaScript syntax [MDN](https://developer.mozilla.org/en-US/)
and [javascript.info](https://javascript.info/) have great references.
:::

## Getting Started

Start by initializing a project

```sh
ags --init
```

then run `ags` in the terminal

```sh
ags
```

Done! You have now a custom written bar using Gtk.

:::tip
AGS will transpile every `.ts`, `.jsx` and `.tsx` files into regular javascript then
it will bundle everything into a single javascript file which then GJS can execute.
The bundler used is [esbuild](https://esbuild.github.io/).
:::

## Root of every shell component: Window

Astal apps are composed of widgets. A widget is a piece of UI that has its own logic and style.
A widget can be as small as a button or an entire bar.
The top level widget is always a [Window](https://aylur.github.io/libastal/class.Window.html) which will hold all widgets.

::: code-group

```tsx [widget/Bar.tsx]
function Bar(monitor = 0) {
    return <window className="Bar" monitor={monitor}>
        <box>Content of the widget</box>
    </window>
}
```

:::

::: code-group

```ts [app.ts]
import Bar from "./widget/Bar"

App.start({
    main() {
        Bar(0)
        Bar(1) // instantiate for each monitor
    },
})
```

:::

## Creating and nesting widgets

Widgets are JavaScript functions which return Gtk widgets,
either by using JSX or using a widget constructor.

:::code-group

```tsx [MyButton.tsx]
function MyButton(): JSX.Element {
    return <button onClicked="echo hello">
        Clicke Me!
    </button>
}
```

```ts [MyButton.ts]
import { Widget } from "astal"

function MyButton(): Widget.Button {
    return Widget.Button({
        onClicked: "echo hello",
        label: "Click Me!",
    })
}
```

:::

:::info
The only difference between the two is the return type.
Using markup the return type is always `Gtk.Widget` (globally available as `JSX.Element`),
while using constructors the return type is the type of the widget.
It is rare to need the actual return type, so most if not all of the time, you can use markup.
:::

Now that you have declared `MyButton`, you can nest it into another component.

```tsx
function MyBar() {
    return <window>
        <box>
            Click The button
            <MyButton />
        </box>
    </window>
}
```

Notice that widgets you defined start with a capital letter `<MyButton />`.
Lowercase tags are builtin widgets, while capital letter is for custom widgets.

## Displaying Data

JSX lets you put markup into JavaScript.
Curly braces let you “escape back” into JavaScript so that you can embed some variable
from your code and display it.

```tsx
function MyWidget() {
    const label = "hello"

    return <button>{label}</button>
}
```

You can also pass JavaScript to markup attributes

```tsx
function MyWidget() {
    const label = "hello"

    return <button label={label} />
}
```

## Conditional Rendering

You can use the same techniques as you use when writing regular JavaScript code.
For example, you can use an if statement to conditionally include JSX:

```tsx
function MyWidget() {
    let content

    if (condition) {
        content = <True />
    } else {
        content = <False />
    }

    return <box>{content}</box>
}
```

You can also inline a [conditional `?`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Conditional_operator) (ternary) expression.

```tsx
function MyWidget() {
    return <box>{condition ? <True /> : <False />}</box>
}
```

When you don’t need the `else` branch, you can also use a shorter [logical && syntax](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Logical_AND#short-circuit_evaluation):

```tsx
function MyWidget() {
    return <box>{condition && <True />}</box>
}
```

:::info
As you can guess from the above snippet, [falsy](https://developer.mozilla.org/en-US/docs/Glossary/Falsy) values are not rendered.
:::

## Rendering lists

You can use [`for` loops](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/for) or [array `map()` function](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map).

```tsx
function MyWidget() {
    const labels = [
        "label1"
        "label2"
        "label3"
    ]

    return <box>
        {labels.map(label => (
            <label label={label} />
        ))}
    </box>
}
```

## Widget signal handlers

You can respond to events by declaring event handler functions inside your widget:

```tsx
function MyButton() {
    function onClicked(self: Widget.Button, ...args) {
        console.log(self, "was clicked")
    }

    return <button onClicked={onClicked} />
}
```

The handler can also be a string, which will get executed in a subprocess asynchronously.

```tsx
function MyButton() {
    return <button onClicked="echo hello" />
}
```

:::info
Attributes prefixed with `on` will connect to a `signal` of the widget.
Their types are not generated, but written by hand, which means not all of them are typed.
Refer to the Gtk and Astal docs to have a full list of them.
:::

## How properties are passed

Using JSX, a custom widget will always have a single object as its parameter.

```ts
type Props = {
    myprop: string
    child?: JSX.Element // when only one child is passed
    children?: Array<JSX.Element> // when multiple children are passed
}

function MyWidget({ myprop, child, children }: Props) {
    //
}
```

```tsx
// child prop of MyWidget is the box
return <MyWidget myprop="hello">
    <box />
</MyWidget>
```

```tsx
// children prop of MyWidget is [box, box, box]
return <MyWidget myprop="hello">
    <box />
    <box />
    <box />
</MyWidget>
```

## State management

The state of widgets are handled with Bindings. A `Binding` lets you
connect the state of one [GObject](https://docs.gtk.org/gobject/class.Object.html) to another, in our case it is used to
rerender part of a widget based on the state of a `GObject`.
A `GObject` can be a [Variable](./variable) or it can be from a [Library](../libraries/references).

We use the `bind` function to create a `Binding` object from a `Variable` or
a regular GObject and one of its properties.

Here is an example of a Counter widget that uses a `Variable` as its state:

```tsx
import { Variable, bind } from "astal"

function Counter() {
    const count = Variable(0)

    function increment() {
        count.set(count.get() + 1)
    }

    return <box>
        <label label={bind(count).as(num => num.toString())} />
        <button onClicked={increment}>
            Click to increment
        <button>
    </box>
}
```

:::info
Bindings have an `.as()` method which lets you transform the assigned value.
In the case of a Label, its label property expects a string, so it needs to be
turned to a string first.
:::

:::tip
`Variables` have a shorthand for `bind(variable).as(transform)`

```tsx
const v = Variable(0)
const transform = (v) => v.toString()

return <box>
    {/* these two are equivalent */}
    <label label={bind(v).as(transform)} />
    <label label={v(transform)} />
</box>
```

:::

Here is an example of a battery percent label that binds the `percentage`
property of the Battery object from the [Battery Library](/guide/libraries/battery):

```tsx
import Battery from "gi://AstalBattery"
import { bind } from "astal"

function BatteryPercentage() {
    const bat = Battery.get_default()

    return <label label={bind(bat, "percentage").as((p) => p * 100 + " %")} />
}
```

## Dynamic children

You can also use a `Binding` for `child` and `children` properties.

```tsx
const child = Variable(<box />)

return <box>{child}</box>
```

```tsx
const num = Variable(3)
const range = (n) => [...Array(n).keys()]

return <box>
    {num(n => range(n).map(i => (
        <button>
            {i.toString()}
        <button/>
    )))}
<box>
```

:::warning
Only bind children of the `box` or the `stack` widget. Gtk does not cleanup widgets by default,
they have to be explicitly destroyed. The box widget is a special container that
will implicitly call `.destroy()` on its removed child widgets.
You can disable this behavior by setting the `noImplicityDestroy` property.
:::

:::info
The above example destroys and recreates every widget in the list everytime
the value of the `Variable` changes. There might be cases where you would
want to handle child creation yourself, because you don't want to lose the
inner state of widgets that does not need to be recreated.
:::

When there is at least one `Binding` passed as a child, the `children`
parameter will always be a flattened `Binding<Array<JSX.Element>>`

```tsx
function MyContainer({ children }) {
    // children is a Binding over an Array of widgets
}

return <MyContainer>
    <box />
    {num(n => range(n).map(i => (
        <button>
            {i.toString()}
        <button/>
    )))}
    [
        [
            <button />
        ]
        <button />
    ]
</MyContainer>
```

:::info
You can pass the followings as children:

- widgets
- deeply nested arrays of widgets
- bindings of widgets,
- bindings of deeply nested arrays of widgets

[falsy](https://developer.mozilla.org/en-US/docs/Glossary/Falsy) values are not rendered and anything not from this list
will be coerced into a string and rendered as a label
:::
