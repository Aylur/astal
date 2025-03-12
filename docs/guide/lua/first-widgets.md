# First Widgets

## Getting Started

Start by importing the singleton
[Astal.Application](https://aylur.github.io/libastal/astal3/class.Application.html) instance.

:::code-group

```lua [init.lua]
local App = require("astal.gtk3.app")

App:start({
    main = function()
        -- you will instantiate Widgets here
        -- and setup anything else if you need
    end
})
```

:::

Then run `lua init.lua` in the terminal, and that's it!
Now you have an Astal instance running written in Lua.

## Root of every shell component: Window

Astal apps are composed of widgets. A widget is a piece of UI that has its own logic and style.
A widget can be as small as a button or an entire bar.
The top level widget is always a [Window](https://aylur.github.io/libastal/astal3/class.Window.html)
which will hold all widgets.

::: code-group

```lua [widget/Bar.lua]
local Widget = require("astal.gtk3.widget")
local Anchor = require("astal.gtk3").Astal.WindowAnchor

return function(monitor)
    return Widget.Window({
        monitor = monitor,
        anchor = Anchor.TOP + Anchor.LEFT + Anchor.RIGHT,
        exclusivity = "EXCLUSIVE",
        Widget.Label({
            label = "Example label content",
        }),
    })
end
```

:::

::: code-group

```lua [init.lua]
local App = require("astal.gtk3.app")
local Bar = require("widget.Bar")

App:start {
    main = function()
        Bar(0)
        Bar(1) -- instantiate for each monitor
    end,
}
```

:::

## Creating and nesting widgets

Widgets are simply Lua functions that return Gtk widgets,
you can nest widgets by passing them as arguments to the table in the function.

:::code-group

```lua [widget/MyButton.lua]
local Widget = require("astal.gtk3.widget")

return function(text)
    return Widget.Button({
        on_click_release = function(_, event)
            if event.button == "PRIMARY" then
                print("Left click")
            elseif event.button == "SECONDARY" then
                print("Right click")
            end
        end,
        Widget.Label({
            label = text,
        }),
    })
end
```

:::

Now, you should be able to nest it into another widgets.

::: code-group

```lua [widget/Bar.lua] {13}
local MyButton = require("widget.MyButton")
local Anchor = require("astal.gtk3").Astal.WindowAnchor

return function(monitor)
    return Widget.Window({
        monitor = monitor,
        anchor = Anchor.TOP + Anchor.LEFT + Anchor.RIGHT,
        exclusivity = "EXCLUSIVE",
        Widget.Box({
            Widget.Label({
                label = "Click the button",
            }),
            MyButton("hi, im a button"),
        }),
    })
end
```

:::

## Widget signal handlers

You can respond to events by declaring event handler functions inside your widget:

```lua
local function MyButton()
    return Widget.Button({
        on_click_release = function(_, event)
            print(event.button)
        end,
    })
end
```

:::info
Keys prefixed with `on_` will connect to a `signal` of the widget.
Refer to the Gtk and Astal docs to have a full list of them.
:::

## State management

The state of widgets are handled with Bindings. A [Binding](./binding) lets you
connect the state of an [object](./binding#subscribable-and-connectable-interface)
to a widget so it re-renders when that state changes.

Use the `bind` function to create a `Binding` object from a `Variable` or
a regular `GObject` and one of its properties.

Here is an example of a Counter widget that uses a `Variable` as its state:

```lua
local astal = require("astal")
local bind = astal.bind
local Variable = astal.Variable
local Widget = require("astal.gtk3.widget")

local function Counter()
    local count = Variable(0)
    return Widget.Box({
        Widget.Label({
            label = bind(count):as(tostring),
        }),
        Widget.Button({
            label = "Click to increment",
            on_click_release = function()
                count:set(count:get() + 1)
            end,
        }),
    })
end
```

:::info
Bindings have an `:as()` method which lets you transform the assigned value.
In the case of a Label, its label property expects a string, so it needs to be
converted into a string first.
:::

:::tip
`Variables` have a shorthand for `bind(variable):as(transform)`

```lua
local v = Variable(0)

return Widget.Box {
    -- these three are equivalent
    Widget.Label({ label = bind(v):as(tostring) }),
    Widget.Label({ label = v():as(tostring) }),
    Widget.Label({ label = v(tostring) }),
}
```

:::

Here is an example of a battery percent label that binds the `percentage`
property of the Battery object from the [Battery Library](/guide/libraries/battery):

```lua
local astal = require("astal")
local bind = astal.bind
local Battery = astal.require("AstalBattery")
local Widget = require("astal.gtk3.widget")

local function BatteryPercentage()
    local bat = Battery.get_default()

    return Widget.Label({
        label = bind(bat, "percentage"):as(function(p)
            return string.format("%.0f%%", p * 100)
        end),
    })
end
```

## Dynamic children

You can also use a `Binding` for `child` and `children` properties.

```lua
local astal = require("astal")
local Variable = astal.Variable
local Widget = require("astal.gtk3.widget")

local child = Variable(Widget.Box())

return Widget.Box({
    child(),
})
```

```lua
local num = Variable(3)

return Widget.Box {
    num():as(function(n)
        local tbl = {}
        for i = 1, n do
            table.insert(tbl, Widget.Button({
                label = tostring(i)
            }))
        end
        return tbl
    end)
}
```

:::tip
Binding children of widgets will implicitly call `:destroy()` on widgets
that would be left without a parent. You can opt out of this behavior
by setting `no_implicity_destroy` property on the container widget.
:::

:::info
You can pass the followings as children:

- widgets
- deeply nested arrays of widgets
- bindings of widgets,
- bindings of deeply nested arrays of widgets

`nil` is the only value that is not rendered and anything not from this list
will be coerced into a string and rendered as a label.
:::
