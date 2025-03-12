# Binding

As mentioned before binding an object's state to another -
so in most cases a `Variable` or a `GObject.Object` property to a widget's property -
is done through the `bind` function which returns a `Binding` object.

`Binding` objects simply hold information about the source and how it should be transformed
which Widget constructors can use to setup a connection between themselves and the source.

```lua
---@class Binding<T>
---@field private transform_fn fun(value: T): any
---@field private emitter Connectable | Subscribable<T>
---@field private property? string
---@field as fun(transform: fun(value: T): any): Binding
---@field get fun(): T
---@field subscribe fun(self, callback: fun(value: T)): function
```

A `Binding` can be constructed from an object implementing
the `Subscribable` interface (usually a `Variable`)
or an object implementing the `Connectable` interface and one of its properties
(usually a `GObject.Object` instance).

Lua type annotations are not expressive enough to explain this,
so I'll use TypeScript to demonstrate it.

<!--TODO: use Teal maybe?-->

```ts
function bind<T>(obj: Subscribable<T>): Binding<T>

function bind<
    Obj extends Connectable,
    Prop extends keyof Obj,
>(obj: Obj, prop: Prop): Binding<Obj[Prop]>
```

## Subscribable and Connectable interface

Any object implementing one of these interfaces can be used with `bind`.

```ts
interface Subscribable<T> {
    subscribe(callback: (value: T) => void): () => void
    get(): T
}

interface Connectable {
    connect(signal: string, callback: (...args: any[]) => unknown): number
    disconnect(id: number): void
}
```

`Connectable` is usually used for GObjects coming from [libraries](../libraries/references)
You won't be implementing it in Lua code.

## Example Custom Subscribable

When binding the children of a box from an array, usually not all elements
of the array changes each time, so it would make sense to not destroy
the widget which represents the element.

::: code-group

```lua :line-numbers [varmap.lua]
local Gtk = require("astal.gtk3").Gtk
local Variable = require("astal.variable")

---@param initial table
---@return varmap
return function(initial)
    local map = initial
    local var = Variable.new({})

    local function notify()
        local arr = {}
        for _, value in pairs(map) do
            table.insert(arr, value)
        end
        var:set(arr)
    end

    local function delete(key)
        if Gtk.Widget:is_type_of(map[key]) then
            map[key]:destroy()
        end

        map[key] = nil
    end

    notify() -- init

    ---@class varmap
    ---@field set fun(key: any, value: any): nil
    ---@field delete fun(key: any): nil
    ---@field get fun(): any
    ---@field subscribe fun(callback: function): function
    ---@overload fun(): Binding
    return setmetatable({
        set = function(key, value)
            delete(key)
            map[key] = value
            notify()
        end,
        delete = function(key)
            delete(key)
            notify()
        end,
        get = function()
            return var:get()
        end,
        subscribe = function(callback)
            return var:subscribe(callback)
        end,
    }, {
        __call = function()
            return var()
        end,
    })
end
```

:::

And this `VarMap<key, Widget>` can be used as an alternative to `Variable<Array<Widget>>`.

```lua
function MappedBox()
    local map = varmap({
        ["1"] = Widget.Label({ label = "1" }),
        ["2"] = Widget.Label({ label = "2" }),
    })

    return Widget.Box({
        setup = function (self)
            self:hook(gobject, "added", function (_, id)
                map.set(id, Widget.Label({ label = id }))
            end)
            self:hook(gobject, "removed", function (_, id)
                map.delete(id)
            end)
        end,
        map():as(function(arr)
            -- can be sorted here
            return arr
        end),
    })
end
```
