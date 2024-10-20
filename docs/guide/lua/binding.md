# Binding

As mentioned before binding an object's state to another -
so in most cases a `Variable` or a `GObject.Object` property to a widget's property -
is done through the `bind` function which returns a `Binding` object.

`Binding` objects simply hold information about the source and how it should be transformed
which Widget constructors can use to setup a connection between themselves and the source.

```lua
---@class Binding
---@field transformFn function
---@field emitter table | Variable
---@field property? string
---@field as fun(self, transform: fun(value: any): any): Binding
---@field get fun(self): any
---@field subscribe fun(self, callback: fun(value: any)): function
local Binding = {}
```

# ...

🚧 Lua documentation is in Progress 🚧
