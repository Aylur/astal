# Subclassing GObject.Object

By default, Astal doesn't provide utilities to easily subclass GObject.Object.

## Example

```lua
local GObject = astal.require("GObject")
local MyClass = GObject.Object:derive("MyClass")
```

## Define properties

```lua
local MyClass = GObject.Object:derive("MyClass")

MyClass._property.my_prop =
    GObject.ParamSpecString('my_prop', 'my_prop', 'my_prop', '', { 'READ', 'WRITE' })

```

The `read`, `write` flags will create a getter and setter for the property and will also
emit the notify signal when the value is set to a new value.

You can use flags to define the behaviour of your property, for example a `read-only` property can be defined as

```lua
MyClass._property.my_prop =
    GObject.ParamSpecString('my_prop', 'my_prop', 'my_prop', '', { 'READ' })
```

:::info
You can find the available flags [here](https://lazka.github.io/pgi-docs/GObject-2.0/flags.html#GObject.ParamFlags)
to specify a particular behavior.
:::

If you want to set a custom default value, do so in the constructor of your class.

```lua
local MyClass = GObject.Object:derive("MyClass")

MyClass._property.my_prop =
    GObject.ParamSpecString('my_prop', 'my_prop', 'my_prop', '', { 'READWRITE' })

MyClass._init = function(self, args)
    if not args then
        args = {}
    end

    args.my_prop = args.my_prop or "default-value"

    for key, value in pairs(args) do
        self[key] = value
    end
end
```

2. Custom getter

```lua
MyClass._attribute.my_prop.get = function(self)
    return "value"
end
```

3. Getter and setter

```lua
MyClass._attribute.my_prop = {
    get = function(self)
        return "value"
    end,
    set = function(self, new_value)
        if new_value ~= self.my_prop then
            self.priv.my_prop = tostring(new_value) -- since it's a string prop
            self:notify("my-prop")
        end
    end
}
```

This will create a read-write property.

:::info
When defining getter/setters for the property, notify signal emission has to be done explicitly.
:::

## Define signals

Wip.
