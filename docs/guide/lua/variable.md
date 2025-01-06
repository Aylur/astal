# Variable

```lua
local Variable = require("astal").Variable
local Variable = require("astal.variable")
```

Variable is just a simple object which holds a single value.
It also has some shortcuts for hooking up subprocesses, intervals and other gobjects.

## Example Usage

```lua
local my_var = Variable("initial-value")

-- whenever its value changes, callback will be executed
my_var:subscribe(function(value)
    print(value)
end)

-- settings its value
my_var:set("new value")

-- getting its value
local value = my_var:get()

-- binding them to widgets
Widget.Label({
    label = bind(my_var):as(function(value)
        return string.format("transformed %s", value)
    end),
    -- shorthand for the above
    label = my_var(function(value)
        return string.format("transformed %s", value)
    end)
})
```

:::warning
Make sure to the transform functions you pass to `:as()` are pure.
The `:get()` function can be called anytime by `astal` especially when `deriving`,
so make sure there are no sideeffects.
:::

## Variable Composition

Using `Variable.derive` any `Subscribable` object can be composed.

```lua
local v1 = Variable(1) -- Variable
local v2 = bind(obj, "prop") -- Binding
local v3 = { -- Subscribable
    get = function()
        return 3
    end,
    subscribe = function()
        return function() end
    end,
}

-- first argument is a list of dependencies
-- second argument is a transform function,
-- where the parameters are the values of the dependencies in the order they were passed
local v4 = Variable.derive({ v1, v2, v3 }, function(v1, v2, v3)
    return v1 * v2 * v3
end)
```

## Subprocess shorthands

Using `:poll` and `:watch` you can start subprocesses and capture their
output. They can poll and watch at the same time, but they
can only poll/watch once.

:::warning
The command parameter is passed to [exec_async](/guide/lua/utilities#executing-external-commands-and-scripts)
which means they are **not** executed in a shell environment,
they do **not** expand ENV variables like `$HOME`,
and they do **not** handle logical operators like `&&` and `||`.

If you want bash, run them with bash.

```lua
Variable(""):poll(1000, { "bash", "-c", "command $VAR && command" })
```

:::

```lua
local my_var = Variable(0)
    :poll(1000, "command", function(out, prev)
        return tonumber(out)
    end)
    :poll(1000, { "bash", "-c", "command" }, function(out, prev)
        return tonumber(out)
    end)
    :poll(1000, function(prev)
        return prev + 1
    end)
```

```lua
local my_var = Variable(0)
    :watch("command", function(out, prev)
        return tonumber(out)
    end)
    :watch({ "bash", "-c", "command" }, function(out, prev)
        return tonumber(out)
    end)
```

You can temporarily stop them and restart them whenever.

```lua
my_var:stop_watch() -- this kills the subprocess
my_var:stop_poll()

my_var:start_listen() -- launches the subprocess again
my_var:start_poll()

print(my_var:is_listening())
print(my_var:is_polling())
```

## Gobject connection shorthands

Using `:observe` you can connect gobject signals and capture their value.

```lua
local my_var = Variable("")
    :observe(obj1, "signal", function()
        return ""
    end):observe(obj2, "signal", function()
        return ""
    end)
```

## Dispose if no longer needed

This will stop the interval, force exit the subprocess and disconnect gobjects.

```lua
my_var:drop()
```

:::warning
Don't forget to drop derived variables or variables with
either `:poll`, `:watch` or `:observe` when they are defined inside closures.

```lua
local function MyWidget()
    local my_var = Variable():poll()

    return Widget.Box({
        on_destroy = function()
            my_var:drop()
        end
    })
end
```

:::
