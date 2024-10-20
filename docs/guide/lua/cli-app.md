# CLI and App

`App` is a singleton **instance** of an [Astal.Application](https://aylur.github.io/libastal/astal3/class.Application.html).

Depending on gtk version require paths will differ

```lua
local App = require("astal.gtk3.app")

local App = require("astal.gtk4.app") -- not available atm thought
```

## Entry point

:::code-group

```lua [init.lua]
App:start({
  main = function()
    -- setup anything
    -- instantiate widgets
  end
})
```

:::

## Instance identifier

You can run multiple instance by defining a unique instance name.

```lua
App:start({
  instance_name = "my-instance", -- defaults to "astal"
  main = function() end
})
```

## Messaging from CLI

If you want to interact with an instance from the CLI,
you can do so by sending a message.

```lua
App:start({
  main = function() end,
  ---@param request string
  ---@param res fun(response: any): nil
  request_handler = function(request, res)
    if request == "say hi" then
      res("hi cli")
    end
    res("unknown command")
  end
})
```

```sh
astal say hi
# hi cli
```

## Toggling Windows by their name

In order for Astal to know about your windows, you have to register them.
You can do this by specifying a **unique** `name` and calling `App:add_window`.

```lua
local App = require("astal.gtk3.app")

local function Bar()
  return Widget.Window({
    name = "Bar",
    setup = function(self)
      App:add_window(self)
    end,
    Widget.Box()
  })
end
```

You can also invoke `App:add_window` by simply passing the `App` to the `application` prop.

```lua
local App = require("astal.gtk3.app")

local function Bar()
  return Widget.Window({
    name = "Bar",
    application = App,
    Widget.Box()
  })
end
```

```sh
astal -t Bar
```

:::warning
When assigning the `application` prop make sure `name` comes before.
Props are set sequentially and if name is applied after application it won't work.
:::

## Bundled scripts

The produced scripts when bundling can run as the main instance
and a "client" instance.

The first time you execute your bundled script the `main` function gets called.
While that instance is running any subsequent execution of the script will call
the `client` function.

:::code-group

```lua [init.lua]
App:start({
  -- main instance
  main = function(...)
    local args = { ... }
    print(string.format("{%s}", table.concat(args, ", ")))
  end,
  client = function(message, ...)
    local res = message("you can message the main instance")
    print(res)
  end,
  -- this runs in the main instance
  request_handler = function(request, res)
    res("response from main")
  end
})
```

:::
