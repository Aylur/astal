# CLI and App

`App` is a singleton **instance** of an [Astal.Application](https://aylur.github.io/libastal/astal3/class.Application.html).

Depending on gtk version require paths will differ

<!--TODO: remove gtk4 notice when its available-->

```lua
local App = require("astal.gtk3.app")

local App = require("astal.gtk4.app") -- not yet available
```

## Entry point

`App:start` is a wrapper function for `App:run`
that will only run the application if there is no
instance already running with the specified name.

:::code-group

```lua [init.lua]
App:start({
    instance_name = "my-instance", -- defaults to "astal"
    main = function()
        -- setup anything
        -- instantiate widgets
    end,
})
```

:::

## Messaging from CLI

If you want to interact with an instance from the CLI,
you can do so by sending a request.

```lua
App:start({
    main = function() end,

    ---@param request string
    ---@param res fun(response: any): nil
    request_handler = function(request, res)
        if request == "say hi" then
            return res("hi cli")
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

## Client instances

The first time `App:start` is invoked the `main` function gets called.
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

    -- client instance
    client = function(request, ...)
        local res = request("you can send a request to the main instance")
        print(res)
    end,

    -- this runs in the main instance
    request_handler = function(request, res)
        res("response from main instance")
    end
})
```

:::
