#!/usr/bin/env lua
-- imports
local astal = require("astal.init")
local Widget, Variable, App, bind = astal.Widget, astal.Variable, astal.App, astal.bind

-- state
local player = astal.require("Playerctl").Player.new("spotify")

local title = Variable(player:get_title()):observe(player, "metadata", function()
    return player:get_title()
end)

local rnd = Variable(1):poll(1000, function()
    return math.random(1, 10)
end)

-- ui
local Bar = function(monitor)
    return Widget.Window({
        application = App,
        id = "bar",
        name = "bar",
        monitor = monitor,
        anchor = astal.Astal.WindowAnchor.BOTTOM
            + astal.Astal.WindowAnchor.LEFT
            + astal.Astal.WindowAnchor.RIGHT,
        exclusivity = "EXCLUSIVE",

        Widget.CenterBox({
            class_name = "bar",
            start_widget = Widget.Label({
                valign = "CENTER",
                label = "Welcome to Astal.lua",
            }),
            center_widget = Widget.Box({
                children = bind(rnd):as(function(n)
                    local children = {}
                    for i = 1, n, 1 do
                        table.insert(
                            children,
                            Widget.Button({
                                label = tostring(i),
                                on_clicked = function()
                                    print(i)
                                end,
                            })
                        )
                    end
                    return children
                end),
            }),
            end_widget = Widget.Label({
                valign = "CENTER",
                label = bind(title),
            }),
        }),
    })
end

-- css
local css = [[
.bar button {
    color: blue;
}
]]

-- main
App:start({
    request_handler = function(msg, res)
        if msg == "quit" then
            os.exit(0)
        end
        if msg == "inspector" then
            res(App:inspector())
        end
        res("hi")
    end,
    css = css,
}, function()
    Bar(0)
end)
