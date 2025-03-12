local lgi = require("lgi")
local Astal = lgi.require("Astal", "3.0")
local Gtk = lgi.require("Gtk", "3.0")
local GObject = lgi.require("GObject", "2.0")
local Binding = require("astal.binding")
local Variable = require("astal.variable")
local exec_async = require("astal.process").exec_async

local function filter(tbl, fn)
    local copy = {}
    for key, value in pairs(tbl) do
        if fn(value, key) then
            if type(key) == "number" then
                table.insert(copy, value)
            else
                copy[key] = value
            end
        end
    end
    return copy
end

local function map(tbl, fn)
    local copy = {}
    for key, value in pairs(tbl) do
        copy[key] = fn(value)
    end
    return copy
end

local function flatten(tbl)
    local copy = {}
    for _, value in pairs(tbl) do
        if type(value) == "table" and getmetatable(value) == nil then
            for _, inner in pairs(flatten(value)) do
                table.insert(copy, inner)
            end
        else
            table.insert(copy, value)
        end
    end
    return copy
end

local function includes(tbl, elem)
    for _, value in pairs(tbl) do
        if value == elem then
            return true
        end
    end
    return false
end

local function set_children(parent, children)
    children = map(
        filter(flatten(children), function(item)
            return not not item
        end),
        function(item)
            if Gtk.Widget:is_type_of(item) then
                return item
            end
            return Gtk.Label({
                visible = true,
                label = tostring(item),
            })
        end
    )

    -- remove
    if Gtk.Bin:is_type_of(parent) then
        local ch = parent:get_child()
        if ch ~= nil then
            parent:remove(ch)
        end
        if ch ~= nil and not includes(children, ch) and not parent.no_implicit_destroy then
            ch:destroy()
        end
    elseif Gtk.Container:is_type_of(parent) then
        for _, ch in ipairs(parent:get_children()) do
            parent:remove(ch)
            if ch ~= nil and not includes(children, ch) and not parent.no_implicit_destroy then
                ch:destroy()
            end
        end
    end

    -- TODO: add more container types
    if Astal.Box:is_type_of(parent) then
        parent:set_children(children)
    elseif Astal.Stack:is_type_of(parent) then
        parent:set_children(children)
    elseif Astal.CenterBox:is_type_of(parent) then
        parent.start_widget = children[1]
        parent.center_widget = children[2]
        parent.end_widget = children[3]
    elseif Astal.Overlay:is_type_of(parent) then
        parent:set_child(children[1])
        children[1] = nil
        parent:set_overlays(children)
    elseif Gtk.Container:is_type_of(parent) then
        for _, child in pairs(children) do
            if Gtk.Widget:is_type_of(child) then
                parent:add(child)
            end
        end
    end
end

local function merge_bindings(array)
    local function get_values(...)
        local args = { ... }
        local i = 0
        return map(array, function(value)
            if getmetatable(value) == Binding then
                i = i + 1
                return args[i]
            else
                return value
            end
        end)
    end

    local bindings = filter(array, function(v)
        return getmetatable(v) == Binding
    end)

    if #bindings == 0 then
        return array
    end

    if #bindings == 1 then
        return bindings[1]:as(get_values)
    end

    return Variable.derive(bindings, get_values)()
end

return function(ctor)
    function ctor:hook(object, signalOrCallback, callback)
        if GObject.Object:is_type_of(object) and type(signalOrCallback) == "string" then
            local id
            if string.sub(signalOrCallback, 1, 8) == "notify::" then
                local prop = string.gsub(signalOrCallback, "notify::", "")
                id = object.on_notify:connect(function()
                    callback(self, object[prop])
                end, prop, false)
            else
                id = object["on_" .. signalOrCallback]:connect(function(_, ...)
                    callback(self, ...)
                end)
            end
            self.on_destroy = function()
                GObject.signal_handler_disconnect(object, id)
            end
        elseif type(object.subscribe) == "function" then
            local unsub = object.subscribe(function(...)
                signalOrCallback(self, ...)
            end)
            self.on_destroy = unsub
        else
            error("can not hook: not gobject+signal or subscribable")
        end
    end

    function ctor:toggle_class_name(name, on)
        Astal.widget_toggle_class_name(self, name, on)
    end

    return function(tbl)
        tbl = tbl or {}

        local bindings = {}
        local setup = tbl.setup

        -- collect children
        local children = merge_bindings(flatten(filter(tbl, function(_, key)
            return type(key) == "number"
        end)))

        -- default visible to true
        if tbl.visible == nil then
            tbl.visible = true
        end

        -- collect props
        local props = filter(tbl, function(_, key)
            return type(key) == "string" and key ~= "setup"
        end)

        -- collect signal handlers
        for prop, value in pairs(props) do
            if string.sub(prop, 0, 2) == "on" and type(value) ~= "function" then
                props[prop] = function()
                    exec_async(value, print)
                end
            end
        end

        -- collect bindings
        for prop, value in pairs(props) do
            if getmetatable(value) == Binding then
                bindings[prop] = value
                props[prop] = value:get()
            end
        end

        -- construct, attach bindings, add children
        local widget = ctor()

        if getmetatable(children) == Binding then
            set_children(widget, children:get())
            widget.on_destroy = children:subscribe(function(v)
                set_children(widget, v)
            end)
        else
            if #children > 0 then
                set_children(widget, children)
            end
        end

        for prop, binding in pairs(bindings) do
            widget.on_destroy = binding:subscribe(function(v)
                widget[prop] = v
            end)
        end

        for prop, value in pairs(props) do
            widget[prop] = value
        end

        if type(setup) == "function" then
            setup(widget)
        end

        return widget
    end
end
