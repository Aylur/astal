local lgi = require("lgi")
local Astal = lgi.require("Astal", "0.1")
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

local flatten
flatten = function(tbl)
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

local function set_children(parent, children)
    children = map(flatten(children), function(item)
        if Gtk.Widget:is_type_of(item) then
            return item
        end
        return Gtk.Label({
            visible = true,
            label = tostring(item),
        })
    end)

    -- remove
    if Gtk.Bin:is_type_of(parent) then
        local rm = parent:get_child()
        if rm ~= nil then
            parent:remove(rm)
        end
    end

    -- FIXME: add rest of the edge cases like Stack
    if Astal.Box:is_type_of(parent) then
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

local function astalify(ctor)
    function ctor:hook(object, signalOrCallback, callback)
        if type(object.subscribe) == "function" then
            local unsub = object.subscribe(function(...)
                signalOrCallback(self, ...)
            end)
            self.on_destroy = unsub
            return
        end
        local id = object["on_" .. signalOrCallback](function(_, ...)
            callback(self, ...)
        end)
        self.on_destroy = function()
            GObject.signal_handler_disconnect(object, id)
        end
    end

    function ctor:toggle_class_name(name, on)
        Astal.toggle_class_name(self, name, on)
    end

    return function(tbl)
        if tbl == nil then
            tbl = {}
        end

        local bindings = {}
        local setup = tbl.setup

        -- collect children
        local children = merge_bindings(flatten(filter(tbl, function(_, key)
            return type(key) == "number"
        end)))

        -- default visible to true
        if type(tbl.visible) ~= "boolean" then
            tbl.visible = true
        end

        -- filter props
        local props = filter(tbl, function(_, key)
            return type(key) == "string" and key ~= "setup"
        end)

        -- handle on_ handlers that are strings
        for prop, value in pairs(props) do
            if string.sub(prop, 0, 2) == "on" and type(value) ~= "function" then
                props[prop] = function()
                    exec_async(value, print, print)
                end
            end
        end

        -- handle bindings
        for prop, value in pairs(props) do
            if getmetatable(value) == Binding then
                bindings[prop] = value
                props[prop] = value:get()
            end
        end

        -- construct, attach bindings, add children
        local widget = ctor()

        for prop, value in pairs(props) do
            widget[prop] = value
        end

        for prop, binding in pairs(bindings) do
            widget.on_destroy = binding:subscribe(function(v)
                widget[prop] = v
            end)
        end

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

        if type(setup) == "function" then
            setup(widget)
        end

        return widget
    end
end

local Widget = {
    astalify = astalify,
    Box = astalify(Astal.Box),
    Button = astalify(Astal.Button),
    CenterBox = astalify(Astal.CenterBox),
    CircularProgress = astalify(Astal.CircularProgress),
    DrawingArea = astalify(Gtk.DrawingArea),
    Entry = astalify(Gtk.Entry),
    EventBox = astalify(Astal.EventBox),
    -- TODO: Fixed
    -- TODO: FlowBox
    Icon = astalify(Astal.Icon),
    Label = astalify(Gtk.Label),
    LevelBar = astalify(Astal.LevelBar),
    -- TODO: ListBox
    Overlay = astalify(Astal.Overlay),
    Revealer = astalify(Gtk.Revealer),
    Scrollable = astalify(Astal.Scrollable),
    Slider = astalify(Astal.Slider),
    -- TODO: Stack
    Switch = astalify(Gtk.Switch),
    Window = astalify(Astal.Window),
}

Gtk.Widget._attribute.css = {
    get = Astal.widget_get_css,
    set = Astal.widget_set_css,
}

Gtk.Widget._attribute.class_name = {
    get = function(self)
        local result = ""
        local strings = Astal.widget_set_class_names(self)
        for i, str in ipairs(strings) do
            result = result .. str
            if i < #strings then
                result = result .. " "
            end
        end
        return result
    end,
    set = function(self, class_name)
        local names = {}
        for word in class_name:gmatch("%S+") do
            table.insert(names, word)
        end
        Astal.widget_set_class_names(self, names)
    end,
}

Gtk.Widget._attribute.cursor = {
    get = Astal.widget_get_cursor,
    set = Astal.widget_set_cursor,
}

Gtk.Widget._attribute.click_through = {
    get = Astal.widget_get_click_through,
    set = Astal.widget_set_click_through,
}

Astal.Box._attribute.children = {
    get = Astal.Box.get_children,
    set = Astal.Box.set_children,
}

return setmetatable(Widget, {
    __call = function(_, ctor)
        return astalify(ctor)
    end,
})
