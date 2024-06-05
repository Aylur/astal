local lgi = require("lgi")
local Astal = lgi.require("Astal", "0.1")
local Gtk = lgi.require("Gtk", "3.0")
local GObject = lgi.require("GObject", "2.0")
local Binding = require("astal.binding")
local exec_async = require("astal.process").exec_async

local function filter(tbl, fn)
    local copy = {}
    for key, value in pairs(tbl) do
        if fn(value, key) then
            copy[key] = value
        end
    end
    return copy
end

local function set_child(parent, child)
    if parent.get_child ~= nil then
        local rm = parent:get_child()
        if rm ~= nil then
            parent:remove(rm)
        end
    end
    if parent.add ~= nil then
        parent:add(child)
    end
end

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

Astal.Box._attribute.children = {
    get = Astal.Box.get_children,
    set = Astal.Box.set_children,
}

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

    return function(tbl)
        local bindings = {}
        local setup = tbl.setup

        local visible
        if type(tbl.visible) == "boolean" then
            visible = tbl.visible
        else
            visible = true
        end

        local props = filter(tbl, function(_, key)
            return key ~= "visible" and key ~= "setup"
        end)

        for prop, value in pairs(props) do
            if getmetatable(value) == Binding then
                bindings[prop] = value
                props[prop] = value:get()
            end
        end

        for prop, value in pairs(props) do
            if string.sub(prop, 0, 2) == "on" and type(value) ~= "function" then
                props[prop] = function()
                    exec_async(value, print, print)
                end
            end
        end

        local widget = ctor(props)

        for prop, binding in pairs(bindings) do
            if prop == "child" then
                widget.on_destroy = binding:subscribe(function(v)
                    set_child(widget, v)
                end)
            else
                widget.on_destroy = binding:subscribe(function(v)
                    widget[prop] = v
                end)
            end
        end

        widget.visible = visible
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
    -- TODO: CircularProgress
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

return setmetatable(Widget, {
    __call = function(_, ctor)
        return astalify(ctor)
    end,
})
