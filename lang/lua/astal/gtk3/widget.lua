local lgi = require("lgi")
local Astal = lgi.require("Astal", "3.0")
local Gtk = lgi.require("Gtk", "3.0")
local astalify = require("astal.gtk3.astalify")

---@overload fun(ctor: any): function
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
    MenuButton = astalify(Gtk.MenuButton),
    Overlay = astalify(Astal.Overlay),
    Revealer = astalify(Gtk.Revealer),
    Scrollable = astalify(Astal.Scrollable),
    Slider = astalify(Astal.Slider),
    Stack = astalify(Astal.Stack),
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
        local strings = Astal.widget_get_class_names(self)
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

Gtk.Widget._attribute.action_group = {
  set = function (self, v)
    self:insert_action_group(v[1], v[2])
  end
}

local no_implicit_destroy = {}
Gtk.Widget._attribute.no_implicit_destroy = {
    get = function(self)
        return no_implicit_destroy[self] or false
    end,
    set = function(self, v)
        if no_implicit_destroy[self] == nil then
            self.on_destroy = function()
                no_implicit_destroy[self] = nil
            end
        end
        no_implicit_destroy[self] = v
    end,
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
