local lgi = require("lgi")
local GObject = lgi.require("GObject", "2.0")

---@class Binding
---@field emitter table|Variable
---@field property? string
---@field transformFn function
local Binding = {}

---@param emitter table
---@param property? string
---@return Binding
function Binding.new(emitter, property)
    return setmetatable({
        emitter = emitter,
        property = property,
        transformFn = function(v)
            return v
        end,
    }, Binding)
end

function Binding:__tostring()
    local str = "Binding<" .. tostring(self.emitter)
    if self.property ~= nil then
        str = str .. ", " .. self.property
    end
    return str .. ">"
end

function Binding:get()
    if self.property ~= nil and GObject.Object:is_type_of(self.emitter) then
        return self.transformFn(self.emitter[self.property])
    end
    if type(self.emitter.get) == "function" then
        return self.transformFn(self.emitter:get())
    end
    error("can not get: Not a GObject or a Variable " + self)
end

---@param transform fun(value: any): any
---@return Binding
function Binding:as(transform)
    local b = Binding.new(self.emitter, self.property)
    b.transformFn = function(v)
        return transform(self.transformFn(v))
    end
    return b
end

---@param callback fun(value: any)
---@return function
function Binding:subscribe(callback)
    if self.property ~= nil and GObject.Object:is_type_of(self.emitter) then
        local id = self.emitter.on_notify:connect(function()
            callback(self:get())
        end, self.property, false)
        return function()
            GObject.signal_handler_disconnect(self.emitter, id)
        end
    end
    if type(self.emitter.subscribe) == "function" then
        return self.emitter:subscribe(function()
            callback(self:get())
        end)
    end
    error("can not subscribe: Not a GObject or a Variable " + self)
end

Binding.__index = Binding
return Binding
