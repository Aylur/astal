local lgi = require("lgi")
local GObject = lgi.require("GObject", "2.0")

---@class Binding
---@field emitter object
---@field property? string
---@field transformFn function
local Binding = {}

---@param emitter object
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
    local str = "Binding<" .. tostring(self:get())
    if self.property ~= nil then
        str = str .. ", " .. self.property
    end
    return str .. ">"
end

function Binding:get()
    if type(self.emitter.get) == "function" then
        return self.transformFn(self.emitter:get())
    end
    return self.transformFn(self.emitter[self.property])
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
    if type(self.emitter.subscribe) == "function" then
        return self.emitter:subscribe(function()
            callback(self:get())
        end)
    end
    local id = self.emitter.on_notify:connect(function()
        callback(self:get())
    end, self.property, false)
    return function()
        GObject.signal_handler_disconnect(self.emitter, id)
    end
end

Binding.__index = Binding
return Binding
