local lgi = require("lgi")
local Astal = lgi.require("AstalIO", "0.1")
local GObject = lgi.require("GObject", "2.0")
local Binding = require("astal.binding")
local Time = require("astal.time")
local Process = require("astal.process")

---@class Variable
---@field private variable table
---@field private err_handler? function
---@field private _value any
---@field private _poll? table
---@field private _watch? table
---@field private poll_interval number
---@field private poll_exec? string[] | string
---@field private poll_transform? fun(next: any, prev: any): any
---@field private poll_fn? function
---@field private watch_transform? fun(next: any, prev: any): any
---@field private watch_exec? string[] | string
---@field private __index Variable
---@overload fun(transform?: fun(v: any): any): Binding
local Variable = {}

Variable.__index = Variable ---@diagnostic disable-line

---@param value? any
---@return Variable
function Variable.new(value)
    local v = Astal.VariableBase()
    local variable = setmetatable({ variable = v, _value = value }, Variable)

    v.on_error = function(_, err)
        if variable.err_handler then
            variable.err_handler(err)
        end
    end

    return variable:on_dropped(function()
        variable:stop_watch()
        variable:stop_poll()
    end)
end

---@private
---@param transform? fun(v: any): any
---@return Binding
function Variable:__call(transform)
    if type(transform) == "nil" then
        return Binding.new(self)
    else
        return Binding.new(self):as(transform)
    end
end

---@private
function Variable:__tostring()
    return "Variable<" .. tostring(self:get()) .. ">"
end

---@return any
function Variable:get()
    return self._value
end

---@param value any
function Variable:set(value)
    if value ~= self:get() then
        self._value = value
        self.variable:emit_changed()
    end
end

function Variable:is_polling()
    return self._poll ~= nil
end

function Variable:is_watching()
    return self._watch ~= nil
end

function Variable:start_poll()
    if self:is_polling() then
        return
    end

    if self.poll_fn then
        self._poll = Time.interval(self.poll_interval, function()
            self:set(self.poll_fn(self:get()))
        end)
    elseif self.poll_exec then
        self._poll = Time.interval(self.poll_interval, function()
            Process.exec_async(self.poll_exec, function(out, err)
                if err ~= nil then
                    return self.variable:emit_error(err)
                else
                    self:set(self.poll_transform(out, self:get()))
                end
            end)
        end)
    end
end

function Variable:start_watch()
    if self:is_watching() then
        return
    end

    self._watch = Process.subprocess(self.watch_exec, function(out)
        self:set(self.watch_transform(out, self:get()))
    end, function(err)
        self.variable:emit_error(err)
    end)
end

function Variable:stop_poll()
    if self:is_polling() then
        self._poll:cancel()
    end
    self._poll = nil
end

function Variable:stop_watch()
    if self:is_watching() then
        self._watch:kill()
    end
    self._watch = nil
end

function Variable:drop()
    self.variable:emit_dropped()
end

---@param callback function
---@return Variable
function Variable:on_dropped(callback)
    self.variable.on_dropped = callback
    return self
end

---@param callback function
---@return Variable
function Variable:on_error(callback)
    self.err_handler = nil
    self.variable.on_error = function(_, err)
        callback(err)
    end
    return self
end

---@param callback fun(value: any)
---@return function
function Variable:subscribe(callback)
    local id = self.variable.on_changed:connect(function()
        callback(self:get())
    end)
    return function()
        GObject.signal_handler_disconnect(self.variable, id)
    end
end

---@param interval number
---@param exec string | string[] | function
---@param transform? fun(next: any, prev: any): any
function Variable:poll(interval, exec, transform)
    transform = transform or function(next)
        return next
    end

    self:stop_poll()
    self.poll_interval = interval
    self.poll_transform = transform

    if type(exec) == "function" then
        self.poll_fn = exec
        self.poll_exec = nil
    else
        self.poll_exec = exec
        self.poll_fn = nil
    end
    self:start_poll()
    return self
end

---@param exec string | string[]
---@param transform? fun(next: any, prev: any): any
---@return Variable
function Variable:watch(exec, transform)
    transform = transform or function(next)
        return next
    end

    self:stop_watch()
    self.watch_exec = exec
    self.watch_transform = transform
    self:start_watch()
    return self
end

---@param object table
---@param sigOrFn string
---@param callback fun(...): any
---@return Variable
---@overload fun(self: Variable, object: { [1]: table, [2]: string }[], callback: fun(...): any): Variable
function Variable:observe(object, sigOrFn, callback)
    local f
    if type(sigOrFn) == "function" then
        f = sigOrFn
    else
        f = callback or function()
            return self:get()
        end
    end

    local set = function(...)
        self:set(f(...))
    end

    local arr = {}

    if type(sigOrFn) == "string" then
        table.insert(arr, { object, sigOrFn })
    end

    for _, tbl in ipairs(arr) do
        local id
        local obj, signal = tbl[1], tbl[2]

        if string.sub(signal, 1, 8) == "notify::" then
            local prop = string.gsub(signal, "notify::", "")
            id = obj.on_notify:connect(function()
                set(obj, obj[prop])
            end, prop, false)
        else
            id = obj["on_" .. signal]:connect(set)
        end

        self:on_dropped(function()
            GObject.signal_handler_disconnect(obj, id)
        end)
    end

    return self
end

---@param deps Variable | table<integer, Binding | Variable>
---@param transform? fun(...): any
---@return Variable
function Variable.derive(deps, transform)
    transform = transform or function(...)
        return { ... }
    end

    if getmetatable(deps) == Variable then
        local var = Variable.new(transform(deps:get()))

        return var:on_dropped(deps:subscribe(function(v)
            var:set(transform(v))
        end))
    end

    for i, var in ipairs(deps) do
        if getmetatable(var) == Variable then
            deps[i] = var()
        end
    end

    local function update()
        local params = {}
        for i, binding in ipairs(deps) do
            params[i] = binding:get()
        end
        return transform(table.unpack(params, 1, #deps))
    end

    local var = Variable.new(update())

    local unsubs = {}

    for i, b in ipairs(deps) do
        unsubs[i] = b:subscribe(function()
            var:set(update())
        end)
    end

    return var:on_dropped(function()
        for _, unsub in ipairs(unsubs) do
            unsub()
        end
    end)
end

return setmetatable(Variable, {
    __call = function(_, v)
        return Variable.new(v)
    end,
})
