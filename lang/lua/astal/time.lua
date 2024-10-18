local lgi = require("lgi")
local Astal = lgi.require("AstalIO", "0.1")
local GObject = lgi.require("GObject", "2.0")

local M = {}

M.Time = Astal.Time

---@param interval number
---@param fn function
---@return { cancel: function, on_now: function }
function M.interval(interval, fn)
    return Astal.Time.interval(interval, GObject.Closure(fn))
end

---@param timeout number
---@param fn function
---@return { cancel: function, on_now: function }
function M.timeout(timeout, fn)
    return Astal.Time.timeout(timeout, GObject.Closure(fn))
end

---@param fn function
---@return { cancel: function, on_now: function }
function M.idle(fn)
    return Astal.Time.idle(GObject.Closure(fn))
end

return M
