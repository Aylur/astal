local lgi = require("lgi")
local Astal = lgi.require("AstalIO", "0.1")
local GObject = lgi.require("GObject", "2.0")

local M = {}

---@param path string
---@return string
function M.read_file(path)
    return Astal.read_file(path)
end

---@param path string
---@param callback fun(content: string, err: string): nil
function M.read_file_async(path, callback)
    Astal.read_file_async(path, function(_, res)
        local content, err = Astal.read_file_finish(res)
        callback(content, err)
    end)
end

---@param path string
---@param content string
function M.write_file(path, content)
    Astal.write_file(path, content)
end

---@param path string
---@param content string
---@param callback? fun(err: string): nil
function M.write_file_async(path, content, callback)
    Astal.write_file_async(path, content, function(_, res)
        if type(callback) == "function" then
            callback(Astal.write_file_finish(res))
        end
    end)
end

---@param path string
---@param callback fun(file: string, event: integer): nil
function M.monitor_file(path, callback)
    return Astal.monitor_file(path, GObject.Closure(callback))
end

return M
