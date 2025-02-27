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
---@param follow_symlinks boolean
function M.write_file(path, content, follow_symlinks)
    Astal.write_file(path, content, follow_symlinks or true)
end

---@param path string
---@param content string
---@param callback? fun(err: string): nil
---@param follow_symlinks boolean
function M.write_file_async(path, content, callback, follow_symlinks)
    Astal.write_file_async(path, content, function(_, res)
        if type(callback) == "function" then
            callback(Astal.write_file_finish(res))
        end
    end, follow_symlinks or true)
end

---@param path string
---@param callback fun(file: string, event: integer): nil
---@param follow_symlinks boolean
function M.monitor_file(path, callback, follow_symlinks)
    return Astal.monitor_file(path, GObject.Closure(callback), follow_symlinks or true)
end

return M
