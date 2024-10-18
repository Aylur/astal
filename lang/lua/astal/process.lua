local lgi = require("lgi")
local Astal = lgi.require("AstalIO", "0.1")

local M = {}

M.Process = Astal.Process

---@param commandline string | string[]
---@param on_stdout? fun(out: string): nil
---@param on_stderr? fun(err: string): nil
---@return { kill: function } | nil proc
function M.subprocess(commandline, on_stdout, on_stderr)
    if on_stdout == nil then
        on_stdout = function(out)
            io.stdout:write(tostring(out) .. "\n")
        end
    end

    if on_stderr == nil then
        on_stderr = function(err)
            io.stderr:write(tostring(err) .. "\n")
        end
    end

    local proc, err
    if type(commandline) == "table" then
        proc, err = Astal.Process.subprocessv(commandline)
    else
        proc, err = Astal.Process.subprocess(commandline)
    end
    if err ~= nil then
        err(err)
        return nil
    end
    proc.on_stdout = function(_, stdoud)
        on_stdout(stdoud)
    end
    proc.on_stderr = function(_, stderr)
        on_stderr(stderr)
    end
    return proc
end

---@param commandline string | string[]
---@return string, string
function M.exec(commandline)
    if type(commandline) == "table" then
        return Astal.Process.execv(commandline)
    else
        return Astal.Process.exec(commandline)
    end
end

---@param commandline string | string[]
---@param callback? fun(out: string, err: string): nil
function M.exec_async(commandline, callback)
    if callback == nil then
        callback = function(out, err)
            if err ~= nil then
                io.stdout:write(tostring(out) .. "\n")
            else
                io.stderr:write(tostring(err) .. "\n")
            end
        end
    end

    if type(commandline) == "table" then
        Astal.Process.exec_asyncv(commandline, function(_, res)
            local out, err = Astal.Process.exec_asyncv_finish(res)
            callback(out, err)
        end)
    else
        Astal.Process.exec_async(commandline, function(_, res)
            local out, err = Astal.Process.exec_finish(res)
            callback(out, err)
        end)
    end
end

return M
