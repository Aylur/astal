local lgi = require("lgi")
local Astal = lgi.require("Astal", "0.1")

local M = {}

local defualt_proc_args = function(on_stdout, on_stderr)
    if on_stdout == nil then
        on_stdout = function(out)
            io.stdout:write(tostring(out) .. "\n")
            return tostring(out)
        end
    end

    if on_stderr == nil then
        on_stderr = function(err)
            io.stderr:write(tostring(err) .. "\n")
            return tostring(err)
        end
    end

    return on_stdout, on_stderr
end

---@param commandline string | string[]
---@param on_stdout? fun(out: string): nil
---@param on_stderr? fun(err: string): nil
---@return { kill: function } | nil proc
function M.subprocess(commandline, on_stdout, on_stderr)
    local out, err = defualt_proc_args(on_stdout, on_stderr)
    local proc, fail
    if type(commandline) == "table" then
        proc, fail = Astal.Process.subprocessv(commandline)
    else
        proc, fail = Astal.Process.subprocess(commandline)
    end
    if fail ~= nil then
        err(fail)
        return nil
    end
    proc.on_stdout = function(_, str)
        out(str)
    end
    proc.on_stderr = function(_, str)
        err(str)
    end
    return proc
end

---@generic T
---@param commandline string | string[]
---@param on_stdout? fun(out: string): T
---@param on_stderr? fun(err: string): T
---@return T
function M.exec(commandline, on_stdout, on_stderr)
    local out, err = defualt_proc_args(on_stdout, on_stderr)
    local stdout, stderr
    if type(commandline) == "table" then
        stdout, stderr = Astal.Process.execv(commandline)
    else
        stdout, stderr = Astal.Process.exec(commandline)
    end
    if stderr then
        return err(stderr)
    end
    return out(stdout)
end

---@param commandline string | string[]
---@param on_stdout? fun(out: string): nil
---@param on_stderr? fun(err: string): nil
---@return { kill: function } | nil proc
function M.exec_async(commandline, on_stdout, on_stderr)
    local out, err = defualt_proc_args(on_stdout, on_stderr)
    local proc, fail
    if type(commandline) == "table" then
        proc, fail = Astal.Process.exec_asyncv(commandline)
    else
        proc, fail = Astal.Process.exec_async(commandline)
    end
    if fail ~= nil then
        err(fail)
        return nil
    end
    proc.on_stdout = function(_, str)
        out(str)
    end
    proc.on_stderr = function(_, str)
        err(str)
    end
    return proc
end

return M
