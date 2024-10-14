local lgi = require("lgi")
local Binding = require("astal.lib.binding")
local File = require("astal.lib.file")
local Process = require("astal.lib.process")
local Time = require("astal.lib.time")
local Variable = require("astal.lib.variable")

return {
    Variable = Variable,
    bind = Binding.new,

    interval = Time.interval,
    timeout = Time.timeout,
    idle = Time.idle,

    subprocess = Process.subprocess,
    exec = Process.exec,
    exec_async = Process.exec_async,

    read_file = File.read_file,
    read_file_async = File.read_file_async,
    write_file = File.write_file,
    write_file_async = File.write_file_async,
    monitor_file = File.monitor_file,

    require = lgi.require,
}
