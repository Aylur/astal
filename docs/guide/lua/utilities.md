# Utilities

## File functions

```lua
local read_file = astal.read_file
local read_file_async = astal.read_file_async
local write_file = astal.write_file
local write_file_async = astal.write_file_async
local monitor_file = astal.monitor_file
```

### Reading files

```lua
---@param path string
---@return string
local function read_file(path) end

---@param path string
---@param callback fun(content: string, err: string): nil
local function read_file_async(path, callback) end
```

### Writing files

```lua
---@param path string
---@param content string
local function write_file(path, content) end

---@param path string
---@param content string
---@param callback? fun(err: string): nil
local function write_file_async(path, content, callback) end
```

### Monitoring files

```lua
---@param path string
---@param callback fun(file: string, event: Gio.FileMonitorEvent): nil
local function monitor_file(path, callback) end
```

## Timeouts and Intervals

```lua
local interval = astal.interval
local timeout = astal.timeout
local idle = astal.idle
```

### Interval

Will immediately execute the function and every `interval` millisecond.

```lua
---@param interval number
---@param callback? function
---@return Astal.Time
local function interval(interval, callback) end
```

### Timeout

Will execute the `callback` after `timeout` millisecond.

```lua
---@param timeout number
---@param callback? function
---@return Astal.Time
local function timeout(timeout, callback) end
```

### Idle

Executes `callback` whenever there are no higher priority events pending.

```lua
---@param callback? function
---@return Astal.Time
local function idle(callback) end
```

Example:

```lua
local timer = interval(1000, function()
  print("optional callback")
end)

-- connect using signals
local id_1 = timer.on_now:connect(function() end)
local id_2 = timer.on_cancelled:connect(function() end)

-- or
function timer:on_now() end
function timer:on_cancelled() end

timer:cancel()
```

## Process functions

```lua
local subprocess = astal.subprocess
local exec = astal.exec
local exec_async = astal.exec_async
```

### Subprocess

You can start a subprocess and run callback functions whenever it outputs to
stdout or stderr. [Astal.Process](https://aylur.github.io/libastal/io/class.Process.html) has a `stdout` and `stderr` signal.

```lua
---@param commandline string | string[]
---@param on_stdout? fun(out: string): nil
---@param on_stderr? fun(err: string): nil
---@return Astal.Process | nil
local function subprocess(commandline, on_stdout, on_stderr) end
```

Example:

```lua
local proc = subprocess(
  "some-command",
  function(out) end,
  function(err) end,
)

-- with signals
local proc = subprocess("some-command")
local id_1 = proc.on_stdout:connect(function(_, stdout) end)
local id_2 = proc.on_stderr:connect(function(_, stderr) end)

-- or
function proc:on_stdout(_, stdout) end
function proc:on_stderr(_, stderr) end
```

### Executing external commands and scripts

```lua
---@param commandline string | string[]
---@return string, string
local function exec(commandline) end

---@param commandline string | string[]
---@param callback? fun(out: string, err: string): nil
local function exec_async(commandline, callback) end
```

Example:

```lua
local out, err = exec("/path/to/script")

if err ~= nil then
  print(err)
else
  print(out)
end

exec_async({ "bash", "-c", "/path/to/script.sh" }, function(out, err)
	if err ~= nil then
		print(err)
	else
		print(out)
	end
end)
```

:::warning
`subprocess`, `exec`, and `exec_async` executes the passed executable as is.
They are **not** executed in a shell environment,
they do **not** expand ENV variables like `$HOME`,
and they do **not** handle logical operators like `&&` and `||`.

If you want bash, run them with bash.

```lua
exec({ "bash", "-c", "command $VAR && command" })
exec("bash -c 'command $VAR' && command")
```

:::
