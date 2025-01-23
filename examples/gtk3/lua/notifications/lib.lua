local astal = require("astal")
local Variable = require("astal").Variable
local Gtk = require("astal.gtk3").Gtk
local GLib = astal.require("GLib")

local M = {}

function M.src(path)
	local str = debug.getinfo(2, "S").source:sub(2)
	local src = str:match("(.*/)") or str:match("(.*\\)") or "./"
	return src .. path
end

---@generic T, R
---@param array T[]
---@param func fun(T, i: integer): R
---@return R[]
function M.map(array, func)
	local new_arr = {}
	for i, v in ipairs(array) do
		new_arr[i] = func(v, i)
	end
	return new_arr
end

---@param path string
---@return boolean
function M.file_exists(path) return GLib.file_test(path, "EXISTS") end

function M.varmap(initial)
	local map = initial
	local var = Variable()

	local function notify()
		local arr = {}
		for _, value in pairs(map) do
			table.insert(arr, value)
		end
		var:set(arr)
	end

	local function delete(key)
		if Gtk.Widget:is_type_of(map[key]) then map[key]:destroy() end

		map[key] = nil
	end

	notify()

	return setmetatable({
		set = function(key, value)
			delete(key)
			map[key] = value
			notify()
		end,
		delete = function(key)
			delete(key)
			notify()
		end,
		get = function() return var:get() end,
		subscribe = function(callback) return var:subscribe(callback) end,
	}, {
		__call = function() return var() end,
	})
end

---@param time number
---@param format? string
function M.time(time, format)
	format = format or "%H:%M"
	return GLib.DateTime.new_from_unix_local(time):format(format)
end

return M
