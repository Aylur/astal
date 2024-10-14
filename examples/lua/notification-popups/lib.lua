local Variable = require("astal").Variable
local astal = require("astal")
local Gtk = astal.Gtk

local M = {}

function M.src(path)
	local str = debug.getinfo(2, "S").source:sub(2)
	local src = str:match("(.*/)") or str:match("(.*\\)") or "./"
	return src .. path
end

---@generic T, R
---@param arr T[]
---@param func fun(T, integer): R
---@return R[]
function M.map(arr, func)
	local new_arr = {}
	for i, v in ipairs(arr) do
		new_arr[i] = func(v, i)
	end
	return new_arr
end

---@param name string
---@param size? 16 | 32 | 64 | 128 | 256 | 512 | number
function M.lookup_icon(name, size)
	if not name or #name == 0 then
		return
	end
	size = size or 256

	local theme = Gtk.IconTheme.get_default()
	local icon_info, path

	for _, n in ipairs({
		name,
		string.lower(name),
		string.upper(name),
	}) do
		icon_info = theme:lookup_icon(n, size, "USE_BUILTIN")

		if icon_info then
			return icon_info
		end
	end
	return false
end

M.date = Variable(""):poll(1000, "date")

return M
