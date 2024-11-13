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

---@generic T
---@param array T[]
---@param start integer
---@param stop? integer
---@return T[]
function M.slice(array, start, stop)
	local new_arr = {}

	stop = stop or #array

	for i = start, stop do
		table.insert(new_arr, array[i])
	end

	return new_arr
end

return M
