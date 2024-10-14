local astal = require("astal")
local Notify = astal.require("Notify")
local timeout = astal.timeout
local App = astal.App

local Notifications = require("widget.notifications")
local src = require("lib").src

local scss = src("style.scss")
local css = "/tmp/style.css"

astal.exec("sass " .. scss .. " " .. css)

App:start({
	css = css,
	main = function()
		for _, mon in pairs(App.monitors) do
			Notifications(mon)
		end
	end,
})
