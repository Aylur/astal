local astal = require("astal")
local App = require("astal.gtk3.app")

local AppLauncher = require("widget.Applauncher")
local src = require("lib").src

local scss = src("style.scss")
local css = "/tmp/style.css"

astal.exec("sass " .. scss .. " " .. css)

App:start({
	instance_name = "launcher",
	css = css,
	main = AppLauncher,
})
