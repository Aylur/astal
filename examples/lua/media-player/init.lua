local astal = require("astal")
local App = require("astal.gtk3.app")
local Widget = require("astal.gtk3.widget")

local MprisPlayers = require("widget.MediaPlayer")
local src = require("lib").src

local scss = src("style.scss")
local css = "/tmp/style.css"

astal.exec("sass " .. scss .. " " .. css)

App:start({
	instance_name = "lua",
	css = css,
	main = function()
		Widget.Window({ MprisPlayers() })
	end,
})
