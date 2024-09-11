#!/usr/bin/lua
local astal = require("astal")
local App = astal.App

local Bar = require("widget.Bar")
local src = require("lib").src

local scss = src("style.scss")
local css = "/tmp/style.css"

astal.exec("sass " .. scss .. " " .. css)

App:start({
	css = css,
	main = function()
		for _, mon in pairs(App.monitors) do
			Bar(mon)
		end
	end,
})
