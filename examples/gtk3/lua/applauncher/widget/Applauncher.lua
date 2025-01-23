local astal = require("astal")

local Apps = astal.require("AstalApps")
local App = require("astal.gtk3.app")
local Astal = require("astal.gtk3").Astal
local Gdk = require("astal.gtk3").Gdk
local Variable = astal.Variable
local Widget = require("astal.gtk3.widget")

local slice = require("lib").slice
local map = require("lib").map

local MAX_ITEMS = 8

local function hide()
	local launcher = App:get_window("launcher")
	if launcher then launcher:hide() end
end

local function AppButton(app)
	return Widget.Button({
		class_name = "AppButton",
		on_clicked = function()
			hide()
			app:launch()
		end,
		Widget.Box({
			Widget.Icon({ icon = app.icon_name }),
			Widget.Box({
				valign = "CENTER",
				vertical = true,
				Widget.Label({
					class_name = "name",
					wrap = true,
					xalign = 0,
					label = app.name,
				}),
				app.description and Widget.Label({
					class_name = "description",
					wrap = true,
					xalign = 0,
					label = app.description,
				}),
			}),
		}),
	})
end

return function()
	local apps = Apps.Apps()

	local text = Variable("")
	local list = text(
		function(t) return slice(apps:fuzzy_query(t), 1, MAX_ITEMS) end
	)

	local on_enter = function()
		local found = apps:fuzzy_query(text:get())[1]
		if found then
			found:launch()
			hide()
		end
	end

	return Widget.Window({
		name = "launcher",
		anchor = Astal.WindowAnchor.TOP + Astal.WindowAnchor.BOTTOM,
		exclusivity = "IGNORE",
		keymode = "ON_DEMAND",
		application = App,
		on_show = function() text:set("") end,
		on_key_press_event = function(self, event)
			if event.keyval == Gdk.KEY_Escape then self:hide() end
		end,
		Widget.Box({
			Widget.EventBox({
				expand = true,
				on_click = hide,
				width_request = 4000,
			}),
			Widget.Box({
				hexpand = false,
				vertical = true,
				Widget.EventBox({ on_click = hide, height_request = 100 }),
				Widget.Box({
					vertical = true,
					width_request = 500,
					class_name = "Applauncher",
					Widget.Entry({
						placeholder_text = "Search",
						text = text(),
						on_changed = function(self) text:set(self.text) end,
						on_activate = on_enter,
					}),
					Widget.Box({
						spacing = 6,
						vertical = true,
						list:as(function(l) return map(l, AppButton) end),
					}),
					Widget.Box({
						halign = "CENTER",
						class_name = "not-found",
						vertical = true,
						visible = list:as(function(l) return #l == 0 end),
						Widget.Icon({ icon = "system-search-symbolic" }),
						Widget.Label({ label = "No match found" }),
					}),
				}),
				Widget.EventBox({ expand = true, on_click = hide }),
			}),
			Widget.EventBox({
				width_request = 4000,
				expand = true,
				on_click = hide,
			}),
		}),
	})
end
