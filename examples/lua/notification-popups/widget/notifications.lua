local astal = require("astal")
local Widget = astal.Widget
local GLib = astal.GLib
local bind = astal.bind
local timeout = astal.timeout

local map = require("lib").map
local lookup_icon = require("lib").lookup_icon

local Notifd = astal.require("AstalNotifd")
local notifd = Notifd.get_default()

local NOTIFICATION_TIMEOUTMS = 5000

local function NotificationIcon(n)
	local icon = "dialog-information-symbolic"

	if n.app_icon and GLib.file_test(n.app_icon, "EXISTS") then
		return Widget.Box({ css = string.format('background-image: url("%s");', n.app_icon) })
	elseif n.app_icon and lookup_icon(n.app_icon) then
		icon = n.app_icon
	elseif n.app_name and lookup_icon(n.app_name) then
		icon = n.app_name
	end

	return Widget.Icon({
		icon = icon,
	})
end

local function Notification(n)
	local icon = Widget.Box({
		valign = "START",
		class_name = "icon",
		NotificationIcon(n),
	})

	local title = Widget.Label({
		class_name = "title",
		xalign = 0,
		justify = "LEFT",
		hexpand = true,
		max_width_chars = 24,
		wrap = true,
		ellipsize = "END",
		use_markup = true,
		label = n.summary,
	})

	local body = Widget.Label({
		class_name = "body",
		hexpand = true,
		use_markup = true,
		xalign = 0,
		justify = "LEFT",
		wrap = true,
		label = n.body,
	})

	local actions = Widget.Box({
		class_name = "actions",
		map(n.actions, function(action)
			return Widget.Button({
				on_click_release = function()
					return n:invoke(action.id)
				end,
				class_name = "action-button",
				hexpand = true,
				label = action.label,
			})
		end),
	})
	return Widget.EventBox({
		Widget.Box({
			class_name = "notification",
			orientation = "VERTICAL",
			Widget.Box({
				icon,
				Widget.Box({
					orientation = "VERTICAL",
					title,
					body,
				}),
			}),
			actions,
		}),
	})
end

return function(gdkmonitor)
	local n_list = {}

	local list = Widget.Box({
		orientation = "VERTICAL",
		setup = function(self)
			self:hook(notifd, "notified", function(_, id)
				local n = notifd:get_notification(id)
				n_list[id] = Notification(n)
				self:add(n_list[id])
				local timeout_ms = n.expire_timeout > 0 and n.expire_timeout or NOTIFICATION_TIMEOUTMS
				timeout(timeout_ms, function()
					return n:dismiss()
				end)
			end)
			self:hook(notifd, "resolved", function(_, id)
				if n_list[id] then
					n_list[id]:destroy()
					n_list[id] = nil
				end
			end)
		end,
	})

	return Widget.Window({
		namespace = "notifications",
		gdkmonitor = gdkmonitor,
		anchor = astal.Astal.WindowAnchor.TOP,
		layer = "OVERLAY",
		class_name = "notification-popups",
		visible = bind(list, "children"):as(function(v)
			return v and #v > 0
		end),
		list,
	})
end
