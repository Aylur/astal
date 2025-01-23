local astal = require("astal")
local Widget = require("astal.gtk3").Widget

local Notifd = astal.require("AstalNotifd")
local Notification = require("notifications.Notification")
local timeout = astal.timeout

local TIMEOUT_DELAY = 5000

local varmap = require("lib").varmap
local notifd = Notifd.get_default()

local function NotificationMap()
	local notif_map = varmap({})

	notifd.on_notified = function(_, id)
		notif_map.set(
			id,
			Notification({
				notification = notifd:get_notification(id),
				-- once hovering over the notification is done
				-- destroy the widget without calling notification.dismiss()
				-- so that it acts as a "popup" and we can still display it
				-- in a notification center like widget
				-- but clicking on the close button will close it
				on_hover_lost = function() notif_map.delete(id) end,
				setup = function()
					timeout(TIMEOUT_DELAY, function()
						-- uncomment this if you want to "hide" the notifications
						-- after TIMEOUT_DELAY

						-- NotificationMap.delete(id)
					end)
				end,
			})
		)
	end

	notifd.on_resolved = function(_, id) notif_map.delete(id) end

	return notif_map
end

return function(gdkmonitor)
	local Anchor = astal.require("Astal").WindowAnchor
	local notifs = NotificationMap()

	return Widget.Window({
		class_name = "NotificationPopups",
		gdkmonitor = gdkmonitor,
		anchor = Anchor.TOP + Anchor.RIGHT,
		Widget.Box({
			vertical = true,
			notifs(),
		}),
	})
end
