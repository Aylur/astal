# Signals

ignore this, I'm just dumb and can't follow where signals go or get emitted from

## Notification

* resolved(reason) - by daemon/proxy
* dismissed() - by user with `.dismiss()`, used to emit resolved from proxy/daemon
* invoked(action) - by user with `.invoke()`

## Deamon

non-spec, used by user

* notified(id, replaced) - by outside through dbus with `.Notify()`
* resolved(id, reason) - by `Notification.dismiss()` or outside with `.CloseNotification`

spec, not used by user

* notification_closed(id, reason) - sideeffect of `resolved`
* action_invoked(id, action) - by `Notification.invoke()`

## Proxy

mirrors Daemon

* notified(id, replaced)
* resolved(id, reason)

creates `Notification` objects through daemon's json strings
and hooks them up to call daemon's signals and vice versa

## Notifd

acts as a bridge between Proxy/Daemon, everything else is internal only
