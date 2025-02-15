import gi

from typing import Callable

gi.require_version("AstalIO", "0.1")
gi.require_version("GObject", "2.0")

from gi.repository import AstalIO, GObject

def interval(interval: int, callback: Callable) -> AstalIO.Time:
    return AstalIO.Time.interval(interval, callback)

def timeout(timeout: int, callback: Callable) -> AstalIO.Time:
    return AstalIO.Time.timeout(timeout, callback)

def idle(callback: Callable) -> AstalIO.Time:
    return AstalIO.Time.idle(callback)

