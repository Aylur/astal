from typing import Callable

from gi.repository import AstalIO

def interval(interval: int, callback: Callable) -> AstalIO.Time:
    return AstalIO.Time.interval(interval, callback)

def timeout(timeout: int, callback: Callable) -> AstalIO.Time:
    return AstalIO.Time.timeout(timeout, callback)

def idle(callback: Callable) -> AstalIO.Time:
    return AstalIO.Time.idle(callback)

