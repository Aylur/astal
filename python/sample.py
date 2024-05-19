#!/usr/bin/env python3
import gi

gi.require_version("Playerctl", "2.0")

from gi.repository import Playerctl
from astal import App, Astal, Variable, Widget, bind

player = Playerctl.Player.new("spotify")
v = Variable(player.get_title()).observe(player, "metadata", lambda *_: player.get_title())


def Bar(monitor):
    return Widget.Window(
        anchor=Astal.WindowAnchor.BOTTOM | Astal.WindowAnchor.LEFT | Astal.WindowAnchor.RIGHT,
        monitor=monitor,
        exclusivity=Astal.Exclusivity.EXCLUSIVE,
        child=Widget.CenterBox(
            start_widget=Widget.Label(
                label="Welcome to Astal.py!",
            ),
            end_widget=Widget.Label(label=v()),
        ),
    )


def start():
    Bar(0)


App.start(callback=start)
