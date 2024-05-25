#!/usr/bin/env -S gjs -m
import { Variable, App, Widget, Astal, bind } from "./dist/index.js"
import Playerctl from "gi://Playerctl"

// state
const player = Playerctl.Player.new("spotify")
const date = Variable("").poll(1000, "date")
const title = Variable(player.get_title()).observe(player, "metadata", () => player.get_title())

// ui
function Bar(monitor) {
    return Widget.Window(
        {
            monitor,
            application: App,
            exclusivity: Astal.Exclusivity.EXCLUSIVE,
            anchor: Astal.WindowAnchor.BOTTOM |
                Astal.WindowAnchor.LEFT |
                Astal.WindowAnchor.RIGHT,
        },
        Widget.CenterBox({
            startWidget: Widget.Label({
                label: date(l => `Current date: ${l}`),
            }),
            endWidget: Widget.Label({
                label: bind(title).as(t => `Title: ${t}`),
            }),
        }),
    )
}

// main
App.start({
    requestHandler(msg, res) {
        switch (msg) {
            case "inspector": return res(App.inspector())
            case "quit": return res(App.quit())
            default: return App.eval(msg).then(res).catch(console.error)
        }
    },
}, () => {
    Bar(0)
})
