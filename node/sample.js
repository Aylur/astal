#!/usr/bin/env node
import { Variable, App, Widget, Astal, bind, gi } from "./dist/index.js"
const Playerctl = gi.require("Playerctl", "2.0")

// state
const player = Playerctl.Player.new("spotify")
const title = Variable(player.getTitle()).observe(player, "metadata", () => player.getTitle())
const date = Variable("")
    // FIXME: doesn't work because promises don't resolve
    // .poll(1000, "date")
    // FIXME: don't know why but this doesn't work either
    // .watch("bash -c 'while true; do date; sleep 1; done'")
    // this does
    .poll(1000, Date)

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
