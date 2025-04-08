import Astal from "gi://Astal?version=4.0"
import AstalIO from "gi://AstalIO"
import GLib from "gi://GLib"
import Gtk from "gi://Gtk?version=4.0"
import GObject from "gi://GObject?version=2.0"
import AstalBattery from "gi://AstalBattery"
import AstalWp from "gi://AstalWp"
import AstalNetwork from "gi://AstalNetwork"
import AstalMpris from "gi://AstalMpris"
import AstalPowerProfiles from "gi://AstalPowerProfiles"
import AstalTray from "gi://AstalTray"
import AstalBluetooth from "gi://AstalBluetooth"
import { string, number, boolean } from "./props"

const { TOP, LEFT, RIGHT } = Astal.WindowAnchor
const SYNC = GObject.BindingFlags.SYNC_CREATE

export default class Bar extends Astal.Window {
    static {
        GObject.registerClass(
            {
                GTypeName: "Bar",
                Template: "resource:///ui/Bar.ui",
                InternalChildren: ["popover", "calendar", "traybox"],
                Properties: {
                    ...string("clock"),
                    ...string("volume-icon"),
                    ...boolean("battery-visible"),
                    ...string("battery-label"),
                    ...string("battery-icon"),
                    ...number("volume", 0, 1),
                    ...string("network-icon"),
                    ...boolean("mpris-visible"),
                    ...string("mpris-label"),
                    ...string("mpris-art"),
                    ...string("power-profile-icon"),
                    ...boolean("bluetooth-visible"),
                },
            },
            this,
        )
    }

    declare clock: string
    declare battery_label: string
    declare mpris_label: string
    declare bluetooth_visible: string

    declare _popover: Gtk.Popover
    declare _calendar: Gtk.Calendar
    declare _traybox: Gtk.Box

    constructor() {
        super({
            visible: true,
            exclusivity: Astal.Exclusivity.EXCLUSIVE,
            anchor: TOP | LEFT | RIGHT,
            cssClasses: ["Bar"],
        })

        // clock
        const timer = AstalIO.Time.interval(1000, () => {
            this.clock = GLib.DateTime.new_now_local().format("%H:%M:%S")!
        })
        this.connect("destroy", () => timer.cancel())

        // everytime popover is opened, select current day
        this._popover.connect("notify::visible", ({ visible }) => {
            if (visible) {
                this._calendar.select_day(GLib.DateTime.new_now_local())
            }
        })

        // network
        const nw = AstalNetwork.get_default()
        let networkBinding: GObject.Binding

        // @ts-expect-error mistyped
        nw.bind_property_full(
            "primary",
            this,
            "network-icon",
            SYNC,
            (_, primary: AstalNetwork.Primary) => {
                networkBinding?.unbind()

                switch (primary) {
                    case AstalNetwork.Primary.WIRED:
                        networkBinding = nw.wired.bind_property(
                            "icon-name",
                            this,
                            "network-icon",
                            SYNC,
                        )
                        return [false, ""]
                    case AstalNetwork.Primary.WIFI:
                        networkBinding = nw.wifi.bind_property(
                            "icon-name",
                            this,
                            "network-icon",
                            SYNC,
                        )
                        return [false, ""]
                    default:
                        return [true, "network-idle-symbolic"]
                }
            },
            null,
        )

        // battery
        const bat = AstalBattery.get_default()

        bat.bind_property("is-present", this, "battery-visible", SYNC)
        bat.bind_property("icon-name", this, "battery-icon", SYNC)

        this.battery_label = `${Math.floor(bat.percentage * 100)}%`
        const batteryId = bat.connect("notify::percentage", () => {
            this.battery_label = `${Math.floor(bat.percentage * 100)}%`
        })
        this.connect("destroy", () => bat.disconnect(batteryId))

        // volume
        const speaker = AstalWp.get_default()!.defaultSpeaker
        speaker.bind_property("volume-icon", this, "volume-icon", SYNC)
        speaker.bind_property("volume", this, "volume", SYNC)

        // mpris
        const player = AstalMpris.Player.new("spotify")
        player.bind_property("available", this, "mpris-visible", SYNC)
        player.bind_property("cover-art", this, "mpris-art", SYNC)

        this.mpris_label = `${player.artist} - ${player.title}`
        const playerId = player.connect("notify::metadata", () => {
            this.mpris_label = `${player.artist} - ${player.title}`
        })
        this.connect("destroy", () => player.disconnect(playerId))

        // powerprofiles
        const powerprofile = AstalPowerProfiles.get_default()
        powerprofile.bind_property(
            "icon-name",
            this,
            "power-profile-icon",
            SYNC,
        )

        // tray
        const tray = AstalTray.get_default()
        const trayItems = new Map<string, Gtk.MenuButton>()
        const trayId1 = tray.connect("item-added", (_, id) => {
            const item = tray.get_item(id)
            const popover = Gtk.PopoverMenu.new_from_model(item.menu_model)
            const icon = new Gtk.Image()
            const button = new Gtk.MenuButton({ popover, child: icon })

            item.bind_property("gicon", icon, "gicon", SYNC)
            popover.insert_action_group("dbusmenu", item.action_group)
            item.connect("notify::action-group", () => {
                popover.insert_action_group("dbusmenu", item.action_group)
            })

            trayItems.set(id, button)
            this._traybox.append(button)
        })

        const trayId2 = tray.connect("item-removed", (_, id) => {
            const button = trayItems.get(id)
            if (button) {
                this._traybox.remove(button)
                button.run_dispose()
                trayItems.delete(id)
            }
        })

        this.connect("destroy", () => {
            tray.disconnect(trayId1)
            tray.disconnect(trayId2)
        })

        // bluetooth
        const bt = AstalBluetooth.get_default()
        bt.bind_property("is-connected", this, "bluetooth-visible", SYNC)
    }

    change_volume(_scale: Gtk.Scale, _type: Gtk.ScrollType, value: number) {
        AstalWp.get_default()?.defaultSpeaker.set_volume(value)
    }
}
