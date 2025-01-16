import { App, Astal, Gdk, Gtk } from "astal/gtk3"

const { TOP, RIGHT, BOTTOM, LEFT } = Astal.WindowAnchor

type PopupProps = {
    child?: unknown
    marginBottom?: number
    marginTop?: number
    marginLeft?: number
    marginRight?: number
    halign?: Gtk.Align
    valign?: Gtk.Align
}

function Popup({
    child,
    marginBottom,
    marginTop,
    marginLeft,
    marginRight,
    halign = Gtk.Align.CENTER,
    valign = Gtk.Align.CENTER,
}: PopupProps) {
    return (
        <window
            visible={false}
            css="background-color: transparent"
            keymode={Astal.Keymode.EXCLUSIVE}
            anchor={TOP | RIGHT | BOTTOM | LEFT}
            exclusivity={Astal.Exclusivity.IGNORE}
            // close when click occurs otside of child
            onButtonPressEvent={(self, event) => {
                const [, _x, _y] = event.get_coords()
                const { x, y, width, height } = self
                    .get_child()!
                    .get_allocation()

                const xOut = _x < x || _x > x + width
                const yOut = _y < y || _y > y + height

                // clicked outside
                if (xOut || yOut) self.hide()
            }}
            // close when hitting Escape
            onKeyPressEvent={(self, event: Gdk.Event) => {
                if (event.get_keyval()[1] === Gdk.KEY_Escape) {
                    self.hide()
                }
            }}
        >
            <box
                className="Popup"
                onButtonPressEvent={() => true} // make sure click event does not bubble up
                // child can be positioned with `halign` `valign` and margins
                expand
                halign={halign}
                valign={valign}
                marginBottom={marginBottom}
                marginTop={marginTop}
                marginStart={marginLeft}
                marginEnd={marginRight}
            >
                {child}
            </box>
        </window>
    )
}

App.start({
    instanceName: "popup-example",
    css: `
        .Popup {
            background-color: @theme_bg_color;
            box-shadow: 2px 3px 7px 0 rgba(0,0,0,0.4);
            border-radius: 12px;
            padding: 12px;
        }
    `,
    main() {
        const popup = (
            <Popup
                marginTop={36}
                marginRight={60}
                valign={Gtk.Align.START}
                halign={Gtk.Align.END}
            >
                <button onClicked={() => popup.hide()}>
                    Click me to close the popup
                </button>
            </Popup>
        )

        return (
            <window
                anchor={TOP | LEFT | RIGHT}
                exclusivity={Astal.Exclusivity.EXCLUSIVE}
            >
                <button onClicked={() => popup.show()} halign={Gtk.Align.END}>
                    Click to open popup
                </button>
            </window>
        )
    },
})
