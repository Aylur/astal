import { Astal, Gdk, Gtk, Widget } from "astal/gtk3"

const { TOP, BOTTOM, LEFT, RIGHT } = Astal.WindowAnchor

type PopoverProps = Pick<
    Widget.WindowProps,
    | "name"
    | "namespace"
    | "className"
    | "visible"
    | "child"
    | "marginBottom"
    | "marginTop"
    | "marginLeft"
    | "marginRight"
    | "halign"
    | "valign"
> & {
    onClose?(self: Widget.Window): void
}

export default function Popover({
    child,
    visible = false,
    marginBottom,
    marginTop,
    marginLeft,
    marginRight,
    halign = Gtk.Align.CENTER,
    valign = Gtk.Align.CENTER,
    onClose,
    ...props
}: PopoverProps) {
    return (
        <window
            {...props}
            css="background-color: transparent"
            keymode={Astal.Keymode.EXCLUSIVE}
            anchor={TOP | BOTTOM | LEFT | RIGHT}
            exclusivity={Astal.Exclusivity.IGNORE}
            onNotifyVisible={(self) => {
                // instead of anchoring to all sides we set the width explicitly
                // otherwise label wrapping won't work correctly without setting their width
                if (self.visible) {
                    self.widthRequest = self.get_current_monitor().workarea.width
                } else {
                    onClose?.(self)
                }
            }}
            // close when click occurs otside of child
            onButtonPressEvent={(self, event) => {
                const [, _x, _y] = event.get_coords()
                const { x, y, width, height } = self
                    .get_child()!
                    .get_allocation()

                const xOut = _x < x || _x > x + width
                const yOut = _y < y || _y > y + height

                // clicked outside
                if (xOut || yOut) {
                    self.visible = false
                }
            }}
            // close when hitting Escape
            onKeyPressEvent={(self, event: Gdk.Event) => {
                if (event.get_keyval()[1] === Gdk.KEY_Escape) {
                    self.visible = false
                }
            }}
        >
            <box
                // make sure click event does not bubble up
                onButtonPressEvent={() => true}
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
