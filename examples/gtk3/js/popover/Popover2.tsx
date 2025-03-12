import { Astal, Gdk, Widget } from "astal/gtk3"
import Variable from "astal/variable"

type Popover2Props = Pick<
    Widget.WindowProps,
    | "name"
    | "namespace"
    | "className"
    | "visible"
    | "child"
> & {
    onClose?(self: Widget.Window): void
}

/**
 * Full screen window where the child is positioned to center.
 *
 * NOTE: Workaround for the label wrap issue by padding the window
 * with eventboxes and only anchoring to TOP | BOTTOM.
 */
export default function Popover2({
    child,
    onClose,
    ...props
}: Popover2Props) {
    let win: Widget.Window

    const width = Variable(1000)
    const hide = () => (win.visible = false)

    return (
        <window
            {...props}
            setup={self => win = self}
            css="background-color: transparent"
            keymode={Astal.Keymode.EXCLUSIVE}
            anchor={Astal.WindowAnchor.TOP | Astal.WindowAnchor.BOTTOM}
            exclusivity={Astal.Exclusivity.IGNORE}
            onNotifyVisible={(self) => {
                // instead of anchoring to all sides we set the width explicitly
                // otherwise label wrapping won't work correctly without setting their width
                if (self.visible) {
                    width.set(self.get_current_monitor().workarea.width)
                } else {
                    onClose?.(self)
                }
            }}
            // close when hitting Escape
            onKeyPressEvent={(self, event: Gdk.Event) => {
                if (event.get_keyval()[1] === Gdk.KEY_Escape) {
                    self.visible = false
                }
            }}
        >
            <box>
                <eventbox widthRequest={width(w => w / 2)} expand onClick={hide} />
                <box hexpand={false} vertical>
                    <eventbox expand onClick={hide} />
                    {child}
                    <eventbox expand onClick={hide} />
                </box>
                <eventbox widthRequest={width(w => w / 2)} expand onClick={hide} />
            </box>
        </window>
    )
}
