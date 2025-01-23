import { App, Astal, Gtk } from "astal/gtk3"
import { Variable } from "astal"
import Popup from "./Popover"
const { TOP, RIGHT, LEFT } = Astal.WindowAnchor

App.start({
    instanceName: "popup-example",
    css: `
        .Popup>box {
            background-color: @theme_bg_color;
            box-shadow: 2px 3px 7px 0 rgba(0,0,0,0.4);
            border-radius: 12px;
            padding: 12px;
        }
    `,
    main() {
        const visible = Variable(false);

        <Popup
            className="Popup"
            onClose={() => visible.set(false)}
            visible={visible()}
            marginTop={36}
            marginRight={60}
            valign={Gtk.Align.START}
            halign={Gtk.Align.END}
        >
            <button onClicked={() => visible.set(false)}>
                Click me to close the popup
            </button>
        </Popup>

        return (
            <window
                anchor={TOP | LEFT | RIGHT}
                exclusivity={Astal.Exclusivity.EXCLUSIVE}
            >
                <button onClicked={() => visible.set(true)} halign={Gtk.Align.END}>
                    Click to open popup
                </button>
            </window>
        )
    },
})
