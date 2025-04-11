import { App, Astal, Gtk } from "astal/gtk3"
import { Variable } from "astal"
import Popover from "./Popover"
import Popover2 from "./Popover2"
const { TOP, RIGHT, LEFT } = Astal.WindowAnchor

const lorem = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Aenean quis semper risus."

App.start({
    instanceName: "popup-example",
    css: `
        .popup {
            background-color: @theme_bg_color;
            box-shadow: 2px 3px 7px 0 rgba(0,0,0,0.4);
            border-radius: 12px;
            padding: 12px;
        }
    `,
    main() {
        const visible1 = Variable(false);
        const visible2 = Variable(false);

        const _popover1 = <Popover
            className="Popup"
            onClose={() => visible1.set(false)}
            visible={visible1()}
            marginTop={36}
            marginRight={60}
            valign={Gtk.Align.START}
            halign={Gtk.Align.END}
        >
            <box className="popup" vertical>
                {/* maxWidthChars is needed to make wrap work */}
                <label label={lorem} wrap maxWidthChars={8} />
                <button onClicked={() => visible1.set(false)}>
                    Click me to close the popup
                </button>
            </box>
        </Popover>


        const _popover2 = <Popover2
            className="Popup"
            onClose={() => visible2.set(false)}
            visible={visible2()}
        >
            <box className="popup" vertical>
                {/* maxWidthChars is needed, wrap will work as intended */}
                <label label={lorem} wrap />
                <button onClicked={() => visible2.set(false)}>
                    Click me to close the popup
                </button>
            </box>
        </Popover2>

        return (
            <window
                anchor={TOP | LEFT | RIGHT}
                exclusivity={Astal.Exclusivity.EXCLUSIVE}
            >
                <box halign={Gtk.Align.END}>
                    <button onClicked={() => visible2.set(true)} halign={Gtk.Align.END}>
                        Click to open popover2
                    </button>
                    <button onClicked={() => visible1.set(true)} halign={Gtk.Align.END}>
                        Click to open popover
                    </button>
                </box>
            </window>
        )
    },
})
