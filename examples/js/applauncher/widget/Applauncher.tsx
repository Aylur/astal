import Apps from "gi://AstalApps"
import { App, Astal, Gdk, Gtk } from "astal/gtk3"
import { Variable } from "astal"

const MAX_ITEMS = 8

function AppButton({ app }: { app: Apps.Application }) {
    return <button className="AppButton" onClicked={() => app.launch()}>
        <box>
            <icon icon={app.iconName} />
            <box valign={Gtk.Align.CENTER} vertical>
                <label
                    className="name"
                    truncate
                    xalign={0}
                    label={app.name}
                />
                {app.description && <label
                    className="description"
                    wrap
                    xalign={0}
                    label={app.description}
                />}
            </box>
        </box>
    </button>
}

export default function Applauncher() {
    const apps = new Apps.Apps()
    const list = Variable(apps.get_list().slice(0, MAX_ITEMS))
    const hide = () => App.get_window("launcher")!.hide()

    function search(text: string) {
        list.set(apps.fuzzy_query(text).slice(0, MAX_ITEMS))
    }

    return <window
        name="launcher"
        anchor={Astal.WindowAnchor.TOP | Astal.WindowAnchor.BOTTOM}
        exclusivity={Astal.Exclusivity.IGNORE}
        keymode={Astal.Keymode.ON_DEMAND}
        application={App}
        onShow={() => list.set(apps.get_list().slice(0, MAX_ITEMS))}
        onKeyPressEvent={function (self, event: Gdk.Event) {
            if (event.get_keyval()[1] === Gdk.KEY_Escape)
                self.hide()
        }}>
        <box>
            <eventbox widthRequest={4000} expand onClick={hide} />
            <box hexpand={false} vertical>
                <eventbox heightRequest={100} onClick={hide} />
                <box widthRequest={500} className="Applauncher" vertical>
                    <entry
                        placeholderText="Search"
                        onChanged={({ text }) => search(text)}
                    />
                    <box spacing={6} vertical>
                        {list(list => list.map(app => (
                            <AppButton app={app} />
                        )))}
                    </box>
                    <box visible={list(l => l.length === 0)}>
                        <icon icon="system-search-symbolic" />
                        No match found
                    </box>
                </box>
                <eventbox expand onClick={hide} />
            </box>
            <eventbox widthRequest={4000} expand onClick={hide} />
        </box>
    </window>
}
