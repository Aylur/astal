import { App } from "astal/gtk3"
import style from "./style.scss"
import OSD from "./osd/OSD"

App.start({
    instanceName: "osd-example",
    css: style,
    main() {
        App.get_monitors().map(OSD)
    },
})
