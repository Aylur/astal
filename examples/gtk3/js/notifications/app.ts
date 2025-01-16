import { App } from "astal/gtk3"
import style from "./style.scss"
import NotificationPopups from "./notifications/NotificationPopups"

App.start({
    instanceName: "notifications",
    css: style,
    main: () => App.get_monitors().map(NotificationPopups),
})
