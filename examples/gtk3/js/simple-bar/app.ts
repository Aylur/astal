import { App } from "astal/gtk3";
import style from "./style.scss";
import Bar from "./widget/Bar";
import NotificationPopups from "./notifications/NotificationPopups";

App.start({
  css: style,
  instanceName: "js",
  requestHandler(request, res) {
    print(request);
    res("ok");
  },
  main: () => {
    App.get_monitors().map(Bar);
    App.get_monitors().map(NotificationPopups);
  },
});
