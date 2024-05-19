import gi from "node-gtk"
import { RequestHandler, Config, runJS } from "../src/application.js"
const Astal = gi.require("Astal", "0.1")

class AstalJS extends Astal.Application {
    static GTypeName = "AstalJS"
    static { gi.registerClass(this) }

    eval = runJS
    requestHandler?: RequestHandler

    vfunc_response(msg: string, conn: any): void {
        if (typeof this.requestHandler === "function") {
            this.requestHandler(msg, response => {
                Astal.writeSock(conn, response, (_, res) =>
                    Astal.writeSockFinish(res),
                )
            })
        } else {
            // @ts-expect-error missing type
            super.vfunc_response(msg, conn)
        }
    }

    start(
        { requestHandler, css, ...cfg }: Omit<Config, "hold"> = {},
        callback?: (args: string[]) => any,
    ) {
        Object.assign(this, cfg)

        this.requestHandler = requestHandler
        this.on("activate", () => {
            callback?.(process.argv)
        })

        if (!this.acquireSocket()) {
            console.error(`Astal instance "${this.instanceName}" already running`)
            process.exit()
        }

        if (css)
            this.applyCss(css, false)

        // FIXME: promises never resolve
        // https://github.com/romgrk/node-gtk/issues/121
        // https://gitlab.gnome.org/GNOME/gjs/-/issues/468
        App.run([])
    }
}

export const App = new AstalJS
