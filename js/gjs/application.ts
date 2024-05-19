import Astal from "gi://Astal"
import GObject from "gi://GObject"
import Gio from "gi://Gio"
import { RequestHandler, Config, runJS } from "../src/application.js"

// @ts-expect-error missing types
// https://github.com/gjsify/ts-for-gir/issues/164
import { setConsoleLogDomain } from "console"
import { exit, programArgs } from "system"

class AstalJS extends Astal.Application {
    static { GObject.registerClass(this) }

    eval = runJS
    requestHandler?: RequestHandler

    vfunc_response(msg: string, conn: Gio.SocketConnection): void {
        if (typeof this.requestHandler === "function") {
            this.requestHandler(msg, response => {
                Astal.write_sock(conn, response, (_, res) =>
                    Astal.write_sock_finish(res),
                )
            })
        } else {
            super.vfunc_response(msg, conn)
        }
    }

    start({ requestHandler, css, hold, ...cfg }: Config = {}, callback?: (args: string[]) => void) {
        Object.assign(this, cfg)
        setConsoleLogDomain(this.instanceName)

        this.requestHandler = requestHandler
        this.connect("activate", () => callback?.(programArgs))
        if (!this.acquire_socket()) {
            print(`Astal instance "${this.instanceName}" already running`)
            exit(1)
        }

        if (css)
            this.apply_css(css, false)

        hold ??= true
        if (hold)
            this.hold()

        this.runAsync([])
    }
}

export const App = new AstalJS
