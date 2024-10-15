import IO from "gi://AstalIO"
import GObject from "gi://GObject"
import Astal from "gi://Astal?version=3.0"
import Gio from "gi://Gio?version=2.0"
import Gtk from "gi://Gtk?version=3.0"

Gtk.init(null)

type RequestHandler = {
    (request: string, res: (response: any) => void): void
}

type Config = Partial<{
    icons: string
    instanceName: string
    gtkTheme: string
    iconTheme: string
    cursorTheme: string
    css: string
    requestHandler: RequestHandler
    main(...args: string[]): void
    client(message: (msg: string) => string, ...args: string[]): void
    hold: boolean
}>

import { setConsoleLogDomain } from "console"
import { exit, programArgs } from "system"

export default new (class AstalJS extends Astal.Application {
    static { GObject.registerClass({ GTypeName: "AstalJS" }, this) }

    eval(body: string): Promise<any> {
        return new Promise((res, rej) => {
            try {
                const fn = Function(`return (async function() {
                    ${body.includes(";") ? body : `return ${body};`}
                })`)
                fn()()
                    .then(res)
                    .catch(rej)
            }
            catch (error) {
                rej(error)
            }
        })
    }

    requestHandler?: RequestHandler

    vfunc_request(msg: string, conn: Gio.SocketConnection): void {
        if (typeof this.requestHandler === "function") {
            this.requestHandler(msg, (response) => {
                IO.write_sock(conn, String(response), (_, res) =>
                    IO.write_sock_finish(res),
                )
            })
        }
        else {
            super.vfunc_request(msg, conn)
        }
    }

    apply_css(style: string, reset = false) {
        super.apply_css(style, reset)
    }

    quit(code?: number): void {
        super.quit()
        exit(code ?? 0)
    }

    start({ requestHandler, css, hold, main, client, icons, ...cfg }: Config = {}) {
        client ??= () => {
            print(`Astal instance "${this.instanceName}" already running`)
            exit(1)
        }

        Object.assign(this, cfg)
        setConsoleLogDomain(this.instanceName)

        this.requestHandler = requestHandler
        this.connect("activate", () => {
            main?.(...programArgs)
        })

        try {
            this.acquire_socket()
        }
        catch (error) {
            return client(msg => IO.send_message(this.instanceName, msg)!, ...programArgs)
        }

        if (css)
            this.apply_css(css, false)

        if (icons)
            this.add_icons(icons)

        hold ??= true
        if (hold)
            this.hold()

        this.runAsync([])
    }
})
