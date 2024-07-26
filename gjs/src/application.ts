import { Astal, GObject, Gio, GLib } from "./imports.js"

type RequestHandler = {
    (request: string, res: (response: any) => void): void
}

type Config = Partial<{
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

// @ts-expect-error missing types
// https://github.com/gjsify/ts-for-gir/issues/164
import { setConsoleLogDomain } from "console"
import { exit, programArgs } from "system"

class AstalJS extends Astal.Application {
    static { GObject.registerClass(this) }

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
                Astal.write_sock(conn, String(response), (_, res) =>
                    Astal.write_sock_finish(res),
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

    start({ requestHandler, css, hold, main, client, ...cfg }: Config = {}) {
        client ??= () => {
            print(`Astal instance "${this.instanceName}" already running`)
            exit(1)
        }

        Object.assign(this, cfg)
        setConsoleLogDomain(this.instanceName)

        this.requestHandler = requestHandler
        this.connect("activate", () => {
            const path: string[] = import.meta.url.split("/").slice(3)
            const file = path.at(-1)!.replace(".js", ".css")
            const css = `/${path.slice(0, -1).join("/")}/${file}`
            if (file.endsWith(".css") && GLib.file_test(css, GLib.FileTest.EXISTS))
                this.apply_css(css, false)

            main?.(...programArgs)
        })

        if (!this.acquire_socket())
            return client(msg => this.message(msg)!, ...programArgs)

        if (css)
            this.apply_css(css, false)

        hold ??= true
        if (hold)
            this.hold()

        this.runAsync([])
    }
}

export default new AstalJS()
