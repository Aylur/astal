import gi from "node-gtk"
const Astal = gi.require("Astal", "0.1")

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
    hold: boolean
}>

class AstalJS extends Astal.Application {
    static GTypeName = "AstalJS"
    static { gi.registerClass(this) }

    eval(body: string): Promise<any> {
        return new Promise((res, rej) => {
            try {
                const fn = Function(`return (async function() {
                    ${body.includes(";") ? body : `return ${body};`}
                })`)
                fn()()
                    .then(res)
                    .catch(rej)
            } catch (error) {
                rej(error)
            }
        })
    }

    requestHandler?: RequestHandler

    vfunc_request(msg: string, conn: any): void {
        if (typeof this.requestHandler === "function") {
            this.requestHandler(msg, response => {
                Astal.writeSock(conn, String(response), (_, res) =>
                    Astal.writeSockFinish(res),
                )
            })
        } else {
            // @ts-expect-error missing type
            super.vfunc_request(msg, conn)
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
