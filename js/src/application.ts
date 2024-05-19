export type RequestHandler = {
    (request: string, res: (response: string) => void): void
}

export type Config = Partial<{
    instanceName: string
    gtkTheme: string
    iconTheme: string
    cursorTheme: string
    css: string
    requestHandler: RequestHandler
    hold: boolean
}>

export function runJS(body: string): Promise<any> {
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
