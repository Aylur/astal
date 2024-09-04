# CLI and App

`App` is a singleton **instance** of [Astal.Application](https://aylur.github.io/libastal/class.Application.html).

```tsx
import { App } from "astal"
```

## Entry point

:::code-group

```ts [app.ts]
App.start({
    main() {
        // setup anything
        // instantiate widgets
    },
})
```

:::

:::warning
You can not instantiate widgets outside of the main function.
:::

## Instance identifier

You can run multiple instance by defining a unique instance name.

```tsx
App.start({
    instanceName: "my-instance", // defaults to "astal"
    main() {},
})
```

## Messaging from CLI

If you want to interact with an instance from the cli, you can do so by sending a message.

```ts
App.start({
    main() {},
    requestHandler(request: string, res: (response: any) => void) {
        if (request == "say hi") {
            res("hi cli")
        }
        res("unknown command")
    },
})
```

```bash
# ags cli
$ ags -m "say hi"
hi cli

# astal cli
$ astal say hi
hi cli
```

If you want to run arbitrary JavaScript from cli, you can use `App.eval`.
It will evaluate the passed string as the body of an `async` function.

```ts
App.start({
    main() {},
    requestHandler(js: string, res) {
        App.eval(js).then(res).catch(res)
    },
})
```

If the string does not contain a semicolon, a single expression is assumed and returned implicity.

```bash
$ ags -m "'hello'"
hello
```

If the string contains a semicolon, you have to return explicitly

```bash
$ ags -m "'hello';"
undefined

$ ags -m "return 'hello';"
hello
```

## App without AGS

As mentioned before AGS is only a scaffolding tool. You can setup
a dev environment and a bundler yourself. In which case you won't be using
the ags cli to run the bundled scripts. The produced script can run as the main instance
and a "client" instance.

The first time you run your bundled script the `main` function gets executed.
While that instance is running any subsequent execution of the script will call
the `client` function.

:::code-group

```ts [main.ts]
App.start({
    // main instance
    main(...args: Array<string>) {
        print(...args)
    },

    // every subsequent calls
    client(message: (msg: string) => string, ...args: Array<string>) {
        const res = message("you can message the main instance")
        console.log(res)
    },

    // this runs in the main instance
    requestHandler(request: string, res: (response: any) => void) {
        res("response from main")
    },
})
```

:::
