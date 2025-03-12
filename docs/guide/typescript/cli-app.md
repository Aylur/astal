# CLI and App

`App` is a singleton **instance** of an [Astal.Application](https://aylur.github.io/libastal/astal3/class.Application.html).

Depending on gtk version import paths will differ

```ts
import { App } from "astal/gtk3"
import { App } from "astal/gtk4"
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

## Instance identifier

You can run multiple instances by defining a unique instance name.

```ts
App.start({
    instanceName: "my-instance", // defaults to "astal"
    main() { },
})
```

## Messaging from CLI

If you want to interact with an instance from the CLI,
you can do so by sending a message.

```ts
App.start({
    requestHandler(request: string, res: (response: any) => void) {
        if (request == "say hi") {
            return res("hi cli")
        }
        res("unknown command")
    },
    main() { },
})
```

```sh
astal say hi
# hi cli
```

If you want to run arbitrary JavaScript from CLI, you can use `App.eval`
which will evaluate the passed string as the body of an `async` function.

```ts
App.start({
    main() {},
    requestHandler(js, res) {
        App.eval(js).then(res).catch(res)
    },
})
```

If the string does not contain a semicolon, a single expression is assumed and returned implicity.

```sh
astal "'hello'"
# hello
```

If the string contains a semicolon, you have to return explicitly

```sh
astal "'hello';"
# undefined

astal "return 'hello';"
# hello
```

## Toggling Windows by their name

In order for Astal to know about your windows, you have to register them.
You can do this by specifying a **unique** `name` and calling `App.add_window`

```tsx {4}
import { App } from "astal/gtk3"

function Bar() {
    return <window name="Bar" setup={self => App.add_window(self)}>
        <box />
    </window>
}
```

You can also invoke `App.add_window` by simply passing the `App` to the `application` prop.

```tsx {4}
import { App } from "astal/gtk3"

function Bar() {
    return <window name="Bar" application={App}>
        <box />
    </window>
}
```

:::warning
When assigning the `application` prop make sure `name` comes before.
Props are set sequentially and if name is applied after application it won't work.
:::

```sh [astal]
astal -t Bar
```

## Client

The first time you execute your script the `main` function gets called.
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
        print(res)
    },

    // this runs in the main instance
    requestHandler(request: string, res: (response: any) => void) {
        res("response from main")
    },
})
```

:::
