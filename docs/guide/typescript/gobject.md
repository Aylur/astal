# Subclassing GObject.Object

Astal provides decorator functions that make it easy to subclass gobjects.

## Example Usage

```ts
import GObject, { register, property } from "astal/gobject"

@register()
class MyObj extends GObject.Object {
    @property(String)
    declare myProp: string

    @signal(String, Number)
    declare mySignal: (a: string, b: number) => void
}
```

## Property decorator

```ts
type PropertyDeclaration =
    | GObject.ParamSpec
    | { $gtype: GObject.GType }

function property(declaration: PropertyDeclaration)
```

The `property` decorator can take any class that has a registered GType.
This includes the globally available `String`, `Number`, `Boolean` and `Object`
javascript constructors. They are mapped to their relative `GObject.ParamSpec`.

The property decorator can be applied in the following ways:

1. On a property declaration

```ts {3,4}
@register()
class MyObj extends GObject.Object {
    @property(String)
    declare myProp: string
}
```

This will create a getter and setter for the property and will also
emit the notify signal when the value is set to a new value.

:::info
The `declare` keyword is required so that the property declaration
is not transpiled into JavaScript, otherwise the initial value of the
property would be `undefined`.
:::

:::warning
The value is checked by reference, this is important if your
property is an object type.

```ts
const dict = obj.prop
dict["key"] = 0
obj.prop = dict // This will not emit notify::prop // [!code error]
obj.prop = { ...dict } // This will emit notify::prop
```

:::

If you want to set a custom default value, do so in the constructor of your class.

```ts {7}
@register()
class MyObj extends GObject.Object {
    @property(String)
    declare myProp: string

    constructor() {
        super({ myProp: "default-value" } as any)
    }
}
```

2. On a getter

```ts {3,4}
@register()
class MyObj extends GObject.Object {
    @property(String)
    get myProp () {
        return "value"
    }
}
```

This will create a read-only property.

3. On a getter and setter

```ts {5,6,10}
@register()
class MyObj extends GObject.Object {
    declare private _prop: string

    @property(String)
    get myProp () {
        return "value"
    }

    set myProp (v: string) {
        if (v !== this._prop) {
            this._prop = v
            this.notify("my-prop")
        }
    }
}
```

This will create a read-write property.

:::info
When defining getter/setters for the property, notify signal emission has to be done explicitly.
:::

## Signal decorator

```ts
function signal(...params: Array<{ $gtype: GObject.GType })

function signal(declaration?: SignalDeclaration) // Object you would pass to GObject.registerClass
```

You can apply the signal decorator to either a property declaration or a method.

```ts {3,4,6,7}
@register()
class MyObj extends GObject.Object {
    @signal(String, String)
    declare mySig: (a: String, b: String) => void

    @signal(String, String)
    mySig(a: string, b: string) {
        // default signal handler
    }
}
```

You can emit the signal by calling the signal method or using `emit`.

```ts
const obj = new MyObj()
obj.connect("my-sig", (obj, a: string, b: string) => {})

obj.mySig("a", "b")
obj.emit("my-sig", "a", "b")
```

## Register decorator

Every GObject subclass has to be registered. You can pass the same options
to this decorator as you would to `GObject.registerClass`

```ts
@register({ GTypeName: "MyObj" })
class MyObj extends GObject.Object {
}
```
