# libastal

> [!WARNING]
> WIP: everything is subject to change

The main goal of this project is to further abstract gtk bindings in higher level
languages with custom state management mechanisms, namely in javascript (gjs, node),
lua (lua-lgi) and python (pygobject).

`libastal`, which is the library written in Vala,
comes with a few widgets built on top of gtk3 and
tools to execute external binaries and store their output.
It also comes with a builtin cli client to send messages to the running
processes through a socket.

## Developing

first install libastal or enter nix shell

```bash
bash meson-install.sh # non nix
nix develop .#astal # nix
```

python and lua should be stright forward, just run the interpreter

for javascript do

```bash
cd js
npm i
npm run types
npm run build -- --watch
```

## Gtk abstractions

`Variable` and `Binding` objects and a function that turns widget constructors
into ones that can take `Binding` objects as parameters are added on top
of gtk bindings. This mechanism takes care of all state management one would need.

This works the same in js/lua/python, but demonstrated in js

```javascript
// this example will work with Variable<string>
// but it can take any type of value
const v = Variable("value")
    .poll(1000, "some-executable on $PATH")
    .poll(1000, ["some-executable", "with", "args"])
    .poll(1000, () => "some-function")
    .watch("some-executable")
    .watch(["some-executable", "with", "args"])
    .observe(someGObject, "signal", (...args) => "some output")
    .observe([[gobj1, "signal"], [gobj2, "signal"]], (...args) => "some output")
    .onError(console.error) // when the script fails
    .onDropped(() => "clean-up") // cleanup resources if needed on drop() or GC

Button({
    label: bind(v),
    label: bind(v).as(v => "transformed"),
    label: v(t => "transformed"), // shorthand for the above

    // in ags we have Service.bind("prop")
    // here we will do this, since gobject implementations
    // will come from Vala code and not js
    label: bind(anyGObject, "one-of-its-prop").as(prop => "transformed"),

    // event handlers
    on_signalname(self, ...args) { print(self, args) },

    // setup prop is still here, but should be rarely needed
    setup(self) {
        self.hook(v, (self) => print(self))
        self.hook(gobject, "signal", (self) => print(self))
    }
})

// some additional Variable and Binding methods
v.stop_poll()
v.start_poll()

v.stop_watch()
v.start_watch()

v.get()
v.set("new-value")

const unsub = v.subscribe(value => console.log(value))
unsub() // to unsubscribe

const b = bind(v)
b.get()
// note that its value cannot be set through a Binding
// if you want to, you are doing something wrong

// same subscribe mechanism
const unsub = b.subscribe(value => console.log(value))
unsub()

const derived = Variable.derive([v, b], (vval, bval) => {
    return "can take a list of Variable | Binding"
})

v.drop() // dispose when no longer needed

// handle cli client
App.start({
    instanceName: "my-instance",
    responseHandler(msg, response) {
        console.log("message from cli", msg)
        response("hi")
    }
})
```

after `App.start` is called, it will open a socket, which can be used
with the cli client that comes with libastal

```bash
astal --instance-name my-instance "message was sent from cli"
```

## Lower level languages

As said before, the main goal is to make js/lua/python DX better, but libastal
can be used in **any** language that has bindings for glib/gtk.
`Binding` is not implemented in Vala, but in each language, because
they are language specific, and it doesn't make much sense for lower
level languages as they usually don't have a way to declaratively build
layouts. Subclassed widgets and `Variable` can still be used, but they will
need to be hooked **imperatively**. For languages like rust/go/c++
you will mostly benefit from the other libraries (called `Service` in ags).
I can also recommend using [blueprint](https://jwestman.pages.gitlab.gnome.org/blueprint-compiler/)
which lets you define layouts declaratively and hook functionality in your
preferred language.

I am open to add support for any other language if it makes sense,
but if using blueprint makes more sense, I would rather maintain
templates and examples instead to get started with development.

## Goals

- libastal
  - Variables
    - [x] poll (interval, string)
    - [x] pollv (interval, string[])
    - [x] pollfn (interval, closure)
    - [x] watch (string)
    - [x] watchv (string[])
    - ~~[ ] observe (object, signal, closure)~~
  - Time
    - [x] interval
    - [x] timeout
    - [x] idle
    - [x] now signal
  - Process
    - [x] exec: string, error as Error
    - [x] execAsync: proc, stdout, stderr signal
    - [x] subprocess: proc, stdout, stderr signal
  - app instance with a socket: Application
    - [x] gtk settings as props
    - [x] window getters
    - [x] include cli client
  - few additional widgets
    - [x] window widget with gtk-layer-shell
    - [x] box with children prop
    - [x] button with abstract signals for button-event
    - [ ] ?custom calendar like gtk4
    - [x] centerbox
    - [ ] circularprogress
    - [x] eventbox
    - [x] icon
    - [ ] overlay
    - [ ] scrollable/viewport
    - [ ] slider
    - [ ] stack, shown, children setter
  - widgets with no additional behaviour only for the sake of it
    - [ ] ?drawingarea
    - [ ] ?entry
    - [ ] ?fixed
    - [ ] ?flowbox
    - [ ] ?label
    - [ ] ?levelbar
    - [ ] ?revealer
    - [ ] ?switch
  - widget prop setters
    - [x] css
    - [x] class-names
    - [x] cursor
    - [ ] click-through

- language bindings
  - Binding for Variable and any GObject `bind(gobject, property).as(transform)`
  - .hook() for widgets
  - setup prop for widgets
  - constructor overrides to take bindings
  - override default `visible` for widgets to true
  - wrap Variable in native object to make sure no GValue crashes
  - Variable.observe for signals
  - Variable.derive that takes either Variables or Bindings

## Help needed

- node-gtk promise issue
- python types

## TODO

- docs
- ~~consider moving each language into separate repo~~
- support jsx
  - [x] gjs
- port services from ags into Vala
  - [x] [applications](https://github.com/astal-sh/apps)
  - [ ] audio
  - [ ] bluetooth
  - [ ] greetd
  - [ ] hyprland ipc client
  - [ ] mpris
  - [ ] network
  - [ ] sway ipc client
  - [x] [notifications](https://github.com/astal-sh/notifd)
  - [ ] upower (battery, powerprofiles)
  - [x] [systemtray](https://github.com/astal-sh/tray)
