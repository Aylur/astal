# Supported Languages

There are currently two languages that have an additional
Astal package: Lua and Gjs. Their purpose is to abstract away
Gtk by implementing a state management and UI declaring solution.

## JavaScript

The main intended usage of Astal is in TypeScript+JSX.
It is recommended to use [AGS](/guide/typescript/first-widgets) to scaffold and run projects in TypeScript.
However, if you are familiar with JavaScript's tooling
ecosystem you can also setup an environment yourself.
Only a minimal knowledge of JavaScript's syntax is needed to get started.

:::info
The runtime is [GJS](https://gitlab.gnome.org/GNOME/gjs) and **not** nodejs
:::

Examples:

- [Simple Bar](https://github.com/Aylur/astal/tree/main/examples/js/simple-bar)
![simple-bar](https://github.com/user-attachments/assets/a306c864-56b7-44c4-8820-81f424f32b9b)

## Lua

Lua is well-supported, but I would still recommend TypeScript, as Lua lacks a type system, which in turn limits editor support.

Examples:

- [Simple Bar](https://github.com/Aylur/astal/tree/main/examples/lua/simple-bar)
![simple-bar](https://github.com/user-attachments/assets/a306c864-56b7-44c4-8820-81f424f32b9b)

## Python

There is a WIP [package for python](https://github.com/aylur/astal/tree/feat/python),
to bring declaritivity to Python similar to the above two languages.
However, you can still use python the OOP way [pygobject](https://pygobject.gnome.org/tutorials/gobject/subclassing.html) intended it in the meantime.

Examples:

- [Simple Bar](https://github.com/Aylur/astal/tree/main/examples/py/simple-bar)
![simple-bar](https://github.com/user-attachments/assets/a306c864-56b7-44c4-8820-81f424f32b9b)

## Vala

Vala is a language that simply put uses C# syntax and compiles to C.
It is the language most of Astal is written in. I would still recommend
using TypeScript or Lua over Vala as they are simpler to work with.

Examples:

- [Simple Bar](https://github.com/Aylur/astal/tree/main/examples/vala/simple-bar)
![simple-bar](https://github.com/user-attachments/assets/a306c864-56b7-44c4-8820-81f424f32b9b)

## C

I don't recommend using C as it requires quite a lot of boilerplate, both for
build step and code.

Examples:

- TODO

## Other languages

There a few more that supports gobject-introspection, most notably Haskell, Rust and C++.
If you are interested and feel like contributing, PRs are welcome for bindings, and examples.
