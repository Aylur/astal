# Supported Languages

## JavaScript

The main intended usage of Astal is in TypeScript with [AGS](/guide/ags/first-widgets).
It supports JSX and has a state management solution similar to web frameworks.
Only a minimal knowledge of JavaScript's syntax is needed to get started.

:::info
The runtime is [GJS](https://gitlab.gnome.org/GNOME/gjs) and **not** nodejs
:::

Examples:

- [Simple Bar](https://github.com/Aylur/astal/tree/main/examples/js/simple-bar)
![simple-bar](https://github.com/user-attachments/assets/a306c864-56b7-44c4-8820-81f424f32b9b)

## Lua

Similar to how there is a [TypeScript](https://github.com/Aylur/astal/tree/main/core/gjs) lib for GJS, there is also an accompanying library for [Lua](https://github.com/Aylur/astal/tree/main/core/lua).
<!--TODO: open issue and link performance issue-->
Unfortunately, I have encountered very heavy [performance issues](https://github.com/aylur/astal) with [lgi](https://github.com/lgi-devs/lgi),
and so currently I don't recommend using Lua for full desktop shells, but only for static
components that don't render child nodes dynamically, bars and panels for example.

Examples:

- [Simple Bar](https://github.com/Aylur/astal/tree/main/examples/lua/simple-bar)
![simple-bar](https://github.com/user-attachments/assets/a306c864-56b7-44c4-8820-81f424f32b9b)

## Python

There was a WIP [library for python](https://github.com/aylur/astal/tree/feat/python), to make it behave similar to the above two
but I don't plan on finishing it, because I'm not a fan of python.
If you are interested in picking it up, feel free to open a PR.
Nonetheless you can still use python the OOP way [pygobject](https://pygobject.gnome.org/tutorials/gobject/subclassing.html) intended it.

Examples:

- [Simple Bar](https://github.com/Aylur/astal/tree/main/examples/py/simple-bar)
![simple-bar](https://github.com/user-attachments/assets/a306c864-56b7-44c4-8820-81f424f32b9b)

## Vala

Vala is a language that simply put uses C# syntax and compiles to C.
It is the language most of Astal is written in. I would still recommend
using TypeScript or Lua over Vala as they don't need a build step.

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
