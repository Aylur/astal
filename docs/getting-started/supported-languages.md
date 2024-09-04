# Supported Languages

## JavaScript

The main intended usage of Astal is in TypeScript with [AGS](/ags/first-widgets).
It supports JSX and has a state management solution similar to web frameworks.
Only a minimal knowledge of JavaScript's syntax is needed to get started.

:::info
The runtime is [GJS](https://gitlab.gnome.org/GNOME/gjs) and **not** nodejs
:::

Examples:

- TODO

## Lua

Similar to how there is a [TypeScript](https://github.com/Aylur/astal/tree/main/core/gjs) lib for GJS, there is also an accompanying library for [Lua](https://github.com/Aylur/astal/tree/main/core/lua).
<!--TODO: open issue and link performance issue-->
Unfortunately, I have encountered very heavy [performance issues]() with [lgi](https://github.com/lgi-devs/lgi),
and so currently I don't recommend using Lua for full desktop shells, but only for static
components that don't render child nodes dynamically, bars and panels for example.

Examples:

- TODO

## Python

<!--TODO: move python PR to monorepo and link-->
There is a WIP [library for python](), to make it behave similar to the above two
but I don't plan on finishing it, because I'm not a fan of python.
If you are interested in picking it up, feel free to open a PR.
Nonetheless you can still use python the OOP way [pygobject](https://pygobject.gnome.org/tutorials/gobject/subclassing.html) intended it.

Examples:

- TODO

## Vala

Vala is a language that simply put uses C# syntax and compiles to C.
It is the language most of Astal is written in.

Examples:

- TODO

## C

I don't recommend using C as it requires quite a lot of boilerplate.

Examples:

- TODO

## Other languages

There a few more that supports gobject-introspection, most notably Haskell, Rust and C++.
If you are interested and feel like contributing, PRs are welcome for bindings, and examples.
