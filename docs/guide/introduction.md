# Introduction

Astal is a collection of libraries written in Vala and C, designed to serve as
the foundation for both lightweight widgets and fully-featured desktop shells.
It handles the backend logic, so you can focus entirely on building the
frontend.

:::details History

This project began as a JavaScript-only solution under the AGS project, known as
AGS v1. While experimenting with Lua and Python ports, the idea emerged to move
most of the codebase to Vala and use GObject Introspection to share components
across different language ports. This marked the birth of Astal and the release
of AGS v2.

However, I mistakenly included frontend abstractions in Astal, which turned out
to be a misstep. Over time, I also lost interest in Python and Lua. Eventually,
I realized not only that, but also that the initial JSX implementation wasn’t
well-suited for GTK4.

After going through several iterations and decoupling the project from Astal,
Gnim was created, leading to the emergence of AGS v3.

:::

## About this Documentation

The goal of this documentation is to help you get started with writing a GTK
application. It provides code snippets, templates, examples, and a list of
references to help you learn more about GTK and the Astal libraries.

## Supported Languages

For a list of languages that support Gtk see
[this wikipedia article](https://en.wikipedia.org/wiki/List_of_language_bindings_for_GTK).

Interpreted languages like JavaScript and Python are supported out of the box
while some compiled languages might need extra work to make use of Astal
libraries.

Currently we have [examples](https://github.com/Aylur/astal/tree/main/examples)
for

- Vala
- JavaScript (GJS)
- Python (PyGObject)

## Tools for Frontend

While this documentation shows you how to start building GTK applications, you
might also be interested in these projects below. If you decide to use them, be
sure to read their documentation first, then return here to pick up libraries
you need.

### AGS

[AGS](https://aylur.github.io/ags/) is a scaffolding tool for Astal + Gnim
projects written in TypeScript. [Gnim](https://github.com/aylur/gnim) is a
library which brings JSX to GJS.

### Eww

[Eww](https://github.com/elkowar/eww) is a tool that simplifies GTK usage by
exposing only a limited set of widgets and allowing you to define UIs using a
Lisp-inspired DSL. Many Astal libraries include CLI tools that output JSON,
which can be easily integrated into Eww.
