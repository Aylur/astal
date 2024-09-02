---
title: Introduction
description: What is Astal?
sidebar:
    order: 0
---

## What is Astal?

Astal (_meaning "desk"_) is a bundle of libraries built using [GLib](https://docs.gtk.org/glib/) in Vala and C.
The core library [libastal](/astal/reference) has some Gtk widgets that come packaged,
the most important one is the [Window](/astal/reference/class.Window.html) which is the main toplevel component using [gtk-layer-shell](https://github.com/wmww/gtk-layer-shell).
This is what allows us to use Gtk as shell components on Wayland.
libastal also comes with some utility functions such as running external processes,
reading, writing and monitoring files.

## Why Astal?

What makes Astal convenient to use is not the core library, as it could easily be replaced
by the standard library of any of your favorite language that has bindings to Gtk, it is the
accompanying libraries (_formerly known as "services" in AGS_)

Have you ever wanted to write a custom bar, custom notification popups
or an applauncher, but gave up because writing a workspace widget,
implementing the notification daemon or handling a search filter was too much of a hassle?

Astal libraries have you [covered](/astal/libraries/overview), you don't have to worry about these,
you just define the layout, style it with CSS and that's it.
