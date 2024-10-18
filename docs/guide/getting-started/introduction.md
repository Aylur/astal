# Introduction

## What is Astal?

Astal (_meaning "desk"_) is a suite of libraries in Vala and C.
The core library [astal3](https://aylur.github.io/libastal/astal3) and
[astal4](https://aylur.github.io/libastal/astal4) (not yet available)
has some Gtk widgets that come packaged,
the most important one being the [Window](https://aylur.github.io/libastal/astal3/class.Window.html) which is the main toplevel component using [gtk-layer-shell](https://github.com/wmww/gtk-layer-shell).
This is what allows us to use Gtk as shell components on Wayland.
The other part of the core library [astal-io](https://aylur.github.io/libastal/astal-io)
which contains some utility GLib shortcut for running external processes,
reading, writing and monitoring files, timeout and interval functions.

## Why Astal?

What makes Astal convenient to use is not the core libraries, as they can easily be replaced
by the standard library of any of your favorite language that has bindings to Gtk, it is the
accompanying libraries (_formerly known as "services" in AGS_).

Have you ever wanted to write a custom bar, custom notification popups
or an applauncher, but gave up because writing a workspace widget,
implementing the notification daemon or handling a search filter was too much of a hassle?

Astal libraries have you [covered](../libraries/references#astal-libraries), you don't have to worry about these,
you just define the layout, style it with CSS and that's it.
