/* eslint-disable max-len */
import Gtk from "gi://Gtk"
import Astal from "gi://Astal"
import { kebabify } from "../src/binding.js"
import proxy, { type ConstructProps, type Widget } from "../src/astalify.js"

const proxify = proxy(Gtk,
    prop => `set_${kebabify(prop).replaceAll("-", "_")}`,
    {
        cssGetter: Astal.widget_get_css,
        cssSetter: Astal.widget_set_css,
        classGetter: Astal.widget_get_class_names,
        classSetter: Astal.widget_set_class_names,
        cursorGetter: Astal.widget_get_cursor,
        cursorSetter: Astal.widget_set_cursor,
    })

export function astalify<
    C extends typeof Gtk.Widget,
    P extends Record<string, any>,
    N extends string = "Widget",
>(klass: C) {
    // eslint-disable-next-line @typescript-eslint/no-unused-vars
    type Astal<N> = Omit<C, "new"> & {
        new(props?: P, ...children: Gtk.Widget[]): Widget<C>
        (props?: P, ...children: Gtk.Widget[]): Widget<C>
    }

    return proxify(klass) as unknown as Astal<N>
}

// Label
export const Label = astalify<typeof Gtk.Label, LabelProps, "Label">(Gtk.Label)
export type LabelProps = ConstructProps<typeof Gtk.Label, Gtk.Label.ConstructorProperties>

// Icon
export const Icon = astalify<typeof Astal.Icon, IconProps, "Icon">(Astal.Icon)
export type IconProps = ConstructProps<typeof Astal.Icon, Astal.Icon.ConstructorProperties>

// Button
export const Button = astalify<typeof Astal.Button, ButtonProps, "Button">(Astal.Button)
export type ButtonProps = ConstructProps<typeof Astal.Button, Astal.Button.ConstructorProperties, {
    onClicked: (self: Widget<typeof Astal.Button>) => void
}>

// Window
export const Window = astalify<typeof Astal.Window, WindowProps, "Window">(Astal.Window)
export type WindowProps = ConstructProps<typeof Astal.Window, Astal.Window.ConstructorProperties>

// Box
export const Box = astalify<typeof Astal.Box, BoxProps, "Box">(Astal.Box)
export type BoxProps = ConstructProps<typeof Astal.Box, Astal.Box.ConstructorProperties>

// CenterBox
export const CenterBox = astalify<typeof Astal.CenterBox, CenterBoxProps, "CenterBox">(Astal.CenterBox)
export type CenterBoxProps = ConstructProps<typeof Astal.CenterBox, Astal.CenterBox.ConstructorProperties>

// EventBox
export const EventBox = astalify<typeof Astal.EventBox, EventBoxProps, "EventBox">(Astal.EventBox)
export type EventBoxProps = ConstructProps<typeof Astal.EventBox, Astal.EventBox.ConstructorProperties>
