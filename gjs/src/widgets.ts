/* eslint-disable max-len */
import { Astal, Gtk } from "./imports.js"
import astalify, { type ConstructProps } from "./astalify.js"

export { astalify, ConstructProps }

// Label
export const Label = astalify<typeof Gtk.Label, LabelProps, "Label">(Gtk.Label)
export type LabelProps = ConstructProps<typeof Gtk.Label, Gtk.Label.ConstructorProperties>

// Icon
export const Icon = astalify<typeof Astal.Icon, IconProps, "Icon">(Astal.Icon)
export type IconProps = ConstructProps<typeof Astal.Icon, Astal.Icon.ConstructorProperties>

// Button
export const Button = astalify<typeof Astal.Button, ButtonProps, "Button">(Astal.Button)
export type ButtonProps = ConstructProps<typeof Astal.Button, Astal.Button.ConstructorProperties, {
    onClicked: []
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
