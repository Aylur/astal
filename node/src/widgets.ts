/* eslint-disable max-len */
import { Astal, Gtk } from "./imports.js"
import astalify, { type ConstructProps } from "./astalify.js"
import type GtkT from "@girs/node-gtk-3.0/node-gtk-3.0"
import type AstalT from "@girs/node-astal-0.1/node-astal-0.1"

export { astalify, ConstructProps }

// Label
export const Label = astalify<typeof Gtk.Label, LabelProps, "Label">(Gtk.Label)
export type LabelProps = ConstructProps<typeof Gtk.Label, GtkT.Label.ConstructorProperties>

// Icon
export const Icon = astalify<typeof Astal.Icon, IconProps, "Icon">(Astal.Icon)
export type IconProps = ConstructProps<typeof Astal.Icon, AstalT.Icon.ConstructorProperties>

// Button
export const Button = astalify<typeof Astal.Button, ButtonProps, "Button">(Astal.Button)
export type ButtonProps = ConstructProps<typeof Astal.Button, AstalT.Button.ConstructorProperties, {
    onClicked: []
}>

// Window
export const Window = astalify<typeof Astal.Window, WindowProps, "Window">(Astal.Window)
export type WindowProps = ConstructProps<typeof Astal.Window, AstalT.Window.ConstructorProperties>

// Box
export const Box = astalify<typeof Astal.Box, BoxProps, "Box">(Astal.Box)
export type BoxProps = ConstructProps<typeof Astal.Box, AstalT.Box.ConstructorProperties>

// CenterBox
export const CenterBox = astalify<typeof Astal.CenterBox, CenterBoxProps, "CenterBox">(Astal.CenterBox)
export type CenterBoxProps = ConstructProps<typeof Astal.CenterBox, AstalT.CenterBox.ConstructorProperties>

// EventBox
export const EventBox = astalify<typeof Astal.EventBox, EventBoxProps, "EventBox">(Astal.EventBox)
export type EventBoxProps = ConstructProps<typeof Astal.EventBox, AstalT.EventBox.ConstructorProperties>
