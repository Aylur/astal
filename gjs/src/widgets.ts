/* eslint-disable max-len */
import { Astal, Gtk } from "./imports.js"
import astalify, { type ConstructProps, type Widget } from "./astalify.js"

export { astalify, ConstructProps }

// Box
export type Box = Widget<typeof Astal.Box>
export const Box = astalify<typeof Astal.Box, BoxProps, "Box">(Astal.Box)
export type BoxProps = ConstructProps<typeof Astal.Box, Astal.Box.ConstructorProperties>

// Button
export type Button = Widget<typeof Astal.Button>
export const Button = astalify<typeof Astal.Button, ButtonProps, "Button">(Astal.Button)
export type ButtonProps = ConstructProps<typeof Astal.Button, Astal.Button.ConstructorProperties, {
    onClicked: []
    onClick: [event: Astal.ClickEvent]
    onClickRelease: [event: Astal.ClickEvent]
    onHover: [event: Astal.HoverEvent]
    onHoverLost: [event: Astal.HoverEvent]
    onScroll: [event: Astal.ScrollEvent]
}>

// CenterBox
export type CenterBox = Widget<typeof Astal.CenterBox>
export const CenterBox = astalify<typeof Astal.CenterBox, CenterBoxProps, "CenterBox">(Astal.CenterBox)
export type CenterBoxProps = ConstructProps<typeof Astal.CenterBox, Astal.CenterBox.ConstructorProperties>

// TODO: CircularProgress

// DrawingArea
export type DrawingArea = Widget<typeof Gtk.DrawingArea>
export const DrawingArea = astalify<typeof Gtk.DrawingArea, DrawingAreaProps, "DrawingArea">(Gtk.DrawingArea)
export type DrawingAreaProps = ConstructProps<typeof Gtk.DrawingArea, Gtk.DrawingArea.ConstructorProperties, {
    onDraw: [cr: any] // TODO: cairo types
}>

// Entry
export type Entry = Widget<typeof Gtk.Entry>
export const Entry = astalify<typeof Gtk.Entry, EntryProps, "Entry">(Gtk.Entry)
export type EntryProps = ConstructProps<typeof Gtk.Entry, Gtk.Entry.ConstructorProperties, {
    onChanged: []
    onActivate: []
}>

// EventBox
export type EventBox = Widget<typeof Astal.EventBox>
export const EventBox = astalify<typeof Astal.EventBox, EventBoxProps, "EventBox">(Astal.EventBox)
export type EventBoxProps = ConstructProps<typeof Astal.EventBox, Astal.EventBox.ConstructorProperties, {
    onClick: [event: Astal.ClickEvent]
    onClickRelease: [event: Astal.ClickEvent]
    onHover: [event: Astal.HoverEvent]
    onHoverLost: [event: Astal.HoverEvent]
    onScroll: [event: Astal.ScrollEvent]
}>

// TODO: Fixed
// TODO: FlowBox

// Icon
export type Icon = Widget<typeof Astal.Icon>
export const Icon = astalify<typeof Astal.Icon, IconProps, "Icon">(Astal.Icon)
export type IconProps = ConstructProps<typeof Astal.Icon, Astal.Icon.ConstructorProperties>

// Label
export type Label = Widget<typeof Gtk.Label>
export const Label = astalify<typeof Gtk.Label, LabelProps, "Label">(Gtk.Label)
export type LabelProps = ConstructProps<typeof Gtk.Label, Gtk.Label.ConstructorProperties>

// LevelBar
export type LevelBar = Widget<typeof Astal.LevelBar>
export const LevelBar = astalify<typeof Astal.LevelBar, LevelBarProps, "LevelBar">(Astal.LevelBar)
export type LevelBarProps = ConstructProps<typeof Astal.LevelBar, Astal.LevelBar.ConstructorProperties>

// TODO: ListBox

// Overlay
export type Overlay = Widget<typeof Astal.Overlay>
export const Overlay = astalify<typeof Astal.Overlay, OverlayProps, "Overlay">(Astal.Overlay)
export type OverlayProps = ConstructProps<typeof Astal.Overlay, Astal.Overlay.ConstructorProperties>

// Revealer
export type Revealer = Widget<typeof Gtk.Revealer>
export const Revealer = astalify<typeof Gtk.Revealer, RevealerProps, "Revealer">(Gtk.Revealer)
export type RevealerProps = ConstructProps<typeof Gtk.Revealer, Gtk.Revealer.ConstructorProperties>

// Scrollable
export type Scrollable = Widget<typeof Astal.Scrollable>
export const Scrollable = astalify<typeof Astal.Scrollable, ScrollableProps, "Scrollable">(Astal.Scrollable)
export type ScrollableProps = ConstructProps<typeof Astal.Scrollable, Astal.Scrollable.ConstructorProperties>

// Slider
export type Slider = Widget<typeof Astal.Slider>
export const Slider = astalify<typeof Astal.Slider, SliderProps, "Slider">(Astal.Slider)
export type SliderProps = ConstructProps<typeof Astal.Slider, Astal.Slider.ConstructorProperties, {
    onDragged: []
}>

// TODO: Stack

// Switch
export type Switch = Widget<typeof Gtk.Switch>
export const Switch = astalify<typeof Gtk.Switch, SwitchProps, "Switch">(Gtk.Switch)
export type SwitchProps = ConstructProps<typeof Gtk.Switch, Gtk.Switch.ConstructorProperties>

// Window
export type Window = Widget<typeof Astal.Window>
export const Window = astalify<typeof Astal.Window, WindowProps, "Window">(Astal.Window)
export type WindowProps = ConstructProps<typeof Astal.Window, Astal.Window.ConstructorProperties>
