/* eslint-disable max-len */
import { Astal, Gtk } from "./imports.js"
import astalify, { type ConstructProps, type Widget } from "./astalify.js"

export { astalify, ConstructProps }

// Box
export type Box = Widget<Astal.Box>
export const Box = astalify<typeof Astal.Box, BoxProps, "Box">(Astal.Box)
export type BoxProps = ConstructProps<Astal.Box, Astal.Box.ConstructorProps>

// Button
export type Button = Widget<Astal.Button>
export const Button = astalify<typeof Astal.Button, ButtonProps, "Button">(Astal.Button)
export type ButtonProps = ConstructProps<Astal.Button, Astal.Button.ConstructorProps, {
    onClicked: []
    onClick: [event: Astal.ClickEvent]
    onClickRelease: [event: Astal.ClickEvent]
    onHover: [event: Astal.HoverEvent]
    onHoverLost: [event: Astal.HoverEvent]
    onScroll: [event: Astal.ScrollEvent]
}>

// CenterBox
export type CenterBox = Widget<Astal.CenterBox>
export const CenterBox = astalify<typeof Astal.CenterBox, CenterBoxProps, "CenterBox">(Astal.CenterBox)
export type CenterBoxProps = ConstructProps<Astal.CenterBox, Astal.CenterBox.ConstructorProps>

// CircularProgress
export type CircularProgress = Widget<Astal.CircularProgress>
export const CircularProgress = astalify<typeof Astal.CircularProgress, CircularProgressProps, "CircularProgress">(Astal.CircularProgress)
export type CircularProgressProps = ConstructProps<Astal.CircularProgress, Astal.CircularProgress.ConstructorProps>


// DrawingArea
export type DrawingArea = Widget<Gtk.DrawingArea>
export const DrawingArea = astalify<typeof Gtk.DrawingArea, DrawingAreaProps, "DrawingArea">(Gtk.DrawingArea)
export type DrawingAreaProps = ConstructProps<Gtk.DrawingArea, Gtk.DrawingArea.ConstructorProps, {
    onDraw: [cr: any] // TODO: cairo types
}>

// Entry
export type Entry = Widget<Gtk.Entry>
export const Entry = astalify<typeof Gtk.Entry, EntryProps, "Entry">(Gtk.Entry)
export type EntryProps = ConstructProps<Gtk.Entry, Gtk.Entry.ConstructorProps, {
    onChanged: []
    onActivate: []
}>

// EventBox
export type EventBox = Widget<Astal.EventBox>
export const EventBox = astalify<typeof Astal.EventBox, EventBoxProps, "EventBox">(Astal.EventBox)
export type EventBoxProps = ConstructProps<Astal.EventBox, Astal.EventBox.ConstructorProps, {
    onClick: [event: Astal.ClickEvent]
    onClickRelease: [event: Astal.ClickEvent]
    onHover: [event: Astal.HoverEvent]
    onHoverLost: [event: Astal.HoverEvent]
    onScroll: [event: Astal.ScrollEvent]
}>

// TODO: Fixed
// TODO: FlowBox

// Icon
export type Icon = Widget<Astal.Icon>
export const Icon = astalify<typeof Astal.Icon, IconProps, "Icon">(Astal.Icon)
export type IconProps = ConstructProps<Astal.Icon, Astal.Icon.ConstructorProps>

// Label
export type Label = Widget<Astal.Label>
export const Label = astalify<typeof Astal.Label, LabelProps, "Label">(Astal.Label)
export type LabelProps = ConstructProps<Astal.Label, Astal.Label.ConstructorProps>

// LevelBar
export type LevelBar = Widget<Astal.LevelBar>
export const LevelBar = astalify<typeof Astal.LevelBar, LevelBarProps, "LevelBar">(Astal.LevelBar)
export type LevelBarProps = ConstructProps<Astal.LevelBar, Astal.LevelBar.ConstructorProps>

// TODO: ListBox

// Overlay
export type Overlay = Widget<Astal.Overlay>
export const Overlay = astalify<typeof Astal.Overlay, OverlayProps, "Overlay">(Astal.Overlay)
export type OverlayProps = ConstructProps<Astal.Overlay, Astal.Overlay.ConstructorProps>

// Revealer
export type Revealer = Widget<Gtk.Revealer>
export const Revealer = astalify<typeof Gtk.Revealer, RevealerProps, "Revealer">(Gtk.Revealer)
export type RevealerProps = ConstructProps<Gtk.Revealer, Gtk.Revealer.ConstructorProps>

// Scrollable
export type Scrollable = Widget<Astal.Scrollable>
export const Scrollable = astalify<typeof Astal.Scrollable, ScrollableProps, "Scrollable">(Astal.Scrollable)
export type ScrollableProps = ConstructProps<Astal.Scrollable, Astal.Scrollable.ConstructorProps>

// Slider
export type Slider = Widget<Astal.Slider>
export const Slider = astalify<typeof Astal.Slider, SliderProps, "Slider">(Astal.Slider)
export type SliderProps = ConstructProps<Astal.Slider, Astal.Slider.ConstructorProps, {
    onDragged: []
}>

// TODO: Stack

// Switch
export type Switch = Widget<Gtk.Switch>
export const Switch = astalify<typeof Gtk.Switch, SwitchProps, "Switch">(Gtk.Switch)
export type SwitchProps = ConstructProps<Gtk.Switch, Gtk.Switch.ConstructorProps>

// Window
export type Window = Widget<Astal.Window>
export const Window = astalify<typeof Astal.Window, WindowProps, "Window">(Astal.Window)
export type WindowProps = ConstructProps<Astal.Window, Astal.Window.ConstructorProps>
