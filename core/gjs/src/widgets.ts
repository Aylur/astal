/* eslint-disable max-len */
import { Astal, GObject, Gtk } from "./imports.js"
import astalify, { type ConstructProps, type BindableChild } from "./astalify.js"

export { astalify, ConstructProps }

// Box
Object.defineProperty(Astal.Box.prototype, "children", {
    get() { return this.get_children() },
    set(v) { this.set_children(v) },
})

export type BoxProps = ConstructProps<Astal.Box, Astal.Box.ConstructorProps>
export class Box extends astalify(Astal.Box) {
    static { GObject.registerClass({ GTypeName: "Box" }, this) }
    constructor(props?: BoxProps, ...children: Array<BindableChild>) { super({ children, ...props } as any) }
}

// Button
export type ButtonProps = ConstructProps<Astal.Button, Astal.Button.ConstructorProps, {
    onClicked: []
    onClick: [event: Astal.ClickEvent]
    onClickRelease: [event: Astal.ClickEvent]
    onHover: [event: Astal.HoverEvent]
    onHoverLost: [event: Astal.HoverEvent]
    onScroll: [event: Astal.ScrollEvent]
}>
export class Button extends astalify(Astal.Button) {
    static { GObject.registerClass({ GTypeName: "Button" }, this) }
    constructor(props?: ButtonProps, child?: BindableChild) { super({ child, ...props } as any) }
}

// CenterBox
export type CenterBoxProps = ConstructProps<Astal.CenterBox, Astal.CenterBox.ConstructorProps>
export class CenterBox extends astalify(Astal.CenterBox) {
    static { GObject.registerClass({ GTypeName: "CenterBox" }, this) }
    constructor(props?: CenterBoxProps, ...children: Array<BindableChild>) { super({ children, ...props } as any) }
}

// CircularProgress
export type CircularProgressProps = ConstructProps<Astal.CircularProgress, Astal.CircularProgress.ConstructorProps>
export class CircularProgress extends astalify(Astal.CircularProgress) {
    static { GObject.registerClass({ GTypeName: "CircularProgress" }, this) }
    constructor(props?: CircularProgressProps, child?: BindableChild) { super({ child, ...props } as any) }
}

// DrawingArea
export type DrawingAreaProps = ConstructProps<Gtk.DrawingArea, Gtk.DrawingArea.ConstructorProps, {
    onDraw: [cr: any] // TODO: cairo types
}>
export class DrawingArea extends astalify(Gtk.DrawingArea) {
    static { GObject.registerClass({ GTypeName: "DrawingArea" }, this) }
    constructor(props?: DrawingAreaProps) { super(props as any) }
}

// Entry
export type EntryProps = ConstructProps<Gtk.Entry, Gtk.Entry.ConstructorProps, {
    onChanged: []
    onActivate: []
}>
export class Entry extends astalify(Gtk.Entry) {
    static { GObject.registerClass({ GTypeName: "Entry" }, this) }
    constructor(props?: EntryProps) { super(props as any) }
}

// EventBox
export type EventBoxProps = ConstructProps<Astal.EventBox, Astal.EventBox.ConstructorProps, {
    onClick: [event: Astal.ClickEvent]
    onClickRelease: [event: Astal.ClickEvent]
    onHover: [event: Astal.HoverEvent]
    onHoverLost: [event: Astal.HoverEvent]
    onScroll: [event: Astal.ScrollEvent]
}>
export class EventBox extends astalify(Astal.EventBox) {
    static { GObject.registerClass({ GTypeName: "EventBox" }, this) }
    constructor(props?: EventBoxProps, child?: BindableChild) { super({ child, ...props } as any) }
}

// // TODO: Fixed
// // TODO: FlowBox
//
// Icon
export type IconProps = ConstructProps<Astal.Icon, Astal.Icon.ConstructorProps>
export class Icon extends astalify(Astal.Icon) {
    static { GObject.registerClass({ GTypeName: "Icon" }, this) }
    constructor(props?: IconProps) { super(props as any) }
}

// Label
export type LabelProps = ConstructProps<Astal.Label, Astal.Label.ConstructorProps>
export class Label extends astalify(Astal.Label) {
    static { GObject.registerClass({ GTypeName: "Label" }, this) }
    constructor(props?: LabelProps) { super(props as any) }
}

// LevelBar
export type LevelBarProps = ConstructProps<Astal.LevelBar, Astal.LevelBar.ConstructorProps>
export class LevelBar extends astalify(Astal.LevelBar) {
    static { GObject.registerClass({ GTypeName: "LevelBar" }, this) }
    constructor(props?: LevelBarProps) { super(props as any) }
}

// TODO: ListBox

// Overlay
export type OverlayProps = ConstructProps<Astal.Overlay, Astal.Overlay.ConstructorProps>
export class Overlay extends astalify(Astal.Overlay) {
    static { GObject.registerClass({ GTypeName: "Overlay" }, this) }
    constructor(props?: OverlayProps, ...children: Array<BindableChild>) { super({ children, ...props } as any) }
}

// Revealer
export type RevealerProps = ConstructProps<Gtk.Revealer, Gtk.Revealer.ConstructorProps>
export class Revealer extends astalify(Gtk.Revealer) {
    static { GObject.registerClass({ GTypeName: "Revealer" }, this) }
    constructor(props?: RevealerProps, child?: BindableChild) { super({ child, ...props } as any) }
}

// Scrollable
export type ScrollableProps = ConstructProps<Astal.Scrollable, Astal.Scrollable.ConstructorProps>
export class Scrollable extends astalify(Astal.Scrollable) {
    static { GObject.registerClass({ GTypeName: "Scrollable" }, this) }
    constructor(props?: ScrollableProps, child?: BindableChild) { super({ child, ...props } as any) }
}

// Slider
export type SliderProps = ConstructProps<Astal.Slider, Astal.Slider.ConstructorProps, {
    onDragged: []
}>
export class Slider extends astalify(Astal.Slider) {
    static { GObject.registerClass({ GTypeName: "Slider" }, this) }
    constructor(props?: SliderProps) { super(props as any) }
}

// Stack
export type StackProps = ConstructProps<Astal.Stack, Astal.Stack.ConstructorProps>
export class Stack extends astalify(Astal.Stack) {
    static { GObject.registerClass({ GTypeName: "Stack" }, this) }
    constructor(props?: StackProps, ...children: Array<BindableChild>) { super({ children, ...props } as any) }
}

// Switch
export type SwitchProps = ConstructProps<Gtk.Switch, Gtk.Switch.ConstructorProps>
export class Switch extends astalify(Gtk.Switch) {
    static { GObject.registerClass({ GTypeName: "Switch" }, this) }
    constructor(props?: SwitchProps) { super(props as any) }
}

// Window
export type WindowProps = ConstructProps<Astal.Window, Astal.Window.ConstructorProps>
export class Window extends astalify(Astal.Window) {
    static { GObject.registerClass({ GTypeName: "Window" }, this) }
    constructor(props?: WindowProps, child?: BindableChild) { super({ child, ...props } as any) }
}
