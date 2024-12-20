import Astal from "gi://Astal?version=4.0"
import Gtk from "gi://Gtk?version=4.0"
import astalify, { type, type ConstructProps } from "./astalify.js"

function filter(children: any[]) {
    return children.flat(Infinity).map(ch => ch instanceof Gtk.Widget
        ? ch
        : new Gtk.Label({ visible: true, label: String(ch) }))
}

// Box
Object.defineProperty(Astal.Box.prototype, "children", {
    get() { return this.get_children() },
    set(v) { this.set_children(v) },
})

export type BoxProps = ConstructProps<Astal.Box, Astal.Box.ConstructorProps>
export const Box = astalify<Astal.Box, Astal.Box.ConstructorProps>(Astal.Box, {
    getChildren(self) { return self.get_children() },
    setChildren(self, children) { return self.set_children(filter(children)) },
})

// Button
type ButtonSignals = {
    onClicked: []
}

export type ButtonProps = ConstructProps<Gtk.Button, Gtk.Button.ConstructorProps, ButtonSignals>
export const Button = astalify<Gtk.Button, Gtk.Button.ConstructorProps, ButtonSignals>(Gtk.Button)

// CenterBox
export type CenterBoxProps = ConstructProps<Gtk.CenterBox, Gtk.CenterBox.ConstructorProps>
export const CenterBox = astalify<Gtk.CenterBox, Gtk.CenterBox.ConstructorProps>(Gtk.CenterBox, {
    getChildren(box) {
        return [box.startWidget, box.centerWidget, box.endWidget]
    },
    setChildren(box, children) {
        const ch = filter(children)
        box.startWidget = ch[0] || new Gtk.Box
        box.centerWidget = ch[1] || new Gtk.Box
        box.endWidget = ch[2] || new Gtk.Box
    },
})

// TODO: CircularProgress
// TODO: DrawingArea

// Entry
type EntrySignals = {
    onActivate: []
    onNotifyText: []
}

export type EntryProps = ConstructProps<Gtk.Entry, Gtk.Entry.ConstructorProps, EntrySignals>
export const Entry = astalify<Gtk.Entry, Gtk.Entry.ConstructorProps, EntrySignals>(Gtk.Entry, {
    getChildren() { return [] },
})

// Image
export type ImageProps = ConstructProps<Gtk.Image, Gtk.Image.ConstructorProps>
export const Image = astalify<Gtk.Image, Gtk.Image.ConstructorProps>(Gtk.Image, {
    getChildren() { return [] },
})

// Label
export type LabelProps = ConstructProps<Gtk.Label, Gtk.Label.ConstructorProps>
export const Label = astalify<Gtk.Label, Gtk.Label.ConstructorProps>(Gtk.Label, {
    getChildren() { return [] },
    setChildren(self, children) { self.label = String(children) },
})

// LevelBar
export type LevelBarProps = ConstructProps<Gtk.LevelBar, Gtk.LevelBar.ConstructorProps>
export const LevelBar = astalify<Gtk.LevelBar, Gtk.LevelBar.ConstructorProps>(Gtk.LevelBar, {
    getChildren() { return [] },
})

// TODO: ListBox

// Overlay
export type OverlayProps = ConstructProps<Gtk.Overlay, Gtk.Overlay.ConstructorProps>
export const Overlay = astalify<Gtk.Overlay, Gtk.Overlay.ConstructorProps>(Gtk.Overlay, {
    getChildren(self) {
        const children: Array<Gtk.Widget> = []
        let ch = self.get_first_child()
        while (ch !== null) {
            children.push(ch)
            ch = ch.get_next_sibling()
        }

        return children.filter(ch => ch !== self.child)
    },
    setChildren(self, children) {
        for (const child of filter(children)) {
            const types = type in child
                ? (child[type] as string).split(/\s+/)
                : []

            if (types.includes("overlay")) {
                self.add_overlay(child)
            } else {
                self.set_child(child)
            }

            self.set_measure_overlay(child, types.includes("measure"))
            self.set_clip_overlay(child, types.includes("clip"))
        }
    },
})

// Revealer
export type RevealerProps = ConstructProps<Gtk.Revealer, Gtk.Revealer.ConstructorProps>
export const Revealer = astalify<Gtk.Revealer, Gtk.Revealer.ConstructorProps>(Gtk.Revealer)

// Slider
type SliderSignals = {
    onChangeValue: []
}

export type SliderProps = ConstructProps<Astal.Slider, Astal.Slider.ConstructorProps, SliderSignals>
export const Slider = astalify<Astal.Slider, Astal.Slider.ConstructorProps, SliderSignals>(Astal.Slider, {
    getChildren() { return [] },
})

// Stack
export type StackProps = ConstructProps<Gtk.Stack, Gtk.Stack.ConstructorProps>
export const Stack = astalify<Gtk.Stack, Gtk.Stack.ConstructorProps>(Gtk.Stack, {
    setChildren(self, children) {
        for (const child of filter(children)) {
            if (child.name != "" && child.name != null) {
                self.add_named(child, child.name)
            } else {
                self.add_child(child)
            }
        }
    },
})

// Switch
export type SwitchProps = ConstructProps<Gtk.Switch, Gtk.Switch.ConstructorProps>
export const Switch = astalify<Gtk.Switch, Gtk.Switch.ConstructorProps>(Gtk.Switch, {
    getChildren() { return [] },
})

// Window
export type WindowProps = ConstructProps<Astal.Window, Astal.Window.ConstructorProps>
export const Window = astalify<Astal.Window, Astal.Window.ConstructorProps>(Astal.Window)

// MenuButton
export type MenuButtonProps = ConstructProps<Gtk.MenuButton, Gtk.MenuButton.ConstructorProps>
export const MenuButton = astalify<Gtk.MenuButton, Gtk.MenuButton.ConstructorProps>(Gtk.MenuButton, {
    getChildren(self) { return [self.popover, self.child] },
    setChildren(self, children) {
        for (const child of filter(children)) {
            if (child instanceof Gtk.Popover) {
                self.set_popover(child)
            } else {
                self.set_child(child)
            }
        }
    },
})

// Popoper
export type PopoverProps = ConstructProps<Gtk.Popover, Gtk.Popover.ConstructorProps>
export const Popover = astalify<Gtk.Popover, Gtk.Popover.ConstructorProps>(Gtk.Popover)
