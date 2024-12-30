import Astal from "gi://Astal?version=4.0"
import Gtk from "gi://Gtk?version=4.0"
import astalify, { type, type Config, type ConstructProps, type ExtraPropsFunc } from "./astalify.js"

function filter(children: any[]) {
    return children.flat(Infinity).map(ch => ch instanceof Gtk.Widget
        ? ch
        : new Gtk.Label({ visible: true, label: String(ch) }))
}

function mkOverridable<
    Widget extends Gtk.Widget,
    Props extends Gtk.Widget.ConstructorProps = Gtk.Widget.ConstructorProps,
    Signals extends Record<`on${string}`, Array<unknown>> = Record<`on${string}`, any[]>,
>(
    cls: { new(...args: any[]): Widget },
    config: Partial<Config<Widget>> = {},
) {
    return function mkWidget<
        ExtraProps extends Record<string, unknown> = Record<string, undefined>,
    >(extraProps = {} as ExtraPropsFunc<Widget, ExtraProps>) {
        return astalify<Widget, Props, Signals, ExtraProps>(cls, config, extraProps)
    }
}

// Box
Object.defineProperty(Astal.Box.prototype, "children", {
    get() { return this.get_children() },
    set(v) { this.set_children(v) },
})

export type BoxProps = ConstructProps<Astal.Box, Astal.Box.ConstructorProps>

const mkBox = mkOverridable<Astal.Box, Astal.Box.ConstructorProps>(Astal.Box, {
    getChildren(self) { return self.get_children() },
    setChildren(self, children) { return self.set_children(filter(children)) },
})

export const Box = mkBox()
export function ExtendBox<
    ExtraProps extends Record<string, unknown> = Record<string, undefined>,
>(extraProps = {} as ExtraPropsFunc<Astal.Box, ExtraProps>) {
    return mkBox(extraProps)
}

// Button
type ButtonSignals = {
    onClicked: []
}

export type ButtonProps = ConstructProps<Gtk.Button, Gtk.Button.ConstructorProps, ButtonSignals>

const mkButton = mkOverridable<Gtk.Button, Gtk.Button.ConstructorProps, ButtonSignals>(Gtk.Button)

export const Button = mkButton()
export function ExtendButton<
    ExtraProps extends Record<string, unknown> = Record<string, undefined>,
>(extraProps = {} as ExtraPropsFunc<Gtk.Button, ExtraProps>) {
    return mkButton(extraProps)
}

// CenterBox
export type CenterBoxProps = ConstructProps<Gtk.CenterBox, Gtk.CenterBox.ConstructorProps>

const mkCenterBox = mkOverridable<Gtk.CenterBox, Gtk.CenterBox.ConstructorProps>(Gtk.CenterBox, {
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

export const CenterBox = mkCenterBox()
export function ExtendCenterBox<
    ExtraProps extends Record<string, unknown> = Record<string, undefined>,
>(extraProps = {} as ExtraPropsFunc<Gtk.CenterBox, ExtraProps>) {
    return mkCenterBox(extraProps)
}

// TODO: CircularProgress
// TODO: DrawingArea

// Entry
type EntrySignals = {
    onActivate: []
    onNotifyText: []
}

export type EntryProps = ConstructProps<Gtk.Entry, Gtk.Entry.ConstructorProps, EntrySignals>

const mkEntry = mkOverridable<Gtk.Entry, Gtk.Entry.ConstructorProps, EntrySignals>(Gtk.Entry, {
    getChildren() { return [] },
})

export const Entry = mkEntry()
export function ExtendEntry<
    ExtraProps extends Record<string, unknown> = Record<string, undefined>,
>(extraProps = {} as ExtraPropsFunc<Gtk.Entry, ExtraProps>) {
    return mkEntry(extraProps)
}

// Image
export type ImageProps = ConstructProps<Gtk.Image, Gtk.Image.ConstructorProps>

const mkImage = mkOverridable<Gtk.Image, Gtk.Image.ConstructorProps>(Gtk.Image, {
    getChildren() { return [] },
})

export const Image = mkImage()
export function ExtendImage<
    ExtraProps extends Record<string, unknown> = Record<string, undefined>,
>(extraProps = {} as ExtraPropsFunc<Gtk.Image, ExtraProps>) {
    return mkImage(extraProps)
}

// Label
export type LabelProps = ConstructProps<Gtk.Label, Gtk.Label.ConstructorProps>

const mkLabel = mkOverridable<Gtk.Label, Gtk.Label.ConstructorProps>(Gtk.Label, {
    getChildren() { return [] },
    setChildren(self, children) { self.label = String(children) },
})

export const Label = mkLabel()
export function ExtendLabel<
    ExtraProps extends Record<string, unknown> = Record<string, undefined>,
>(extraProps = {} as ExtraPropsFunc<Gtk.Label, ExtraProps>) {
    return mkLabel(extraProps)
}

// LevelBar
export type LevelBarProps = ConstructProps<Gtk.LevelBar, Gtk.LevelBar.ConstructorProps>

const mkLevelBar = mkOverridable<Gtk.LevelBar, Gtk.LevelBar.ConstructorProps>(Gtk.LevelBar, {
    getChildren() { return [] },
})

export const LevelBar = mkLevelBar()
export function ExtendLevelBar<
    ExtraProps extends Record<string, unknown> = Record<string, undefined>,
>(extraProps = {} as ExtraPropsFunc<Gtk.LevelBar, ExtraProps>) {
    return mkLevelBar(extraProps)
}

// TODO: ListBox

// Overlay
export type OverlayProps = ConstructProps<Gtk.Overlay, Gtk.Overlay.ConstructorProps>

const mkOverlay = mkOverridable<Gtk.Overlay, Gtk.Overlay.ConstructorProps>(Gtk.Overlay, {
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

export const Overlay = mkOverlay()
export function ExtendOverlay<
    ExtraProps extends Record<string, unknown> = Record<string, undefined>,
>(extraProps = {} as ExtraPropsFunc<Gtk.Overlay, ExtraProps>) {
    return mkOverlay(extraProps)
}

// Revealer
export type RevealerProps = ConstructProps<Gtk.Revealer, Gtk.Revealer.ConstructorProps>

const mkRevealer = mkOverridable<Gtk.Revealer, Gtk.Revealer.ConstructorProps>(Gtk.Revealer)

export const Revealer = mkRevealer()
export function ExtendRevealer<
    ExtraProps extends Record<string, unknown> = Record<string, undefined>,
>(extraProps = {} as ExtraPropsFunc<Gtk.Revealer, ExtraProps>) {
    return mkRevealer(extraProps)
}

// Slider
type SliderSignals = {
    onChangeValue: []
}

export type SliderProps = ConstructProps<Astal.Slider, Astal.Slider.ConstructorProps, SliderSignals>

const mkSlider = mkOverridable<Astal.Slider, Astal.Slider.ConstructorProps, SliderSignals>(Astal.Slider, {
    getChildren() { return [] },
})

export const Slider = mkSlider()
export function ExtendSlider<
    ExtraProps extends Record<string, unknown> = Record<string, undefined>,
>(extraProps = {} as ExtraPropsFunc<Astal.Slider, ExtraProps>) {
    return mkSlider(extraProps)
}

// Stack
export type StackProps = ConstructProps<Gtk.Stack, Gtk.Stack.ConstructorProps>

const mkStack = mkOverridable<Gtk.Stack, Gtk.Stack.ConstructorProps>(Gtk.Stack, {
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

export const Stack = mkStack()
export function ExtendStack<
    ExtraProps extends Record<string, unknown> = Record<string, undefined>,
>(extraProps = {} as ExtraPropsFunc<Gtk.Stack, ExtraProps>) {
    return mkStack(extraProps)
}

// Switch
export type SwitchProps = ConstructProps<Gtk.Switch, Gtk.Switch.ConstructorProps>

const mkSwitch = mkOverridable<Gtk.Switch, Gtk.Switch.ConstructorProps>(Gtk.Switch, {
    getChildren() { return [] },
})

export const Switch = mkSwitch()
export function ExtendSwitch<
    ExtraProps extends Record<string, unknown> = Record<string, undefined>,
>(extraProps = {} as ExtraPropsFunc<Gtk.Switch, ExtraProps>) {
    return mkSwitch(extraProps)
}

// Window
export type WindowProps = ConstructProps<Astal.Window, Astal.Window.ConstructorProps>

const mkWindow = mkOverridable<Astal.Window, Astal.Window.ConstructorProps>(Astal.Window)

export const Window = mkWindow()
export function ExtendWindow<
    ExtraProps extends Record<string, unknown> = Record<string, undefined>,
>(extraProps = {} as ExtraPropsFunc<Astal.Window, ExtraProps>) {
    return mkWindow(extraProps)
}

// MenuButton
export type MenuButtonProps = ConstructProps<Gtk.MenuButton, Gtk.MenuButton.ConstructorProps>

const mkMenuButton = mkOverridable<Gtk.MenuButton, Gtk.MenuButton.ConstructorProps>(Gtk.MenuButton, {
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

export const MenuButton = mkMenuButton()
export function ExtendMenuButton<
    ExtraProps extends Record<string, unknown> = Record<string, undefined>,
>(extraProps = {} as ExtraPropsFunc<Gtk.MenuButton, ExtraProps>) {
    return mkMenuButton(extraProps)
}

// Popoper
export type PopoverProps = ConstructProps<Gtk.Popover, Gtk.Popover.ConstructorProps>

const mkPopover = mkOverridable<Gtk.Popover, Gtk.Popover.ConstructorProps>(Gtk.Popover)

export const Popover = mkPopover()
export function ExtendPopover<
    ExtraProps extends Record<string, unknown> = Record<string, undefined>,
>(extraProps = {} as ExtraPropsFunc<Gtk.Popover, ExtraProps>) {
    return mkPopover(extraProps)
}
