import { Gtk, Astal } from "./imports.js"

export function setChild(parent: Gtk.Widget, child: Gtk.Widget) {
    if (parent instanceof Gtk.Bin) {
        const rm = parent.get_child()
        if (rm)
            parent.remove(rm)
    }
    if (parent instanceof Gtk.Container)
        parent.add(child)
}

// gjs fails to map List types?
Object.defineProperty(Astal.Box.prototype, "children", {
    get() { return this.get_children() },
    set(v) { this.set_children(v) },
})

// gjs deprecated the child setter
Object.defineProperty(Gtk.Container.prototype, "child", {
    get() { return this.get_child?.() },
    set(v) { setChild(this, v) },
})
