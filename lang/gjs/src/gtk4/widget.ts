import { Astal, ConstructProps } from "../gtk4"
import astalify, { BindableChild } from "./astalify"
import GObject from "gi://GObject"

export type WindowProps = ConstructProps<Window, Astal.Window.ConstructorProps>
export class Window extends astalify(
    Astal.Window,
    (children, self) => {
        if (children.length != 1) {
            console.error(`Window can have only 1 child, attempted to set ${children.length}`)
        }
        if (children.length > 0)
            self.set_child(children[0])
    },
) {
    static { GObject.registerClass({ GTypeName: "Window" }, this) }
    constructor(props?: WindowProps, child?: BindableChild) { super({ child, ...props } as any) }
}
