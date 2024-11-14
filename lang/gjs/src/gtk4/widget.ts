import { Astal, ConstructProps } from "../gtk3"
import astalify, { BindableChild } from "./astalify"
import GObject from "gi://GObject"

export type WindowProps = ConstructProps<Window, Astal.Window.ConstructorProps>
export class Window extends astalify(Astal.Window) {
    static { GObject.registerClass({ GTypeName: "Window" }, this) }
    constructor(props?: WindowProps, child?: BindableChild) { super({ child, ...props } as any) }
}
