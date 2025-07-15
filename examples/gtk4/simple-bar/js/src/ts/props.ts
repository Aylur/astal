import GObject from "gi://GObject?version=2.0"

export function string(name: string, defaultValue = "") {
    return {
        [name]: GObject.ParamSpec.string(
            name,
            null,
            null,
            GObject.ParamFlags.READWRITE,
            defaultValue,
        ),
    }
}

export function number(
    name: string,
    min = -Number.MIN_SAFE_INTEGER,
    max = Number.MAX_SAFE_INTEGER,
    defaultValue = 0,
) {
    return {
        [name]: GObject.ParamSpec.double(
            name,
            null,
            null,
            GObject.ParamFlags.READWRITE,
            min,
            max,
            defaultValue,
        ),
    }
}

export function boolean(name: string, defaultValue = false) {
    return {
        [name]: GObject.ParamSpec.boolean(
            name,
            null,
            null,
            GObject.ParamFlags.READWRITE,
            defaultValue,
        ),
    }
}

