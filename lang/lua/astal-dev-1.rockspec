package = "astal"
version = "dev-1"

source = {
    url = "git+https://github.com/aylur/astal",
}

description = {
    summary = "lua bindings for libastal.",
    homepage = "https://aylur.github.io/astal/",
    license = "LGPL-2.1",
}

dependencies = {
    "lua >= 5.1, < 5.4",
    "lgi >= 0.9.2",
}

build = {
    type = "builtin",
    modules = {
        ["astal.binding"] = "astal/binding.lua",
        ["astal.file"] = "astal/file.lua",
        ["astal.init"] = "astal/init.lua",
        ["astal.process"] = "astal/process.lua",
        ["astal.time"] = "astal/time.lua",
        ["astal.variable"] = "astal/variable.lua",
        ["astal.gtk3.app"] = "astal/gtk3/app.lua",
        ["astal.gtk3.init"] = "astal/gtk3/init.lua",
        ["astal.gtk3.astalify"] = "astal/gtk3/astalify.lua",
        ["astal.gtk3.widget"] = "astal/gtk3/widget.lua",
        -- ["astal.gtk4.app"] = "astal/gtk4/app.lua",
        -- ["astal.gtk4.init"] = "astal/gtk4/init.lua",
        -- ["astal.gtk4.astalify"] = "astal/gtk4/astalify.lua",
        -- ["astal.gtk4.widget"] = "astal/gtk4/widget.lua",
    },
}
