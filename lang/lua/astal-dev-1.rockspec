package = "astal"
version = "dev-1"

source = {
    url = "git+https://github.com/aylur/astal",
}

description = {
    summary = "lua bindings for libastal.",
    homepage = "https://aylur.github.io/astal/",
    license = "GPL-3",
}

dependencies = {
    "lua >= 5.1, < 5.4",
    "lgi >= 0.9.2",
}

build = {
    type = "builtin",
    modules = {
        ["astal.application"] = "lib/application.lua",
        ["astal.binding"] = "lib/binding.lua",
        ["astal.init"] = "lib/init.lua",
        ["astal.process"] = "lib/process.lua",
        ["astal.time"] = "lib/time.lua",
        ["astal.variable"] = "lib/variable.lua",
        ["astal.widget"] = "lib/widget.lua",
        ["astal.file"] = "lib/file.lua",
    },
}
