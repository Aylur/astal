package = "astal"
version = "dev-1"
source = {
   url = "git+https://github.com/astal-sh/libastal"
}
description = {
   summary = "lua bindings for libastal.",
   homepage = "https://github.com/astal-sh/libastal",
   license = "GPL-3"
}
build = {
   type = "builtin",
   modules = {
      ["astal.application"] = "astal/application.lua",
      ["astal.binding"] = "astal/binding.lua",
      ["astal.init"] = "astal/init.lua",
      ["astal.process"] = "astal/process.lua",
      ["astal.time"] = "astal/time.lua",
      ["astal.variable"] = "astal/variable.lua",
      ["astal.widget"] = "astal/widget.lua",
  }
}
