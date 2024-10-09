local a = require("astal")

a.App:start({
    instance_name = "Hello",
    main = function()
        a.Widget.Window({
            no_implicit_destroy = true,
            a.Widget.Button({
                label = "hello",
            }),
        })
    end,
})
