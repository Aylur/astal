local App = require("astal.application")

App:start({
    instance_name = "test",
    main = function()
        App:quit(1)
    end,
})
