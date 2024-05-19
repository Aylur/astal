local lgi = require("lgi")
local Astal = lgi.require("Astal", "0.1")

local AstalLua = Astal.Application:derive("AstalLua")
local request_handler

function AstalLua:do_request(msg, conn)
    if type(request_handler) == "function" then
        request_handler(msg, function(request)
            Astal.write_sock(conn, request, function(_, res)
                Astal.write_sock_finish(res)
            end)
        end)
    else
        Astal.Application.do_request(self, msg, conn)
    end
end

local app = AstalLua()

---@class StartConfig
---@field instance_name? string
---@field gtk_theme? string
---@field icon_theme? string
---@field cursor_theme? string
---@field css? string
---@field hold? boolean
---@field request_handler? fun(msg: string, response: fun(res: string))

---@param config StartConfig | nil
---@param callback function | nil
function Astal.Application:start(config, callback)
    if config == nil then
        config = {}
    end

    if config.hold == nil then
        config.hold = true
    end

    request_handler = config.request_handler

    if config.css then
        self:apply_css(config.css)
    end
    if config.instance_name then
        self.instance_name = config.instance_name
    end
    if config.gtk_theme then
        self.gtk_theme = config.gtk_theme
    end
    if config.icon_theme then
        self.icon_theme = config.icon_theme
    end
    if config.cursor_theme then
        self.cursor_theme = config.cursor_theme
    end

    app.on_activate = function()
        if config.hold then
            self:hold()
        end
        if type(callback) == "function" then
            callback()
        end
    end

    if not app:acquire_socket() then
        print('Astal instance "' .. app.instance_name .. '" is already running')
        os.exit(1)
    end

    self:run(nil)
end

return app
