local lgi = require("lgi")
local Astal = lgi.require("Astal", "0.1")

local AstalLua = Astal.Application:derive("AstalLua")
local request_handler

function AstalLua:do_request(msg, conn)
    if type(request_handler) == "function" then
        request_handler(msg, function(response)
            Astal.write_sock(conn, tostring(response), function(_, res)
                Astal.write_sock_finish(res)
            end)
        end)
    else
        Astal.Application.do_request(self, msg, conn)
    end
end

function AstalLua:quit(code)
    Astal.Application.quit(self)
    os.exit(code)
end

local app = AstalLua()

---@class StartConfig
---@field icons? string
---@field instance_name? string
---@field gtk_theme? string
---@field icon_theme? string
---@field cursor_theme? string
---@field css? string
---@field hold? boolean
---@field request_handler? fun(msg: string, response: fun(res: any))
---@field main? fun(...): unknown
---@field client? fun(message: fun(msg: string): string, ...): unknown

---@param config StartConfig | nil
function Astal.Application:start(config)
    if config == nil then
        config = {}
    end

    if config.client == nil then
        config.client = function()
            print('Astal instance "' .. app.instance_name .. '" is already running')
            os.exit(1)
        end
    end

    if config.hold == nil then
        config.hold = true
    end

    request_handler = config.request_handler

    if config.css then
        self:apply_css(config.css)
    end
    if config.icons then
        self:add_icons(config.icons)
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
        if type(config.main) == "function" then
            config.main(table.unpack(arg))
        end
        if config.hold then
            self:hold()
        end
    end

    if not app:acquire_socket() then
        return config.client(function(msg)
            return Astal.Application.send_message(self.instance_name, msg)
        end, table.unpack(arg))
    end

    self:run(nil)
end

return app
