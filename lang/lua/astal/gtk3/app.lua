local lgi = require("lgi")
local Astal = lgi.require("Astal", "3.0")
local AstalIO = lgi.require("AstalIO", "0.1")

local AstalLua = Astal.Application:derive("AstalLua")
local request_handler

function AstalLua:do_request(msg, conn)
    if type(request_handler) == "function" then
        request_handler(msg, function(response)
            AstalIO.write_sock(conn, tostring(response), function(_, res)
                AstalIO.write_sock_finish(res)
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
---@field icons string?
---@field instance_name string?
---@field gtk_theme string?
---@field icon_theme string?
---@field cursor_theme string?
---@field css string?
---@field hold boolean?
---@field request_handler fun(msg: string, response: fun(res: any)): nil
---@field main fun(...): nil
---@field client fun(message: fun(msg: string): string, ...): nil

---@param config? StartConfig
function Astal.Application:start(config)
    config = config or {}

    config.client = config.client
        or function()
            print('Astal instance "' .. app.instance_name .. '" is already running')
            os.exit(1)
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

    for _, key in ipairs({ "instance_name", "gtk_theme", "icon_theme", "cursor_theme" }) do
        if config[key] then
            self[key] = config[key]
        end
    end

    app.on_activate = function()
        if type(config.main) == "function" then
            config.main(table.unpack(arg or {}))
        end
        if config.hold then
            self:hold()
        end
    end

    local _, err = app:acquire_socket()
    if err ~= nil then
        return config.client(function(msg)
            return AstalIO.send_request(self.instance_name, msg)
        end, table.unpack(arg or {}))
    end

    self:run(nil)
end

return app
