namespace AstalSway {
public enum PayloadType {
    MESSAGE_RUN_COMMAND = 0,
    MESSAGE_GET_WORKSPACES = 1,
    MESSAGE_SUBSCRIBE = 2,
    MESSAGE_GET_OUTPUTS = 3,
    MESSAGE_GET_TREE = 4,
    MESSAGE_GET_MARKS = 5,
    MESSAGE_GET_BAR_CONFIG = 6,
    MESSAGE_GET_VERSION = 7,
    MESSAGE_GET_BINDING_NODES = 8,
    MESSAGE_GET_CONFIG = 9,
    MESSAGE_SEND_TICK = 10,
    MESSAGE_SYNC = 11,
    MESSAGE_GET_BINDING_STATE = 12,
    MESSAGE_GET_INPUTS = 100,
    MESSAGE_GET_SEATS = 101,
    EVENT_WORKSPACE = 0x80000000,
    EVENT_OUTPUT = 0x80000001,
    EVENT_MODE = 0x80000002,
    EVENT_WINDOW = 0x80000003,
    EVENT_BARCONFIG_UPDATE = 0x80000004,
    EVENT_BINDING = 0x80000005,
    EVENT_SHUTDOWN = 0x80000006,
    EVENT_TICK = 0x80000007,
    EVENT_BAR_STATE_UPDATE = 0x80000014,
    EVENT_INPUT = 0x80000015,
}

public enum NodeType {
    ROOT,
    WORKSPACE,
    CONTAINER,
    WINDOW,
    OUTPUT,
    SCRATCHPAD
}

public struct Rectangle {
    internal static Rectangle from_json(Json.Object obj) {
        return Rectangle() {
            x = (int)obj.get_int_member("x"),
            y = (int)obj.get_int_member("y"),
            width = (int)obj.get_int_member("width"),
            height = (int)obj.get_int_member("height"),
        };

    }

    public int x;
    public int y;
    public int width;
    public int height;
}

}
