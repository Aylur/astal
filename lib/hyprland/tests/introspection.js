import Hyprland from "gi://AstalHyprland?version=0.1";
const hypr = Hyprland.Hyprland.get_default();
function check(value, message) { if (!value) throw new Error(message); }
const a = hypr.get_workspace_by_address("special:alpha");
const b = hypr.get_workspace_by_address("special:beta");
check(a && b && a !== b, "special identities collide");
check(hypr.get_workspace(1).id === 1, "numeric API changed");
check(hypr.get_client("0x1002").workspace === a, "client association lost");
check(hypr.get_monitor(0).special_workspace === a, "monitor association lost");
check(a.address === "special:alpha", "address property missing");
print("GJS imports local GI namespace and resolves workspace references");
