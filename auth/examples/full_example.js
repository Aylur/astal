#!/usr/bin/env -S gjs -m

import Auth from "gi://AstalAuth";
import GLib from "gi://GLib";

const loop = GLib.MainLoop.new(null, false);

const pam = new Auth.Pam();
pam.connect("auth-prompt-visible", (p, msg) => {
    print(msg);
    p.supply_secret("");
});
pam.connect("auth-prompt-hidden", (p, msg) => {
    print(msg);
    p.supply_secret("password");
});
pam.connect("auth-info", (p, msg) => {
    print(msg);
    p.supply_secret("");
});
pam.connect("auth-error", (p, msg) => {
    print(msg);
    p.supply_secret("");
});

pam.connect("success", p => {
    print("authentication sucessful");
    loop.quit();
});
pam.connect("fail", (p, msg) => {
    print(msg);
    loop.quit();
});

pam.start_authenticate();

loop.runAsync()

