#!/usr/bin/env -S gjs -m
import Auth from "gi://AstalAuth";
import Gio from "gi://Gio";

Gio._promisify(Auth.Pam, "authenticate");

await Auth.Pam.authenticate("password")
    .then(_ => print("authentication sucessful"))
    .catch(logError);