import { defineConfig } from "astro/config"
import starlight from "@astrojs/starlight"

export default defineConfig({
    site: "https://aylur.github.io",
    base: "astal",
    integrations: [
        starlight({
            title: "Astal",
            editLink: {
                baseUrl: "https://github.com/Aylur/Astal/tree/main/docs",
            },
            social: {
                github: "https://github.com/Aylur/Astal",
                discord: "https://discord.gg/CXQpHwDuhY",
            },
            customCss: ["./src/style.css"],
            favicon: "./favicon.ico",
            sidebar: [
                { label: "Getting Started", autogenerate: { directory: "/getting-started" } },
                { label: "AGS", autogenerate: { directory: "/ags" } },
                {
                    label: "Libraries",
                    items: [
                        "libraries/references",
                        { label: "Astal", link: "/reference" },
                        { label: "Apps", link: "/reference/apps" },
                        { label: "Auth", link: "/reference/auth" },
                        { label: "Battery", link: "/reference/battery" },
                        { label: "Bluetooth", link: "/reference/bluetooth" },
                        { label: "Hyprland", link: "/reference/hyprland" },
                        { label: "Mpris", link: "/reference/mpris" },
                        { label: "Network", link: "/reference/network" },
                        { label: "Notifd", link: "/reference/notifd" },
                        { label: "PowerProfiles", link: "/reference/powerprofiles" },
                        { label: "River", link: "/reference/river" },
                        { label: "Tray", link: "/reference/tray" },
                        { label: "WirePlumber", link: "/reference/wireplumber" },
                    ],
                }
            ]
        }),
    ],
})
