import { defineConfig } from "astro/config"
import starlight from "@astrojs/starlight"

const directories = [
    ["Getting Started", "/getting-started"],
    ["AGS", "/ags"],
    ["Libraries", "/libraries"],
]

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
            sidebar: directories.map(([label, directory]) => ({
                label,
                autogenerate: { directory },
            })),
        }),
    ],
})
