import { defineConfig } from 'vitepress'

function github(url = "") {
    return `https://github.com/aylur/astal${url}`
}

export default defineConfig({
    title: "Astal",
    description: "Documentation website of the Astal project",

    outDir: "./dist",
    base: "/astal/",
    cleanUrls: true,

    lastUpdated: true,
    ignoreDeadLinks: true, // FIXME:

    head: [
        ['link', { rel: 'icon', href: '/astal/favicon.ico' }],
    ],

    themeConfig: {
        // logo: "",
        //

        nav: [{
            text: '0.1.0',
            items: [
                { text: 'Contributing', link: github("/blob/main/CONTRIBUTING.md") },
                { text: 'Changelog', link: github("/blob/main/CHANGELOG.md") },
            ],
        }],

        sidebar: [
            {
                text: 'Getting Started',
                base: "/getting-started",
                collapsed: false,
                items: [
                    { text: 'Introduction', link: '/introduction' },
                    { text: 'Installation', link: '/installation' },
                    { text: 'Supported Languages', link: '/supported-languages' },
                ],
            },
            {
                text: 'AGS',
                base: "/ags",
                collapsed: false,
                items: [
                    { text: 'Installation', link: '/installation' },
                    { text: 'First Widgets', link: '/first-widgets' },
                    { text: 'Theming', link: '/theming' },
                    { text: 'CLI and App', link: '/cli-app' },
                    { text: 'Widget', link: '/widget' },
                    { text: 'Utilities', link: '/utilities' },
                    { text: 'Variable', link: '/variable' },
                    { text: 'FAQ', link: '/faq' },
                ],
            },
            {
                text: 'Libraries',
                collapsed: true,
                items: [
                    { text: 'References', link: '/libraries/references' },
                    { text: "Astal", link: "/libraries/libastal" },
                    { text: "Apps", link: "/libraries/apps" },
                    { text: "Auth", link: "/libraries/auth" },
                    { text: "Battery", link: "/libraries/battery" },
                    { text: "Bluetooth", link: "/libraries/bluetooth" },
                    { text: "Hyprland", link: "/libraries/hyprland" },
                    { text: "Mpris", link: "/libraries/mpris" },
                    { text: "Network", link: "/libraries/network" },
                    { text: "Notifd", link: "/libraries/notifd" },
                    { text: "PowerProfiles", link: "/libraries/powerprofiles" },
                    { text: "River", link: "/libraries/river" },
                    { text: "Tray", link: "/libraries/tray" },
                    { text: "WirePlumber", link: "/libraries/wireplumber" },
                ],
            },
        ],

        socialLinks: [
            { icon: 'github', link: github() },
            { icon: 'discord', link: '"https://discord.gg/CXQpHwDuhY"' },
        ],

        editLink: {
            pattern: github("/edit/main/docs/:path"),
            text: 'Edit this page on GitHub',
        },

        lastUpdated: {
            text: 'Last updated',
        },

        search: {
            provider: "local",
        }
    },
})
