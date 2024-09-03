import { defineConfig } from 'vitepress'

function reference(lib = "") {
    return `https://aylur.github.io/libastal${lib}`
}

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
                    { text: "Astal", link: reference("") },
                    { text: "Apps", link: reference("/apps") },
                    { text: "Auth", link: reference("/auth") },
                    { text: "Battery", link: reference("/battery") },
                    { text: "Bluetooth", link: reference("/bluetooth") },
                    { text: "Hyprland", link: reference("/hyprland") },
                    { text: "Mpris", link: reference("/mpris") },
                    { text: "Network", link: reference("/network") },
                    { text: "Notifd", link: reference("/notifd") },
                    { text: "PowerProfiles", link: reference("/powerprofiles") },
                    { text: "River", link: reference("/river") },
                    { text: "Tray", link: reference("/tray") },
                    { text: "WirePlumber", link: reference("/wireplumber") },
                ],
            },
        ],

        socialLinks: [
            { icon: 'github', link: github() },
            { icon: 'discord', link: '"https://discord.gg/CXQpHwDuhY"' },
        ],

        editLink: {
            pattern: github("edit/main/docs/:path"),
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
