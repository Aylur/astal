import { defineConfig } from "vitepress"

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

  head: [["link", { rel: "icon", href: "/astal/icon.svg" }]],

  themeConfig: {
    logo: "/icon.svg",

    footer: {
      message: "Released under the LGPL v2.1 License",
      copyright: "Logo is created by VDawg",
    },

    nav: [
      {
        text: "Showcases",
        link: "/showcases",
        activeMatch: "/showcases/",
      },
      {
        text: "Guide",
        link: "/guide/introduction",
        activeMatch: "/guide/",
      },
    ],

    sidebar: [
      { text: "Introduction", link: "/guide/introduction" },
      { text: "Installation", link: "/guide/installation" },
      { text: "Nix", link: "/guide/nix" },
      {
        text: "Legacy Documentation",
        link: "https://aylur.github.io/astal-legacy-docs/",
      },
      {
        text: "Libraries",
        collapsed: false,
        items: [
          { text: "References", link: "/guide/libraries/references" },
          { text: "IO", link: "https://aylur.github.io/libastal/io" },
          { text: "Astal3", link: "https://aylur.github.io/libastal/astal3" },
          { text: "Astal4", link: "https://aylur.github.io/libastal/astal4" },
          { text: "Apps", link: "/guide/libraries/apps" },
          { text: "Auth", link: "/guide/libraries/auth" },
          { text: "Battery", link: "/guide/libraries/battery" },
          { text: "Bluetooth", link: "/guide/libraries/bluetooth" },
          { text: "Cava", link: "/guide/libraries/cava" },
          { text: "Greet", link: "/guide/libraries/greet" },
          { text: "Hyprland", link: "/guide/libraries/hyprland" },
          { text: "Mpris", link: "/guide/libraries/mpris" },
          { text: "Network", link: "/guide/libraries/network" },
          { text: "Notifd", link: "/guide/libraries/notifd" },
          { text: "PowerProfiles", link: "/guide/libraries/powerprofiles" },
          { text: "River", link: "/guide/libraries/river" },
          { text: "Tray", link: "/guide/libraries/tray" },
          { text: "WirePlumber", link: "/guide/libraries/wireplumber" },
        ],
      },
    ],

    socialLinks: [
      { icon: "github", link: github() },
      { icon: "discord", link: "https://discord.gg/CXQpHwDuhY" },
    ],

    editLink: {
      pattern: github("/edit/main/docs/:path"),
      text: "Edit this page on GitHub",
    },

    lastUpdated: {
      text: "Last updated",
    },

    search: {
      provider: "local",
    },
  },
})
