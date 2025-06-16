---
layout: home
pageClass: home-page

hero:
  name: "Astal"
  text: "Create Beautiful Widgets With Ease"
  tagline: "The Linux Suite to Craft Desktop Shells and Widgets with GTK!"
  image: /icon.svg
  actions:
    - theme: brand
      text: Introduction
      link: /guide/introduction
    - theme: alt
      text: References
      link: /guide/libraries/references

features:
  - title: Use Your Preferred Language
    icon: <i style="color:var(--vp-c-brand-3)" class="devicon-c-plain"></i>
    details:
      Astal is written with GLib and it supports languages that support GObject
      introspection.
  - title: No bash scripts needed
    icon: <i style="color:var(--vp-c-brand-3)" class="devicon-bash-plain"></i>
    details:
      Includes modules to work with Network, Bluetooth, Battery, Audio and <a
      href="/astal/guide/libraries/references#astal-libraries">more</a>.
  - title: Use any Gtk widget
    icon: <i style="color:var(--vp-c-brand-3)" class="fl-gtk"></i>
    details:
      With Astal you work with Gtk directly. You are not limited to only a set
      of them.
---

<!--TODO: add icons for buttons https://github.com/vuejs/vitepress/pull/3795-->

<style>
:root {
  --vp-home-hero-name-color: transparent;
  --vp-home-hero-name-background: -webkit-linear-gradient(120deg, var(--vp-c-purple-3), var(--vp-c-brand-3));

  --vp-home-hero-image-background-image: linear-gradient(-45deg, var(--vp-c-purple-3), var(--vp-c-brand-3));
  --vp-home-hero-image-filter: blur(44px);
}

:root {
  --overlay-gradient: color-mix(in srgb, var(--vp-c-brand-1), transparent 55%);
}

.dark {
  --overlay-gradient: color-mix(in srgb, var(--vp-c-brand-1), transparent 85%);
}

.home-page {
  background:
    linear-gradient(215deg, var(--overlay-gradient), transparent 40%),
    radial-gradient(var(--overlay-gradient), transparent 40%) no-repeat -60vw -40vh / 105vw 200vh,
    radial-gradient(var(--overlay-gradient), transparent 65%) no-repeat 50% calc(100% + 20rem) / 60rem 30rem;

  .VPFeature a {
    font-weight: bold;
    color: var(--vp-c-brand-2);
  }

  .VPFooter {
    background-color: transparent !important;
    border: none;
  }

  .VPNavBar:not(.top) {
    background-color: transparent !important;
    -webkit-backdrop-filter: blur(16px);
    backdrop-filter: blur(16px);

    div.divider {
      display: none;
    }
  }
}

@media (min-width: 640px) {
  :root {
    --vp-home-hero-image-filter: blur(56px);
  }
}

@media (min-width: 960px) {
  :root {
    --vp-home-hero-image-filter: blur(68px);
  }
}
</style>
