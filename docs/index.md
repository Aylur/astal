---
layout: home
pageClass: home-page

hero:
  name: "Astal"
  text: "Create Beautiful Widgets With Ease"
  tagline: "The Framework to Craft Desktop Shells and <i>beautiful</i> <i>functional</i> Wayland Widgets with GTK!"
  image: /icon.svg
  actions:
    - theme: brand
      text: What is Astal?
      link: /getting-started/introduction
    - theme: alt
      text: Get Started
      link: /getting-started/installation
    - theme: alt
      text: References
      link: /libraries/references

features:
  - title: Use Your Preferred Language
    icon: <i style="color:var(--vp-c-brand-3)" class="devicon-typescript-plain"></i>
    details: The main focus of Astal is TypeScript using JSX. But you can use the libraries in any language that supports <a href="https://en.wikipedia.org/wiki/List_of_language_bindings_for_GTK">Gobject Introspection</a>.
  - title: No bash scripts needed
    icon: <i style="color:var(--vp-c-brand-3)" class="devicon-bash-plain"></i>
    details: Includes modules to work with Network, Bluetooth, Battery, Audio and more.
  - title: Use any Gtk widget
    icon: <i style="color:var(--vp-c-brand-3)" class="fl-gtk"></i>
    details: With Astal you work with Gtk directly. You are not limited to only a set of them.
---
<script setup>
import Showcases from './showcases/Showcases.vue'
</script>

<Showcases />

<!--TODO: add icons for buttons https://github.com/vuejs/vitepress/pull/3795-->

<style>
:root {
  --vp-home-hero-name-color: transparent;
  --vp-home-hero-name-background: -webkit-linear-gradient(120deg, var(--vp-c-purple-3), var(--vp-c-brand-3));

  --vp-home-hero-image-background-image: linear-gradient(-45deg, var(--vp-c-purple-3), var(--vp-c-brand-3));
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
    --vp-home-hero-image-filter: blur(46px);
  }
}

@media (min-width: 960px) {
  :root {
    --vp-home-hero-image-filter: blur(58px);
  }
}
</style>
