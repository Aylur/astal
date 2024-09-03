---
layout: home
pageClass: home-page

hero:
  name: "Astal"
  text: "Library and Framework for building Desktop Shells"
  tagline: "The best way to make <i>beautiful</i> <b>and</b> <i>functional</i> wayland widgets!"
  image: /front-image.png
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
    details: The main focus of Astal is TypeScript using JSX. But you can use the libraries in any language that supports <a href="https://en.wikipedia.org/wiki/List_of_language_bindings_for_GTK">Gobject Introspection</a>.
  - title: No bash scripts needed
    details: Includes modules to work with Network, Bluetooth, Battery, Audio and more.
  - title: Use any Gtk widget
    details: With Astal you work with Gtk directly. You are not limited to only a set of them.
---
<script setup>
import Showcases from './showcases/Showcases.vue'
</script>

<Showcases />

<!--TODO: add feature icons-->
<!--TODO: add icons for buttons https://github.com/vuejs/vitepress/pull/3795-->

<style>
:root {
  --vp-home-hero-name-color: transparent;
  --vp-home-hero-name-background: -webkit-linear-gradient(120deg, var(--vp-c-brand-1), var(--vp-c-brand-3));
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

  .VPNavBar {
    background-color: transparent !important;
    -webkit-backdrop-filter: blur(16px);
    backdrop-filter: blur(16px);

    div.divider {
      display: none;
    }
  }
}
</style>
