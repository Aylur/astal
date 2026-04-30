import { inject } from "@vercel/analytics"
import type { Theme } from "vitepress"
import DefaultTheme from "vitepress/theme"
import "./theme.css"
import "devicon/devicon.min.css"
import "font-logos/assets/font-logos.css"

const theme: Theme = {
  extends: DefaultTheme,
  enhanceApp() {
    inject()
  },
}

export default theme
