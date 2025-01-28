# Setting up a Project
It is recommended to use [AGS](https://aylur.github.io/ags/)
to scaffold and run projects in TypeScript.

It lets you

- generate TypeScript types using [ts-for-gir](https://github.com/gjsify/ts-for-gir)
- generate a tsconfig which is used by LSPs to provide intellisense
- bundle your TypeScript and JavaScript code using [esbuild](https://esbuild.github.io/)

:::details Trivia
AGS is the predecessor of Astal, which was written purely in TypeScript and so only supported
JavaScript/TypeScript. Now it serves as a scaffolding tool for Astal+TypeScript projects.
:::

## Manual setup
Without AGS, you will have to set up the bundling
step and add a few files(e.g. tsconfig.json & package.json) yourself

### Installing gjs bindings
Before proceeding, you will have to install Astal's gjs bindings

#### Arch
:::code-group

```sh [AUR installation]
yay -S libastal-gjs-git
```
:::

#### NixOS
Like when [installing astal](../getting-started/nix.md), the `astal-gjs`
derivation(flake output: `packages.${system}.gjs`)
will usually only be used in derivations and devShells.

The following command will build the bindings and
print the store path, which can be used in package.json

```sh
nix build --no-link github:aylur/astal#gjs; nix eval --raw github:aylur/astal#gjs
```

#### Building from Source
1. Ensure that AstalIO and Astal3(or Astal4?? if you want
to use gtk4) and their dependencies are installed

Question for pr reviewers: should astal-gjs always depend on Astal3?
(the above line should be removed before this is merged)

2. Clone the repo and install the bindings

```sh
git clone https://github.com/aylur/astal /tmp/astal
cd astal/lang/gjs
# This will install the bindings to /usr/share/astal/gjs
meson setup --prefix /usr build
meson install -C build
```

### Adding required files

The following few code blocks should be enough to get you started

:::code-group

```json [tsconfig.json]
{
  "$schema": "https://json.schemastore.org/tsconfig",
  "compilerOptions": {
    "experimentalDecorators": true,
    "strict": true,
    "target": "ES2022",
    "module": "ES2022",
    "moduleResolution": "Bundler",
    // Uncomment the following 2 lines if you want to use javascript
    // "checkJs": true,
    // "allowJs": true,
    "jsx": "react-jsx",
    "paths": {
      "astal": ["LOCATION_OF_ASTAL_GJS/share/gjs"],
      "astal/*": ["LOCATION_OF_ASTAL_GJS/share/gjs/*"]
    },
    // E.g. ...: "astal/gtk3",
    "jsxImportSource": "astal/gtk<GTK_VERSION>",
  }
}

```

```ts [app.ts]
import { App } from "astal/gtk<GTK_VERSION>"
import Bar from "./widget/Bar"

App.start({
    main() {
        App.get_monitors().map(Bar)
    },
})
```

```sh [.bundle]
#!/usr/bin/env bash
# This is to be used as a shebang line in your app.ts
# To use it that way, make this and your app.ts file executable

entrypointPath="$1"

esbuild \
  --format=esm --target=es2022 \
  --external:{console,system,cairo,gettext,'file://*','gi://*','resource://*'} \
  --bundle "${entrypointPath}" --outfile=/tmp/my-shell.js

gjs -m /tmp/my-shell.js
```

:::

Nix users can consult the [nix page](../getting-started/nix#typescript)
for a better way to write the bundler script.
In that case, the shebang line of `app.ts` should be `#!/usr/bin/env -S astalGjsBundler --run`
