# Installation

It is recommended to use [AGS](https://github.com/aylur/ags)
to scaffold and run projects in TypeScript.

It lets you

- generate TypeScript types using [ts-for-gir](https://github.com/gjsify/ts-for-gir)
- generate a tsconfig which is used by LSPs
- bundle your TypeScript and JavaScript code using [esbuild](https://esbuild.github.io/).

:::details Trivia
AGS is the predecessor of Astal, which was written purely in TypeScript and so only supported
JavaScript/TypeScript. Now it serves as a scaffolding tool for Astal+TypeScript+JSX projects.
:::

## Nix

maintainer: [@Aylur](https://github.com/Aylur)

Read more about it on the [nix page](../getting-started/nix#ags)

You can try without installing.

<!--TODO: remove v2 after merge-->
```sh
nix run github:aylur/ags/v2#agsFull -- --help
```

## From source

1. [Install Astal](../getting-started/installation.md) if you have not already

2. Install the Astal GJS package

```sh
git clone https://github.com/aylur/astal /tmp/astal
cd /tmp/astal/lang/gjs
meson setup --prefix /usr build
meson install -C build
```

:::tip
You might be wondering why it is recommended to install a JavaScript
package on the system instead of installing it as a node module.
It is solely to keep it in **sync** with the core `astal-io` and `astal3`/`astal4` package.
:::

3. Install the following dependencies

:::code-group

```sh [<i class="devicon-archlinux-plain"></i> Arch]
sudo pacman -Syu go npm gjs
```

```sh [<i class="devicon-fedora-plain"></i> Fedora]
sudo dnf install golang npm gjs
```

```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
sudo apt install golang-go npm gjs
```

:::

3. Clone the repo and Install

<!--TODO: remove v2 after merge-->
```sh
git clone https://github.com/aylur/ags.git /tmp/ags
cd /tmp/ags
git checkout v2 # https://github.com/Aylur/ags/pull/504
go install
```

:::tip
`go install` installs the `ags` binary to `$GOPATH/bin` so make sure its in your `$PATH`.
You can move it to another directory if you like. For example

```sh
sudo mv $GOPATH/bin/ags /usr/bin/ags
```

:::

## Without AGS

🚧 Setting up a dev environment without AGS is not yet documented. 🚧
