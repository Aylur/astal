# Installation

## Nix

maintainer: [@Aylur](https://github.com/Aylur)

Read more about it on the [nix page](../getting-started/nix#ags)

You can try without installing.

<!--TODO: remove v2 after merge-->
```sh
nix run github:aylur/ags/v2 -- --help
```

## Bulding AGS from source

1. [Install Astal](../getting-started/installation.md) if you have not already

2. Install the following dependencies

:::code-group

```sh [Arch]
sudo pacman -Syu go npm gjs
```

```sh [Fedora]
sudo dnf install golang npm gjs
```

```sh [Alpine]
sudo apk add go npm gjs
```

```sh [Ubuntu]
sudo apt install golang-go npm gjs
```

```sh [openSUSE]
sudo zypper install go npm gjs
```

:::

3. Clone the repo and Install

<!--TODO: remove v2 after merge-->
```sh
git clone https://github.com/aylur/ags.git
cd ags
git checkout v2 # https://github.com/Aylur/ags/pull/504
go install
```

:::info
If you have installed Astal **not** in `/usr` make sure to set its path.

```sh
go install -ldflags "-X main.astalGjs=$(pkg-config --variable prefix astal-0.1)/share/astal/gjs"
```

:::
