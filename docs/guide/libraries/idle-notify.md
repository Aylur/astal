# IdleNotify

Library implementing the `ext-idle-notifier-v1` wayland protocol.

## Usage

You can browse the [IdleNotify reference](https://docs.astal.dev/idle-notify).

### Library

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import IdleNotify from "gi://AstalIdleNotify"

const notifier = IdleNotify.get_default()

const notif = notifier.get_input_idle_notification(1000)

notif.connect("idled", () => print("idled"))
notif.connect("resumed", () => print("resumed"))
```
:::

## Installation

1. install dependencies

    This lib does depend on AstalWl, which needs to be installed first.

    :::code-group

    ```sh [<i class="devicon-archlinux-plain"></i> Arch]
    sudo pacman -Syu meson vala valadoc wayland-client gobject-introspection
    ```
    :::

2. clone repo

    ```sh
    git clone https://github.com/aylur/astal.git
    cd astal/lib/idle-notify
    ```

3. install

    ```sh
    meson setup build
    meson install -C build
    ```
