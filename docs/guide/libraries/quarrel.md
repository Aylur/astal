# Quarrel

A CLI argument parser with subcommand support.

## Usage

You can browse the
[Quarrel reference](https://aylur.github.io/libastal/quarrel).

### Library

:::code-group

```js [<i class="devicon-javascript-plain"></i> JavaScript]
import { programInvocationName, programArgs, exit } from "system"
import Quarrel from "gi://Quarrel"

const help = Quarrel.Flag.new("help", "h".charCodeAt(0), "Print this help")

const greeting = Quarrel.StringOpt.new(
    "greeting",
    "g".charCodeAt(0),
    "Greeting [default: 'Greeting']",
)

const greet = Quarrel.Command.new("greet")
    .about("Greeter subcommand")
    .arg("NAME", "Positional argument")
    .opt(greeting)
    .opt(help)

const cli = Quarrel.Command.new("hello")
    .about("My Awesome CLI")
    .subcommand(greet)
    .arg("NAME", "Positional argument")
    .opt(help)

const command = cli.parse([programInvocationName, ...programArgs])

if (command === cli) {
    if (help.value || cli.args.length === 0) {
        print(Quarrel.help(cli))
        exit(1)
    }

    const [name] = cli.args
    print(`Hello ${name ?? "unknown"}!`)
}

if (command === greet) {
    if (help.value || greet.args.length === 0) {
        print(Quarrel.help(greet))
        exit(1)
    }

    const [name] = greet.args
    print(`${greeting.value ?? "Greeting"} ${name ?? "unknown"}!`)
}
```

```py [<i class="devicon-python-plain"></i> Python]
import sys
from gi.repository import Quarrel

help = Quarrel.Flag.new("help", ord("h"), "Print this help")

greeting = Quarrel.StringOpt.new(
    "greeting",
    ord("g"),
    "Greeting [default: 'Greeting']",
)

greet = (
    Quarrel.Command.new("greet")
    .about("Greeter subcommand")
    .arg("NAME", "Positional argument")
    .opt(greeting)
    .opt(help)
)

cli = (
    Quarrel.Command.new("hello")
    .about("My Awesome CLI")
    .subcommand(greet)
    .arg("NAME", "Positional argument")
    .opt(help)
)

command = cli.parse(sys.argv)

if command == cli:
    if help.value or len(cli.get_args()) == 0:
        print(Quarrel.help(cli))
        sys.exit(1)

    name = cli.get_args()[0]
    print(f"Hello {name or 'unknown'}!")

if command == greet:
    if help.value or len(greet.get_args()) == 0:
        print(Quarrel.help(greet))
        sys.exit(1)

    name = greet.get_args()[0]
    print(f"{greeting.value or 'Greeting'} {name or 'unknown'}!")
```

```lua [<i class="devicon-lua-plain"></i> Lua]
local Quarrel = require("lgi").require("Quarrel")

local help = Quarrel.Flag.new("help", string.byte("h"), "Print this help")

local greeting = Quarrel.StringOpt.new(
    "greeting",
    string.byte("g"),
    "Greeting [default: 'Greeting']"
)

local greet = Quarrel.Command.new("greet")
    :about("Greeter subcommand")
    :arg("NAME", "Positional argument")
    :opt(greeting):opt(help)

local cli = Quarrel.Command.new("hello")
    :about("My Awesome CLI")
    :subcommand(greet)
    :arg("NAME", "Positional argument")
    :opt(help)

local argv = { arg[0] }
for i = 1, #arg do
	table.insert(argv, arg[i])
end

local command = cli:parse(argv)

if command == cli then
	if help.value or #cli.args == 0 then
		print(Quarrel.help(cli))
		os.exit(1)
	end

	local name = cli.args[1]
	print(string.format("Hello %s!", name or "unknown"))
end

if command == greet then
	if help.value or #greet.args == 0 then
		print(Quarrel.help(greet))
		os.exit(1)
	end

	local name = greet.args[1]
	print(string.format("%s %s!", greeting.value or "Greeting", name or "unknown"))
end
```

```vala [<i class="devicon-vala-plain"></i> Vala]
int main(string[] args) {
    var help = new Quarrel.Flag("help", 'h', "Print this help");

    var greeting = new Quarrel.StringOpt(
        "greeting",
        'g',
        "Greeting [default: 'Greeting']"
    );

    var greet = new Quarrel.Command("greet")
        .about("Greeter subcommand")
        .arg("NAME", "Positional argument")
        .opt(greeting)
        .opt(help);

    var cli = new Quarrel.Command("hello")
        .about("My Awesome CLI")
        .subcommand(greet)
        .arg("NAME", "Positional argument")
        .opt(help);

    var command = cli.parse(args);

    if (command == cli) {
        if (help.value || cli.args.length == 0) {
            print("%s\n", Quarrel.help(cli));
            return 1;
        }

        var name = cli.args[0];
        print("Hello %s!\n", name ?? "unknown");
    }

    if (command == greet) {
        if (help.value || greet.args.length == 0) {
            print("%s\n", Quarrel.help(greet));
            return 1;
        }

        var name = greet.args[0];
        print("%s %s!\n", greeting.value ?? "Greeting", name ?? "unknown");
    }

    return 0;
}
```

:::

## Installation

1. install dependencies

    :::code-group

    ```sh [<i class="devicon-archlinux-plain"></i> Arch]
    sudo pacman -Syu meson vala valadoc gobject-introspection
    ```

    ```sh [<i class="devicon-fedora-plain"></i> Fedora]
    sudo dnf install meson vala valadoc gobject-introspection-devel
    ```

    ```sh [<i class="devicon-ubuntu-plain"></i> Ubuntu]
    sudo apt install meson valac valadoc gobject-introspection
    ```

    :::

2. clone repo

    ```sh
    git clone https://github.com/aylur/astal.git
    cd astal/lib/quarrel
    ```

3. install

    ```sh
    meson setup build
    meson install -C build
    ```
