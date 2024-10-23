# Contributing

You can contribute by:
- [Suggesting new features](https://github.com/Aylur/astal/issues/new?assignees=&labels=enhancement&projects=&template=feature_request.md&title=)
- [Reporting bugs](https://github.com/Aylur/astal/issues/new?assignees=&labels=bug&projects=&template=bug_report.md&title=)
- Improving docs with additional contexts and examples
  - adding more distros to sections about installations e.g [bulding from source](https://aylur.github.io/astal/guide/getting-started/installation#bulding-libastal-from-source)
- Adding more example projects to [examples](https://github.com/Aylur/astal/tree/main/examples)
- Adding new language support/binding. For these open a PR for discussions.
- Adding new libraries e.g support for more wayland compositors
- [Adding](https://github.com/Aylur/astal/tree/main/docs#add-your-creation-to-the-showcases-page) your project to the [showcases page](https://aylur.github.io/astal/showcases/).
- Creating packaging for distributions

## Adding new libraries

Write libraries preferably in Vala. Only choose C if some dependency is only available in C e.g wayland.

## Todo

Planned features, you could help with:
- [niri ipc library](https://github.com/Aylur/astal/issues/8)
- sway ipc library
- greetd ipc library
- core: http request library, abstraction over libsoup included (mostly to be used in gjs and lua)
- core: notification sending, libnotify clone [#26](https://github.com/Aylur/astal/issues/26)
- setting up [uncrustify](https://github.com/uncrustify/uncrustify) for Vala
