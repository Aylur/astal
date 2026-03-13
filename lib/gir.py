"""
Vala's generated gir does not contain comments,
so we use valadoc to generate them. However, they are formatted
for valadoc and not gi-docgen so we need to fix it.
"""

import argparse
import xml.etree.ElementTree as ET
import html
import sys
import subprocess
import re
import os


# valac fails on gi-docgen compliant markdown
# gi-docgen removes valac compliant ulink
# so we use vala notation and turn it into markdown
def _ulink_to_markdown(text: str):
    pattern = r'<ulink url="(.*?)">(.*?)</ulink>'
    return re.sub(pattern, r"[\2](\1)", text)


def _fix_gir(name: str, gir: str, out: str, properties: str | None):
    namespaces = {
        "": "http://www.gtk.org/introspection/core/1.0",
        "c": "http://www.gtk.org/introspection/c/1.0",
        "glib": "http://www.gtk.org/introspection/glib/1.0",
    }
    for prefix, uri in namespaces.items():
        ET.register_namespace(prefix, uri)

    tree = ET.parse(gir)
    root = tree.getroot()

    for doc in root.findall(".//doc", namespaces):
        if doc.text:
            doc.text = _ulink_to_markdown(
                html.unescape(doc.text).replace("<para>", "").replace("</para>", "")
            )

    if (inc := root.find("c:include", namespaces)) is not None:
        inc.set("name", f"{name}.h")
    else:
        print("no c:include tag found", file=sys.stderr)
        exit(1)

    # Fix "Can't convert untyped array to JS value"
    # https://gitlab.gnome.org/GNOME/vala/-/issues/1212
    if properties is not None:
        for part in properties.split(":"):
            klass, prop = part.split(".")
            query = f'.//class[@name="{klass}"]/property[@name="{prop}"]'
            if elem := root.find(query, namespaces):
                elem.set("getter", f"get_{prop}")

    tree.write(out, encoding="utf-8", xml_declaration=True)


def _valadoc(name: str, gir: str, pkgs: str, sources: list[str]):
    pkglist = ["glib-2.0", "gobject-2.0", "gio-2.0", *pkgs.split(":")]
    cmd = [
        os.getenv("VALADOC", "valadoc"),
        *["-o", "docs"],
        *["--package-name", name],
        *["--gir", gir],
        *[f"--pkg={pkg}" for pkg in pkglist if pkg != ""],
        *sources,
    ]
    try:
        subprocess.run(
            cmd,
            check=True,
            text=True,
            stderr=sys.stderr,
            stdout=sys.stdout,
        )
    except subprocess.CalledProcessError:
        exit(1)


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--name", required=True, help="name of package in kebab-case")
    parser.add_argument("--gir", required=True, help="name of .gir target")
    parser.add_argument("--pkgs", default="", help="dependencies separeted with :")
    parser.add_argument("--property", help="class.member properties")
    parser.add_argument("sources", nargs=argparse.REMAINDER, help="vala source files")
    args = parser.parse_args()

    split = args.gir.split(":")
    gir = split[0]
    out = split[1] if len(split) > 1 else gir

    _valadoc(args.name, gir, args.pkgs, args.sources)
    _fix_gir(args.name, gir, out, args.property)
