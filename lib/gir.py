"""
Vala's generated gir does not contain comments,
so we use valadoc to generate them. However, they are formatted
for valadoc and not gi-docgen so we need to fix it.
"""

import xml.etree.ElementTree as ET
import html
import sys
import subprocess


def fix_gir(gir: str):
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
            doc.text = (
                html.unescape(doc.text).replace("<para>", "").replace("</para>", "")
            )

    tree.write(gir, encoding="utf-8", xml_declaration=True)


def valadoc(gir: str, args: list[str]):
    subprocess.run(["valadoc", "-o", "docs", "--gir", gir, *args])


if __name__ == "__main__":
    gir = sys.argv[1]
    args = sys.argv[2:]

    valadoc(gir, args)
    fix_gir(gir)
