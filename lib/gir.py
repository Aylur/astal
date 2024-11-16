"""
Vala's generated gir does not contain comments,
so we use valadoc to generate them. However, they are formatted
for valadoc and not gi-docgen so we need to fix it.
"""

import xml.etree.ElementTree as ET
import html
import sys
import subprocess
import re
import os


# valac fails on gi-docgen compliant markdown
# gi-docgen removes valac compliant ulink
# so we use vala notation and turn it into markdown
def ulink_to_markdown(text: str):
    pattern = r'<ulink url="(.*?)">(.*?)</ulink>'
    return re.sub(pattern, r"[\2](\1)", text)


def fix_gir(name: str, gir: str, out: str):
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
            doc.text = ulink_to_markdown(
                html.unescape(doc.text).replace("<para>", "").replace("</para>", "")
            )

    if (inc := root.find("c:include", namespaces)) is not None:
        inc.set("name", f"{name}.h")
    else:
        print("no c:include tag found", file=sys.stderr)
        exit(1)

    tree.write(out, encoding="utf-8", xml_declaration=True)


def valadoc(name: str, gir: str, args: list[str]):
    cmd = [os.getenv("VALADOC", "valadoc"), "-o", "docs", "--package-name", name, "--gir", gir, *args]
    try:
        subprocess.run(cmd, check=True, text=True, capture_output=True)
    except subprocess.CalledProcessError as e:
        print(e.stderr, file=sys.stderr)
        exit(1)


if __name__ == "__main__":
    name = sys.argv[1]
    in_out = sys.argv[2].split(":")
    args = sys.argv[3:]

    gir = in_out[0]
    out = in_out[1] if len(in_out) > 1 else gir

    valadoc(name, gir, args)
    fix_gir(name, gir, out)
