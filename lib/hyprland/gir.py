# Keep the shared generator unchanged; correct only the new Hyprland property.
import runpy
from pathlib import Path
import sys
import xml.etree.ElementTree as ET

runpy.run_path(str(Path(__file__).resolve().parent.parent / "gir.py"), run_name="__main__")
output = sys.argv[sys.argv.index("--gir") + 1].split(":")[-1]
tree = ET.parse(output)
ns = {"gi": "http://www.gtk.org/introspection/core/1.0"}
prop = tree.find('.//gi:class[@name="Workspace"]/gi:property[@name="address"]', ns)
if prop is not None:
    prop.set("writable", "0")
tree.write(output, encoding="utf-8", xml_declaration=True)
