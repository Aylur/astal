import gi

gi.require_version("GObject", "2.0")
gi.require_version("AstalIO", "0.1")

from astal.binding import bind
from astal.variable import Variable
from astal.file import *
from astal.time import *
from astal.process import *
