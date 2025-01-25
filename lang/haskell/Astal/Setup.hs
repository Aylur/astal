{-# LANGUAGE OverloadedStrings #-}

import Data.GI.CodeGen.CabalHooks (setupBinding, TaggedOverride(..))

import qualified GI.AstalIO.Config as AstalIO
import qualified GI.GObject.Config as GObject
import qualified GI.Gdk.Config as Gdk
import qualified GI.GdkPixbuf.Config as GdkPixbuf
import qualified GI.Gio.Config as Gio
import qualified GI.Gtk.Config as Gtk


main :: IO ()
main = setupBinding name version pkgName pkgVersion verbose overridesFile inheritedOverrides outputDir
  where name = "Astal"
        version = "3.0"
        pkgName = "gi-astal"
        pkgVersion = "3.0.0"
        overridesFile = Nothing
        verbose = False
        outputDir = Nothing
        inheritedOverrides = [TaggedOverride "inherited:AstalIO" AstalIO.overrides, TaggedOverride "inherited:GObject" GObject.overrides, TaggedOverride "inherited:Gdk" Gdk.overrides, TaggedOverride "inherited:GdkPixbuf" GdkPixbuf.overrides, TaggedOverride "inherited:Gio" Gio.overrides, TaggedOverride "inherited:Gtk" Gtk.overrides]
