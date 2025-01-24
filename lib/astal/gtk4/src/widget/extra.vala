public class Astal.Extra {
   /**
    * Restores window input region.
    */
   public static void restoreWindow(Gtk.Window window) {
      var windowGdkNative = window.get_native();
      if (windowGdkNative == null) return;

      var WindowGdkSurface = windowGdkNative.get_surface();
      if (WindowGdkSurface == null) return;

      Cairo.RectangleInt cairoRectangle = {
         0,
         0,
         window.get_width(),
         window.get_height()
      };

      var cairoRegionForInput = new Cairo.Region.rectangle(cairoRectangle);
      WindowGdkSurface.set_input_region(cairoRegionForInput);
   }


   /**
    * Makes the selected widget interactable within the window while setting the other widgets within the window to be click-through.
    * Note: This won't dynamically resize, you will have to listen for widget resize and call this again.
    */
   public static void makeSingleClickableWidget(Gtk.Widget widget, Gtk.Window window) {
      var windowGdkNative = window.get_native();
      if (windowGdkNative == null) return;

      var WindowGdkSurface = windowGdkNative.get_surface();
      if (WindowGdkSurface == null) return;

      Gtk.Allocation childAllocation;
      widget.get_allocation(out childAllocation);

      double x, y;
      widget.translate_coordinates(window, 0, 0, out x, out y);

      Cairo.RectangleInt cairoRectangle = {
         (int) x,
         (int) y,
         widget.get_width(),
         widget.get_height()
      };

      var cairoRegionForInput = new Cairo.Region.rectangle(cairoRectangle);
      WindowGdkSurface.set_input_region(cairoRegionForInput);
   }
}
