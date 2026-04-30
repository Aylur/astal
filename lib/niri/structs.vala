namespace AstalNiri {
public struct PhysicalSize {
   public uint32 x;
   public uint32 y;
   public PhysicalSize.from_json(Json.Array size) {
        x = (uint32)size.get_int_element(0);
        y = (uint32)size.get_int_element(1);
   }
}
// public enum Transform {
//     Normal = "normal",
//     _90 = "90",
//     _180 = "180",
//     _270 = "270",
//     Flipped = "flipped",
//     Flipped90 = "flipped-90",
//     Flipped180 = "flipped-180",
//     Flipped270 = "flipped-270",
// }
public struct LogicalOutput {
    public int32  x            {get; private set;}
    public int32  y            {get; private set;}
    public uint32 width        {get; private set;}
    public uint32 height       {get; private set;}
    public float  scale        {get; private set;}
    public string transform    {get; private set;}

    internal LogicalOutput.from_json(Json.Object object) {
        x = (int32)object.get_int_member("x");
        y = (int32)object.get_int_member("y");
        width = (uint32)object.get_int_member("width");
        height = (uint32)object.get_int_member("height");
        scale = (float)object.get_int_member("scale");
        transform = object.get_string_member("transform");
    }
}
public struct Mode {
    public int64  width        {get; private set;}
    public int64  height       {get; private set;}
    public int64  refresh_rate {get; private set;}
    public bool is_preferred   {get; private set;}

    internal Mode.from_json(Json.Object object) {
        width = object.get_int_member("width");
        height = object.get_int_member("height");
        refresh_rate = object.get_int_member("refresh_rate");
        is_preferred = object.get_boolean_member("is_preferred");
    }
}

public enum WorkspaceReferenceArgTag {
  Id,
  Index,
  Name,
}

public enum ColumnDisplayTag {
  Normal,
  Tabbed
}

public enum SizeChangeTag {
  SetFixed,
  SetProportion,
  AdjustFixed,
  AdjustProportion,
}

public enum LayoutSwitchTargetTag {
  Next,
  Prev,
  Index,
}

public struct WindowLayout {
    /**
      * Optional
      * 1-indexed
      */
    public uint pos_in_scrolling_layout[2];
    public double tile_size[2];
    public int32 window_size[2];
    /**
      * Optional
      * only populated on floating windows https://github.com/YaLTeR/niri/issues/2381#issuecomment-3289241212
      */
    public double tile_pos_in_workspace_view[2];
    public double window_offset_in_tile[2];

    internal WindowLayout.from_json(Json.Object object) {
        if (object.has_member("pos_in_scrolling_layout") && !object.get_null_member("pos_in_scrolling_layout")) {
            var arr = object.get_array_member("pos_in_scrolling_layout");
            pos_in_scrolling_layout[0] = (uint)arr.get_int_element(0);
            pos_in_scrolling_layout[1] = (uint)arr.get_int_element(1);
        }

        var tile_arr = object.get_array_member("tile_size");
        tile_size[0] = tile_arr.get_double_element(0);
        tile_size[1] = tile_arr.get_double_element(1);

        var win_arr = object.get_array_member("window_size");
        window_size[0] = (int)win_arr.get_int_element(0);
        window_size[1] = (int)win_arr.get_int_element(1);

        if (object.has_member("tile_pos_in_workspace_view") && !object.get_null_member("tile_pos_in_workspace_view")) {
            var pos_arr = object.get_array_member("tile_pos_in_workspace_view");
            tile_pos_in_workspace_view[0] = pos_arr.get_double_element(0);
            tile_pos_in_workspace_view[1] = pos_arr.get_double_element(1);
        }

        var offset_arr = object.get_array_member("window_offset_in_tile");
        window_offset_in_tile[0] = offset_arr.get_double_element(0);
        window_offset_in_tile[1] = offset_arr.get_double_element(1);
    }
}

}


