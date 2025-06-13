namespace AstalNiri {
public struct PhysicalSize {
   public uint32 x;
   public uint32 y;
   public PhysicalSize.from_json(Json.Object object) {
        x = (uint32)object.get_int_member("x");
        y = (uint32)object.get_int_member("y");
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

}


