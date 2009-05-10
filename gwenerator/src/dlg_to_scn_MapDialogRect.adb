  procedure Dlg_to_Scn(  -- version using specific font info
    window     :  in Window_type;
    xd,yd,wd,hd:  in Integer;
    xs,ys,ws,hs: out Integer)
  is
    use Interfaces;
    type Rect_Type is record l,t,r,b: unsigned_32; end record;
    type Rect_Type_Access is access all Rect_Type;
    -- MapDialogRect / http://msdn.microsoft.com/en-us/library/ms645502(VS.85).aspx
    function MapDialogRect( h: GWindows.Types.Handle; rect: Rect_Type_Access) return Integer_32;
    pragma Import (StdCall, MapDialogRect, "MapDialogRect");
    rect: aliased Rect_Type;
  begin
    rect:= (unsigned_32(xd), unsigned_32(yd), unsigned_32(xd+wd), unsigned_32(yd+hd));
    if MapDialogRect(Handle(window), rect'access) = 0 then
      -- Message_Box("","Failed!");
      Dlg_to_Scn(  -- use default version
        xd,yd,wd,hd,
        xs,ys,ws,hs
      );
    else
      xs:= Integer(rect.l);
      ys:= Integer(rect.t);
      ws:= Integer(rect.r-rect.l);
      hs:= Integer(rect.b-rect.t);
    end if;
  end Dlg_to_Scn;
