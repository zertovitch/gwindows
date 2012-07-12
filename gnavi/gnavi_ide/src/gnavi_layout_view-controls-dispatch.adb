with GWindows.Application;
with GWindows.Drawing_Objects;

--  GNAVI: No user changes may be made here
--
--  NB: This is a manual fake of what GNAVI should create once it is working.
--      We need this for building GNAVI_IDE first - kind of bootstrapping!
--      GdM July 2012

separate (GNAVI_Layout_View.Controls)

function Dispatch
     (Parent      : GWindows.Base.Pointer_To_Base_Window_Class;
      Control_XML : GNAVI_Window.Control_Element)
     return GWindows.Base.Pointer_To_Base_Window_Class
is
begin
  return Parent;  --  Probably not what we want...
end Dispatch;

