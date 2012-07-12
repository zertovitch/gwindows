with GWindows.Application;
with GWindows.Drawing_Objects;

--  GNAVI: No user changes may be made here
--
--  NB: This is a manual fake of what GNAVI should create once it is working.
--      We need this for building GNAVI_IDE first!
--      GdM July 2012

separate (GNAVI_Edit_Window_Package)

procedure On_Create (Window : in out GNAVI_Edit_Window_Type)
is
begin
   Do_Create(Window);  --  Guess-worked correct action
end On_Create;

