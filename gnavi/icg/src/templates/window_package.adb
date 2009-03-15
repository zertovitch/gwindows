package body @_Window_Name_@_Package is

   procedure On_Create (Window : in out @_Window_Name_@_Type) is separate;

   -------------------------------------------------------------------------
   --  Handlers
   -------------------------------------------------------------------------

--  GNAVI: Create Global Instance
begin
   Create (@_Window_Name_@@_Create_Params_@);
end @_Window_Name_@_Package;
