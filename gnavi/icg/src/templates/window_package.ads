with @_Window_Type_Package_@;
with GWindows.Base;

package @_Window_Name_@_Package is

   -------------------------------------------------------------------------
   --  @_Window_Name_@ Specs
   -------------------------------------------------------------------------

   type @_Window_Name_@_Type is
     new @_Window_Type_@ with
      record
         --  GNAVI: Controls
         null;
         --  GNAVI: Add custom data below this comment
      end record;

   type @_Window_Name_@_Access is
     access all @_Window_Name_@_Type;

   type Pointer_To_@_Window_Name_@_Class is
     access all @_Window_Name_@_Type'Class;

   procedure On_Create (Window : in out @_Window_Name_@_Type);

   @_Window_Name_@ : @_Window_Name_@_Type;

   -------------------------------------------------------------------------
   --  Handlers
   -------------------------------------------------------------------------

end @_Window_Name_@_Package;
