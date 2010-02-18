with GWenerator_Resource_GUI;

with GWens;                             use GWens;

with GWindows;                          use GWindows;
with GWindows.Drawing_Objects;
with GWindows.Buttons.Graphic;

with Interfaces.C;
with Windows_pipes;

package GWen_Windows is

  type GWen_Window_Type is new GWenerator_Resource_GUI.Main_dialog_Type with record
    proj               : GWens.GWen;
    short_name         : GString_Unbounded;
    menus              : GWenerator_Resource_GUI.Main_menu_Type;
    RC_new             : Boolean:= False;
    Ada_new            : Boolean:= False;
    last_save_success  : Boolean;
    ear, no_ear,
    wheels             : GWindows.Drawing_Objects.Bitmap_Type;
    more_details,
    less_details,
    more_build,
    less_build         : GWindows.Drawing_Objects.Bitmap_Type;
    details_button,
    ada_build_button   : GWindows.Buttons.Graphic.Bitmap_Button_type;
    build_process      : Windows_pipes.Piped_process;
    last_seen_running  : Boolean:= False;
  end record;

  --------------------------------------------
  -- Overriden methods for GWen_Window_Type --
  --------------------------------------------

  procedure On_Create (Window : in out GWen_Window_Type);
  --  Handles setting up icons, menus, etc.

  procedure On_Menu_Select
    (Window : in out GWen_Window_Type;
     Item   : in     Integer);

  procedure On_Destroy (Window : in out GWen_Window_Type);

  procedure On_Pre_Create (Window    : in out GWen_Window_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned);

  procedure On_Close (Window : in out GWen_Window_Type;
                      Can_Close :    out Boolean);

  procedure On_Message (Window       : in out GWen_Window_Type;
                        message      : in     Interfaces.C.unsigned;
                        wParam       : in     Interfaces.C.int;
                        lParam       : in     Interfaces.C.int;
                        Return_Value : in out Interfaces.C.long);

  --------------------------------------
  -- New methods for GWen_Window_Type --
  --------------------------------------

  procedure On_New (Window : in out GWen_Window_Type);
  procedure On_Open (Window : in out GWen_Window_Type);
  procedure On_Save (Window : in out GWen_Window_Type);
  procedure On_Save_As (Window : in out GWen_Window_Type);
  procedure On_Options (Window : in out GWen_Window_Type);
  procedure On_About (Window : in out GWen_Window_Type);

end GWen_Windows;
