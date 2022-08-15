with GWenerator_Resource_GUI;

with GWens;

with GWindows,
     GWindows.Drawing_Objects,
     GWindows.Types,
     GWindows.Windows;

with Interfaces.C;
with Windows_pipes;

package GWen_Windows is

  type GWen_Window_Type is new GWenerator_Resource_GUI.Main_dialog_Type with record
    proj               : GWens.GWen;
    short_name         : GWindows.GString_Unbounded;
    menus              : GWenerator_Resource_GUI.Main_Menu_Type;
    RC_new             : Boolean := False;
    Ada_new            : Boolean := False;
    last_save_success  : Boolean;
    ear, no_ear,
    wheels             : GWindows.Drawing_Objects.Bitmap_Type;
    more_details,
    less_details,
    more_build,
    less_build         : GWindows.Drawing_Objects.Bitmap_Type;
    build_process      : Windows_pipes.Piped_process;
    last_seen_running  : Boolean := False;
    last_build_failed  : Boolean := False;
  end record;

  --------------------------------------------
  -- Overriden methods for GWen_Window_Type --
  --------------------------------------------

  overriding procedure On_Create (Window : in out GWen_Window_Type);
  --  Handles setting up icons, menus, etc.

  overriding procedure On_Menu_Select
    (Window : in out GWen_Window_Type;
     Item   : in     Integer);

  overriding procedure On_Destroy (Window : in out GWen_Window_Type);

  overriding procedure On_File_Drop
    (Window      : in out GWen_Window_Type;
     File_Names : in     GWindows.Windows.Array_Of_File_Names);

  overriding procedure On_Pre_Create
    (Window    : in out GWen_Window_Type;
     dwStyle   : in out Interfaces.C.unsigned;
     dwExStyle : in out Interfaces.C.unsigned);

  overriding procedure On_Close
    (Window : in out GWen_Window_Type;
     Can_Close :    out Boolean);

  overriding procedure On_Message
    (Window       : in out GWen_Window_Type;
     message      : in     Interfaces.C.unsigned;
     wParam       : in     GWindows.Types.Wparam;
     lParam       : in     GWindows.Types.Lparam;
     Return_Value : in out GWindows.Types.Lresult);

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
