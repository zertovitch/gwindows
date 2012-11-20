------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                    G W I N D O W S . W I N D O W S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2012 David Botton                   --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. It is distributed in the hope that it will be useful,  but WITHOUT --
-- ANY WARRANTY;  without  even the  implied warranty of MERCHANTABILITY or --
-- FITNESS FOR A PARTICULAR PURPOSE.    See the GNU General  Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- More information about GWindows and the latest current release can       --
-- be located on the web at one of the following places:                    --
--   http://sf.net/projects/gnavi/                                          --
--   http://www.gnavi.org/gwindows                                          --
--   http://www.adapower.com/gwindows                                       --
--                                                                          --
------------------------------------------------------------------------------

with Interfaces.C;

with GWindows.Base;
with GWindows.Menus;
with GWindows.Types;
with GWindows.Drawing;
with GWindows.Drawing_Objects;
with GWindows.Constants;
with GWindows.Cursors;
with GWindows.Colors;

package GWindows.Windows is

   -------------------------------------------------------------------------
   --  Window_Type
   -------------------------------------------------------------------------
   --  Window_Type is the base for all Win32 based windows, dialogs,
   --  windows based custom controls, etc.

   type Window_Type is new GWindows.Base.Base_Window_Type with private;
   type Window_Access is access all Window_Type;
   type Pointer_To_Window_Class is access all Window_Type'Class;

   -------------------------------------------------------------------------
   --  Window_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Window     : in out Window_Type;
      Title      : in     GString     := "";
      Left       : in     Integer     := GWindows.Constants.Use_Default;
      Top        : in     Integer     := GWindows.Constants.Use_Default;
      Width      : in     Integer     := GWindows.Constants.Use_Default;
      Height     : in     Integer     := GWindows.Constants.Use_Default;
      Is_Dynamic : in     Boolean     := False;
      CClass     : in     GString     := "");
   --  Create a regular overlapped window
   --  Note: CClass is used to select a custom Win32 class. It should only
   --        be used with full understanding of the framework.

   procedure Create_Child
     (Window     : in out Window_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Title      : in     GString                              := "";
      Left       : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Top        : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Width      : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Height     : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Is_Dynamic : in     Boolean                              := False;
      CClass     : in     GString                              := "");
   --  Create a child window
   --  Note: CClass is used to select a custom Win32 class. It should only
   --        be used with full understanding of the framework.

   procedure Create_As_Dialog
     (Window      : in out Window_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString                              := "";
      Left        : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Top         : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Width       : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Height      : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Help_Button : in     Boolean                              := False;
      Is_Dynamic  : in     Boolean                              := False);
   procedure Create_As_Dialog
     (Window      : in out Window_Type;
      Title       : in     GString     := "";
      Left        : in     Integer     :=
        GWindows.Constants.Use_Default;
      Top         : in     Integer     :=
        GWindows.Constants.Use_Default;
      Width       : in     Integer     :=
        GWindows.Constants.Use_Default;
      Height      : in     Integer     :=
        GWindows.Constants.Use_Default;
      Help_Button : in     Boolean     := False;
      Is_Dynamic  : in     Boolean     := False);
   --  Create a window styled as a Dialog Box

   procedure Create_As_Tool_Window
     (Window     : in out Window_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Title      : in     GString                              := "";
      Left       : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Top        : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Width      : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Height     : in     Integer                              :=
        GWindows.Constants.Use_Default;
      Is_Dynamic : in     Boolean                              := False);
   --  Create a window styled as a Tool Window

   procedure Create_As_Control
     (Window     : in out Window_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Title      : in     GString                              := "";
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      All_Keys   : in     Boolean                              := False;
      Container  : in     Boolean                              := True;
      ID         : in     Integer                              := 0;
      Styles     : in     Interfaces.C.unsigned                := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  Creates as a control instead of a window
   --
   --  If messages for all keys are desired (ie. that control navigation
   --  keys will be passed to Window instead of handled by framework) then
   --  All_Keys = True
   --  If control will contain other controls then Container = True

   procedure Create_MDI_Top
     (Window     : in out Window_Type;
      Title      : in     GString     := "";
      Left       : in     Integer     := GWindows.Constants.Use_Default;
      Top        : in     Integer     := GWindows.Constants.Use_Default;
      Width      : in     Integer     := GWindows.Constants.Use_Default;
      Height     : in     Integer     := GWindows.Constants.Use_Default;
      Is_Dynamic : in     Boolean     := False;
      CClass     : in     GString     := "");
   --  Create as an MDI Top Window
   --  When using MDI windows you should not have any menus with command
   --  IDs from 5000 to the maximum number of MDI child windows you will
   --  use.
   --  For details on creating an MDI application, see the MDI sample

   procedure Create_MDI_Child
     (Window     : in out Window_Type;
      Parent     : in     Window_Type'Class;
      Title      : in     GString           := "";
      Left       : in     Integer           := GWindows.Constants.Use_Default;
      Top        : in     Integer           := GWindows.Constants.Use_Default;
      Width      : in     Integer           := GWindows.Constants.Use_Default;
      Height     : in     Integer           := GWindows.Constants.Use_Default;
      Is_Dynamic : in     Boolean           := False;
      CClass     : in     GString           := "");
   --  Create as an MDI child window
   --  For details on creating an MDI application, see the MDI sample

   procedure Create_Dialog
     (Window     : in out Window_Type;
      Name       : in     GString;
      Is_Dynamic : in     Boolean                := False);
   --  Create window from dialog resource template
   --  (use #XXXX for numeric resource)

   procedure Create_Dialog
     (Window     : in out Window_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Name       : in     GString;
      Is_Dynamic : in     Boolean                := False);
   --  Create window from dialog resource template as a child of Parent
   --  (use #XXXX for numeric resource)

   -------------------------------------------------------------------------
   --  Window_Type - Properties
   -------------------------------------------------------------------------

   procedure Background_Color (Window : in out Window_Type;
                               Color  : in     GWindows.Colors.Color_Type);
   function Background_Color (Window : in Window_Type)
                             return GWindows.Colors.Color_Type;
   --  The default color is the system color: COLOR_BTNFACE

   procedure Zoom (Window : in Window_Type;
                   State  : in Boolean     := True);
   function Zoom (Window : in Window_Type) return Boolean;
   --  Zoom (Maximized) state of window

   procedure Iconic (Window : in out Window_Type;
                     State  : in     Boolean     := True);
   function Iconic (Window : in Window_Type) return Boolean;
   --  Iconic (Minimized) state of window

   procedure Large_Icon (Window  : in Window_Type;
                         Name    : in GString);
   procedure Large_Icon (Window  : in Window_Type;
                         Icon    : in Drawing_Objects.Icon_Type);
   procedure Small_Icon (Window  : in Window_Type;
                         Name    : in GString);
   procedure Small_Icon (Window  : in Window_Type;
                         Icon    : in Drawing_Objects.Icon_Type);
   --  Window Icon
   --  To specify a numeric resource use #XXXX where XXXX is the resource ID

   procedure Accept_File_Drag_And_Drop (Window : Window_Type;
                                        State  : Boolean     := True);
   --  Turns on acceptance of files dragged on to this window

   procedure Menu (Window      : in Window_Type;
                   Name        : in GString;
                   Destroy_Old : in Boolean           := True);
   --  Set menu for window from resource Name
   --  To specify a numeric resource use #XXXX where XXXX is the resource ID

   procedure Menu (Window      : in Window_Type;
                   Menu        : in GWindows.Menus.Menu_Type;
                   Destroy_Old : in Boolean                  := True);

   function Menu (Window : in Window_Type)
                 return GWindows.Menus.Menu_Type;
   --  Window Menu

   procedure MDI_Menu (Window      : in out Window_Type;
                       Menu        : in     GWindows.Menus.Menu_Type;
                       Window_Menu : in     Positive);
   --  Sets the MDI Top window menu and uses the sub menu Window_Menu for
   --  MDI window list

   procedure MDI_Active_Window
     (Window : in Window_Type;
      Child  : in GWindows.Base.Base_Window_Type'Class);
   function MDI_Active_Window
     (Window : in Window_Type)
     return GWindows.Base.Pointer_To_Base_Window_Class;
   --  MDI window

   procedure Default_Cursor (Window : in out Window_Type;
                             Cursor : in     GWindows.Cursors.Cursor_Type);
   --  Set the default cursor display when mouse is over this Window

   procedure Default_Standard_Cursor (Window : in out Window_Type;
                                      Cursor : in     Integer);
   --  Set the default cursor display to a standard system cursor
   --  from GWindows.Cursors

   type Scroll_Bar_Type is (Horizontal, Vertical);

   procedure Scroll_Range
     (Window  : in Window_Type;
      Bar     : in Scroll_Bar_Type;
      Minimum : in Integer;
      Maximum : in Integer);

   procedure Scroll_Maximum
     (Window  : in Window_Type;
      Bar     : in Scroll_Bar_Type;
      Maximum : in Integer);

   function Scroll_Maximum
     (Window : in Window_Type;
      Bar    : in Scroll_Bar_Type) return Integer;

   procedure Scroll_Minimum
     (Window  : in Window_Type;
      Bar     : in Scroll_Bar_Type;
      Minimum : in Integer);

   function Scroll_Minimum
     (Window : in Window_Type;
      Bar    : in Scroll_Bar_Type) return Integer;
   --  Scroll Range Property

   procedure Scroll_Position
     (Window   : in Window_Type;
      Bar      : in Scroll_Bar_Type;
      Position : in Integer);

   function Scroll_Position
     (Window : in Window_Type;
      Bar    : in Scroll_Bar_Type) return Integer;
   --  Scroll Position

   procedure Scroll_Page_Size
     (Window   : in Window_Type;
      Bar      : in Scroll_Bar_Type;
      Size     : in Natural);

   function Scroll_Page_Size
     (Window : in Window_Type;
      Bar    : in Scroll_Bar_Type) return Natural;
   --  Scroll Page_Size

   function Scroll_Drag_Position
     (Window : in Window_Type;
      Bar    : in Scroll_Bar_Type) return Integer;
   --  Scroll position during a Thumb_Drag scroll event

   -------------------------------------------------------------------------
   --  Window_Type - Methods
   -------------------------------------------------------------------------

   procedure Menu_Refresh (Window : in Window_Type);
   --  If a change is made to menus attached to a window, a Menu_Refresh
   --  should be called

   procedure Display_Context_Menu (Window   : in Window_Type;
                                   Name     : in GString;
                                   Sub_Menu : in Natural;
                                   X        : in Integer;
                                   Y        : in Integer);

   procedure Display_Context_Menu (Window   : in Window_Type;
                                   Menu     : in GWindows.Menus.Menu_Type;
                                   Sub_Menu : in Natural;
                                   X        : in Integer;
                                   Y        : in Integer);

   --  Display a context menu (aka a right click menu)
   --  Name is for a menu resource. Sub_Menu is the 1 based index of the
   --  popup menu below the menu bar to be used or 0 if Menu is a popup
   --  instead of a menu bar
   --  NOTE: Coordinates are relative to desktop

   procedure MDI_Tile_Horizontal (Window : in Window_Type);
   --  Tells MDI_Top window to tile its children horizontaly

   procedure MDI_Tile_Vertical (Window : in Window_Type);
   --  Tells MDI_Top window to tile its children Verticaly

   procedure MDI_Cascade (Window : in Window_Type);
   --  Tells MDI_Top window to tile its children

   procedure MDI_Arrange_Icons (Window : in Window_Type);
   --  Tells MDI_Top window to arrange icons of its children

   procedure MDI_Close_All (Window : in out Window_Type);
   --  Closes all MDI child windows

   procedure Dock_Children (Window : in Window_Type);
   --  Lays out child windows based on their Docking property

   -------------------------------------------------------------------------
   --  Window_Type - Event Types
   -------------------------------------------------------------------------

   type Mouse_Keys is (Left_Button,
                       Middle_Button,
                       Right_Button,
                       Control,
                       Shift);

   type Mouse_Key_States is array (Mouse_Keys'Range) of Boolean;

   type Special_Key_Type is
     (Control,
      Shift,
      Escape,
      Pause,
      Caps_Lock,
      Page_Up,
      Page_Down,
      End_Key,
      Home_Key,
      Left_Key,
      Up_Key,
      Right_Key,
      Down_Key,
      Print_Screen,
      Insert,
      Delete,
      F1, F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12,
      Number_Lock,
      Scroll_Lock,
      None);

   type Hover_Item_Type is (Menu_Item, Menu, System_Menu, Closed);

   type Location_Event is access
     procedure (Window : in out GWindows.Base.Base_Window_Type'Class;
                Left   : in     Integer;
                Top    : in     Integer);

   type Size_Event is access
     procedure (Window : in out GWindows.Base.Base_Window_Type'Class;
                Width  : in     Integer;
                Height : in     Integer);

   type Mouse_Event is access
     procedure (Window : in out GWindows.Base.Base_Window_Type'Class;
                X      : in     Integer;
                Y      : in     Integer;
                Keys   : in     Mouse_Key_States);

   type Wheel_Event is access
     procedure (Window  : in out GWindows.Base.Base_Window_Type'Class;
                X       : in     Integer;
                Y       : in     Integer;
                Keys    : in     Mouse_Key_States;
                Z_Delta : in Integer);

   type Character_Event is access
     procedure (Window      : in out GWindows.Base.Base_Window_Type'Class;
                Special_Key : in     Special_Key_Type;
                Value       : in     GCharacter);

   type Hover_Event is access
     procedure (Window : in out GWindows.Base.Base_Window_Type'Class;
                Item   : in     Integer;
                Kind   : in     Hover_Item_Type);

   type Select_Event is access
     procedure (Window : in out GWindows.Base.Base_Window_Type'Class;
                Item   : in     Integer);

   type Paint_Event is access
     procedure (Window : in out GWindows.Base.Base_Window_Type'Class;
                Canvas : in out GWindows.Drawing.Canvas_Type;
                Area   : in     GWindows.Types.Rectangle_Type);

   type Close_Event is access
     procedure (Window    : in out GWindows.Base.Base_Window_Type'Class;
                Can_Close :    out Boolean);

   type Array_Of_File_Names is
     array (Natural range <>) of GWindows.GString_Unbounded;
   Empty_Array_Of_File_Names : constant Array_Of_File_Names;
   type Array_Of_File_Names_Access is access Array_Of_File_Names;

   type File_Drop_Event is access
     procedure (Window     : in out GWindows.Base.Base_Window_Type'Class;
                File_Names : in     Array_Of_File_Names);

   -------------------------------------------------------------------------
   --  Window_Type - Event Handlers
   -------------------------------------------------------------------------
   --  See Event Methods for details on each event

   procedure On_Focus_Handler (Window  : in out Window_Type;
                               Handler : in GWindows.Base.Action_Event);
   procedure Fire_On_Focus (Window : in out Window_Type);

   procedure On_Lost_Focus_Handler (Window  : in out Window_Type;
                                    Handler : in GWindows.Base.Action_Event);
   procedure Fire_On_Lost_Focus (Window : in out Window_Type);

   procedure On_Size_Handler (Window  : in out Window_Type;
                              Handler : in Size_Event);
   procedure Fire_On_Size (Window : in out Window_Type;
                           Width  : in     Integer;
                           Height : in     Integer);

   procedure On_Move_Handler (Window  : in out Window_Type;
                              Handler : in Location_Event);
   procedure Fire_On_Move (Window : in out Window_Type;
                           Left   : in     Integer;
                           Top    : in     Integer);

   procedure On_Show_Handler (Window  : in out Window_Type;
                              Handler : in GWindows.Base.Action_Event);
   procedure Fire_On_Show (Window : in out Window_Type);

   procedure On_Hide_Handler (Window  : in out Window_Type;
                               Handler : in GWindows.Base.Action_Event);
   procedure Fire_On_Hide (Window : in out Window_Type);

   procedure On_Mouse_Move_Handler (Window  : in out Window_Type;
                                    Handler : in     Mouse_Event);
   procedure Fire_On_Mouse_Move (Window : in out Window_Type;
                                 X      : in     Integer;
                                 Y      : in     Integer;
                                 Keys   : in     Mouse_Key_States);

   procedure On_Mouse_Wheel_Handler (Window  : in out Window_Type;
                                     Handler : in     Wheel_Event);
   procedure Fire_On_Mouse_Wheel (Window  : in out Window_Type;
                                  X       : in     Integer;
                                  Y       : in     Integer;
                                  Keys    : in     Mouse_Key_States;
                                  Z_Delta : in Integer);

   procedure On_Left_Mouse_Button_Down_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event);
   procedure Fire_On_Left_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   procedure On_Left_Mouse_Button_Up_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event);
   procedure Fire_On_Left_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   procedure On_Left_Mouse_Button_Double_Click_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event);
   procedure Fire_On_Left_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   procedure On_Right_Mouse_Button_Down_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event);
   procedure Fire_On_Right_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   procedure On_Right_Mouse_Button_Up_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event);
   procedure Fire_On_Right_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   procedure On_Right_Mouse_Button_Double_Click_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event);
   procedure Fire_On_Right_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   procedure On_Middle_Mouse_Button_Down_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event);
   procedure Fire_On_Middle_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   procedure On_Middle_Mouse_Button_Up_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event);
   procedure Fire_On_Middle_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   procedure On_Middle_Mouse_Button_Double_Click_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event);
   procedure Fire_On_Middle_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   procedure On_NC_Left_Mouse_Button_Down_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event);
   procedure Fire_On_NC_Left_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   procedure On_NC_Left_Mouse_Button_Up_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event);
   procedure Fire_On_NC_Left_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   procedure On_NC_Left_Mouse_Button_Double_Click_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event);
   procedure Fire_On_NC_Left_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   procedure On_NC_Right_Mouse_Button_Down_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event);
   procedure Fire_On_NC_Right_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   procedure On_NC_Right_Mouse_Button_Up_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event);
   procedure Fire_On_NC_Right_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   procedure On_NC_Right_Mouse_Button_Double_Click_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event);
   procedure Fire_On_NC_Right_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   procedure On_NC_Middle_Mouse_Button_Down_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event);
   procedure Fire_On_NC_Middle_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   procedure On_NC_Middle_Mouse_Button_Up_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event);
   procedure Fire_On_NC_Middle_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   procedure On_NC_Middle_Mouse_Button_Double_Click_Handler
     (Window  : in out Window_Type;
      Handler : in     Mouse_Event);
   procedure Fire_On_NC_Middle_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   procedure On_Character_Up_Handler (Window  : in out Window_Type;
                                      Handler : in Character_Event);
   procedure Fire_On_Character_Up (Window      : in out Window_Type;
                                   Special_Key : in     Special_Key_Type;
                                   Value       : in     GCharacter);

   procedure On_Character_Down_Handler (Window  : in out Window_Type;
                                        Handler : in Character_Event);
   procedure Fire_On_Character_Down (Window      : in out Window_Type;
                                     Special_Key : in     Special_Key_Type;
                                     Value       : in     GCharacter);

   procedure On_Menu_Hover_Handler (Window  : in out Window_Type;
                                    Handler : in     Hover_Event);
   procedure Fire_On_Menu_Hover (Window : in out Window_Type;
                                 Item   : in     Integer;
                                 Kind   : in     Hover_Item_Type);

   procedure On_Menu_Select_Handler (Window  : in out Window_Type;
                                     Handler : in Select_Event);
   procedure Fire_On_Menu_Select (Window : in out Window_Type;
                                  Item   : in     Integer);

   procedure On_Accelerator_Select_Handler (Window  : in out Window_Type;
                                     Handler : in Select_Event);
   procedure Fire_On_Accelerator_Select (Window : in out Window_Type;
                                  Item   : in     Integer);

   procedure On_Paint_Handler (Window  : in out Window_Type;
                               Handler : in Paint_Event);
   procedure Fire_On_Paint (Window : in out Window_Type;
                            Canvas : in out GWindows.Drawing.Canvas_Type;
                            Area   : in     GWindows.Types.Rectangle_Type);

   procedure On_Erase_Background_Handler (Window  : in out Window_Type;
                                          Handler : in Paint_Event);
   procedure Fire_On_Erase_Background
     (Window : in out Window_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type);

   procedure On_Change_Cursor_Handler (Window  : in out Window_Type;
                              Handler : in GWindows.Base.Action_Event);
   procedure Fire_On_Change_Cursor (Window : in out Window_Type);

   procedure On_Close_Handler (Window  : in out Window_Type;
                               Handler : in Close_Event);
   procedure Fire_On_Close (Window    : in out Window_Type;
                            Can_Close :    out Boolean);

   procedure On_MDI_Activate_Handler (Window  : in out Window_Type;
                               Handler : in GWindows.Base.Action_Event);
   procedure Fire_On_MDI_Activate (Window : in out Window_Type);

   procedure On_MDI_Deactivate_Handler (Window  : in out Window_Type;
                               Handler : in GWindows.Base.Action_Event);
   procedure Fire_On_MDI_Deactivate (Window : in out Window_Type);

   procedure On_File_Drop_Handler (Window  : in out Window_Type;
                                   Handler : in File_Drop_Event);
   procedure Fire_On_File_Drop (Window     : in out Window_Type;
                                File_Names : in     Array_Of_File_Names);

   -------------------------------------------------------------------------
   --  Window_Type - Events
   -------------------------------------------------------------------------

   procedure On_Focus (Window : in out Window_Type);
   --  Window received focus

   procedure On_Lost_Focus (Window : in out Window_Type);
   --  Window lost focus

   procedure On_Size (Window : in out Window_Type;
                      Width  : in     Integer;
                      Height : in     Integer);
   --  Window size has changed (Width and Height are of client area)
   --  Handles resizing of MDI Client Area and Docking of Children

   procedure On_Move (Window : in out Window_Type;
                      Left   : in     Integer;
                      Top    : in     Integer);
   --  Window has moved.

   procedure On_Show (Window : in out Window_Type);
   --  Window has been shown

   procedure On_Hide (Window : in out Window_Type);
   --  Window has been hidden

   procedure On_Mouse_Move (Window : in out Window_Type;
                            X      : in     Integer;
                            Y      : in     Integer;
                            Keys   : in     Mouse_Key_States);
   --  Mouse has moved

   procedure On_Mouse_Wheel (Window  : in out Window_Type;
                             X       : in     Integer;
                             Y       : in     Integer;
                             Keys    : in     Mouse_Key_States;
                             Z_Delta : in     Integer);
   --  Mouse wheel has moved

   procedure On_Left_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);
   --  Left mouse button pushed down at (X,Y)

   procedure On_Right_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);
   --  Right mouse button pushed down at (X,Y)

   procedure On_Middle_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   --  Middle mouse button pushed down at (X,Y)

   procedure On_Left_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);
   --  Left mouse button released at (X,Y)

   procedure On_Right_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);
   --  Right mouse button released at (X,Y)

   procedure On_Middle_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);
   --  Middle mouse button released at (X,Y)

   procedure On_Left_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);
   --  Left mouse button double clicked at (X,Y)

   procedure On_Right_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);
   --  Right mouse button double clicked at (X,Y)

   procedure On_Middle_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);
   --  Middle mouse button double clicked at (X,Y)

   procedure On_NC_Left_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);
   --  Left mouse button pushed down at (X,Y)

   procedure On_NC_Right_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);
   --  Right mouse button pushed down at (X,Y)

   procedure On_NC_Middle_Mouse_Button_Down
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);

   --  Middle mouse button pushed down at (X,Y)

   procedure On_NC_Left_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);
   --  Left mouse button released at (X,Y)

   procedure On_NC_Right_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);
   --  Right mouse button released at (X,Y)

   procedure On_NC_Middle_Mouse_Button_Up
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);
   --  Middle mouse button released at (X,Y)

   procedure On_NC_Left_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);
   --  Left mouse button double clicked at (X,Y)

   procedure On_NC_Right_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);
   --  Right mouse button double clicked at (X,Y)

   procedure On_NC_Middle_Mouse_Button_Double_Click
     (Window : in out Window_Type;
      X      : in     Integer;
      Y      : in     Integer;
      Keys   : in     Mouse_Key_States);
   --  Middle mouse button double clicked at (X,Y)

   procedure On_Character_Down (Window      : in out Window_Type;
                                Special_Key : in     Special_Key_Type;
                                Value       : in     GCharacter);
   procedure On_Character_Up (Window      : in out Window_Type;
                              Special_Key : in     Special_Key_Type;
                              Value       : in     GCharacter);
   --  If Special_Key is 'None', Key equals the character value of the
   --  key combination pressed (eg. 'A', Esc, ctrl-A, etc.) other wise it
   --  is null. The F10 key is absorbed by the OS for many window types to
   --  generate context sensitive help messages. The function keys F11 and
   --  above are only captured if the input focus (GWindows.Base.Focus) is
   --  on the window.

   procedure On_Menu_Hover (Window : in out Window_Type;
                            Item   : in     Integer;
                            Kind   : in     Hover_Item_Type);
   --  Called when the mouse is over a menu item. If Kind is Menu then Item
   --  is the sub menu index

   procedure On_Menu_Select (Window : in out Window_Type;
                             Item   : in     Integer);
   --  Called when an item is selected by a menu item

   procedure On_Accelerator_Select (Window : in out Window_Type;
                                    Item   : in     Integer);
   --  Called when an accelerator key is hit
   --  Default implementation calls On_Menu_Select with the Item

   procedure On_Paint (Window : in out Window_Type;
                       Canvas : in out GWindows.Drawing.Canvas_Type;
                       Area   : in     GWindows.Types.Rectangle_Type);
   --  Called when application requests painting of the client area
   --  Area is area that needs painting

   procedure On_Erase_Background
     (Window : in out Window_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type);
   --  Called when background needs to be painted. By default it paints
   --  the background with the Background_Color property

   procedure On_Close (Window    : in out Window_Type;
                       Can_Close :    out Boolean);
   --  Called to request if window may close

   procedure On_MDI_Activate (Window : in out Window_Type);
   --  Called when MDI child window is activated and can be used to set
   --  the menu for the current child window.

   procedure On_MDI_Deactivate (Window : in out Window_Type);
   --  Called when MDI child window is deactivated and can be used to set
   --  the menu back to the main MDI window.

   procedure On_File_Drop (Window     : in out Window_Type;
                           File_Names : in     Array_Of_File_Names);
   --  Called when Accept_File_Drag_And_Drop has been called with a true
   --  state and files have been dragged on to the application

   procedure On_Change_Cursor (Window : in out Window_Type);
   --  Called when mouse enters client area to set cursor for window

   -------------------------------------------------------------------------
   --  Window_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  These should be overiden with caution and only with a full
   --  understanding of the internals of the entire GWindows framework

   procedure On_Message (Window       : in out Window_Type;
                         message      : Interfaces.C.unsigned;
                         wParam       : GWindows.Types.Wparam;
                         lParam       : GWindows.Types.Lparam;
                         Return_Value : in out GWindows.Types.Lresult);
   --  Handles additional Win32 messages for top level Windows

   procedure On_Horizontal_Scroll
     (Window  : in out Window_Type;
      Request : GWindows.Base.Scroll_Request_Type;
      Control : GWindows.Base.Pointer_To_Base_Window_Class);
   --  Handles basic processing of Horizontal Scroll Bar

   procedure On_Vertical_Scroll
     (Window  : in out Window_Type;
      Request : GWindows.Base.Scroll_Request_Type;
      Control : GWindows.Base.Pointer_To_Base_Window_Class);
   --  Handles basic process of Veritcal Scroll Bar

   procedure On_Command
     (Window  : in out Window_Type;
      Code    : in     Integer;
      ID      : in     Integer;
      Control : in     GWindows.Base.Pointer_To_Base_Window_Class);
   --  Handles menu and accelerator selections

   procedure Run_Mode (Window : in out Window_Type;
                       Value  : in     GWindows.Base.Run_Mode_Type);
   --  Overide to catch development mode changes

private
   type Window_Type is new GWindows.Base.Base_Window_Type with
      record
         On_Focus_Event          : GWindows.Base.Action_Event   := null;
         On_Lost_Focus_Event     : GWindows.Base.Action_Event   := null;
         On_Size_Event           : Size_Event                   := null;
         On_Move_Event           : Location_Event               := null;
         On_Show_Event           : GWindows.Base.Action_Event   := null;
         On_Hide_Event           : GWindows.Base.Action_Event   := null;
         On_Close_Event          : Close_Event                  := null;
         On_MDI_Activate_Event   : GWindows.Base.Action_Event   := null;
         On_MDI_Deactivate_Event : GWindows.Base.Action_Event   := null;
         On_File_Drop_Event      : File_Drop_Event              := null;

         On_Mouse_Move_Event                          : Mouse_Event := null;
         On_Mouse_Wheel_Event                         : Wheel_Event := null;
         On_Left_Mouse_Button_Down_Event              : Mouse_Event := null;
         On_Left_Mouse_Button_Up_Event                : Mouse_Event := null;
         On_Left_Mouse_Button_Double_Click_Event      : Mouse_Event := null;
         On_Right_Mouse_Button_Down_Event             : Mouse_Event := null;
         On_Right_Mouse_Button_Up_Event               : Mouse_Event := null;
         On_Right_Mouse_Button_Double_Click_Event     : Mouse_Event := null;
         On_Middle_Mouse_Button_Down_Event            : Mouse_Event := null;
         On_Middle_Mouse_Button_Up_Event              : Mouse_Event := null;
         On_Middle_Mouse_Button_Double_Click_Event    : Mouse_Event := null;
         On_NC_Left_Mouse_Button_Down_Event           : Mouse_Event := null;
         On_NC_Left_Mouse_Button_Up_Event             : Mouse_Event := null;
         On_NC_Left_Mouse_Button_Double_Click_Event   : Mouse_Event := null;
         On_NC_Right_Mouse_Button_Down_Event          : Mouse_Event := null;
         On_NC_Right_Mouse_Button_Up_Event            : Mouse_Event := null;
         On_NC_Right_Mouse_Button_Double_Click_Event  : Mouse_Event := null;
         On_NC_Middle_Mouse_Button_Down_Event         : Mouse_Event := null;
         On_NC_Middle_Mouse_Button_Up_Event           : Mouse_Event := null;
         On_NC_Middle_Mouse_Button_Double_Click_Event : Mouse_Event := null;

         On_Character_Up_Event   : Character_Event := null;
         On_Character_Down_Event : Character_Event := null;

         On_Menu_Hover_Event         : Hover_Event  := null;
         On_Menu_Select_Event        : Select_Event := null;
         On_Accelerator_Select_Event : Select_Event := null;

         On_Paint_Event            : Paint_Event                  := null;
         On_Erase_Background_Event : Paint_Event                  := null;
         On_Change_Cursor_Event    : GWindows.Base.Action_Event   := null;

         All_Keys : Boolean := False;

         Font_Handle : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         Default_Cursor       : GWindows.Cursors.Cursor_Type := 0;
         Background_Color     : GWindows.Colors.Color_Type;
         Background_Color_Sys : Boolean := True;
      end record;

   Empty_Array_Of_File_Names : constant Array_Of_File_Names :=
      (0 .. -1 => Null_GString_Unbounded);

end GWindows.Windows;
