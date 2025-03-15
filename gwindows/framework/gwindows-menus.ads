------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                        G W I N D O W S . M E N U S                       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2019 David Botton                   --
--                                                                          --
-- MIT License                                                              --
--                                                                          --
-- Permission is hereby granted, free of charge, to any person obtaining    --
-- a copy of this software and associated documentation files (the          --
-- "Software"), to deal in the Software without restriction, including      --
-- without limitation the rights to use, copy, modify, merge, publish,      --
-- distribute, sublicense, and/or sell copies of the Software, and to       --
-- permit persons to whom the Software is furnished to do so, subject to    --
-- the following conditions:                                                --
--                                                                          --
-- The above copyright notice and this permission notice shall be included  --
-- in all copies or substantial portions of the Software.                   --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,          --
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF       --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   --
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY     --
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,     --
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE        --
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                   --
--                                                                          --
-- More information about GWindows and the latest current release can       --
-- be located on the web at one of the following places:                    --
--   http://sf.net/projects/gnavi/                                          --
--   http://www.gnavi.org/gwindows                                          --
--   http://www.adapower.com/gwindows                                       --
--                                                                          --
------------------------------------------------------------------------------

with GWindows.Base;
with GWindows.Cursors;
with GWindows.Types;

package GWindows.Menus is

   -------------------------------------------------------------------------
   --  Menu_Type
   -------------------------------------------------------------------------

   type Menu_Type is new GWindows.Types.Handle;
   Null_Menu : constant Menu_Type := Menu_Type (GWindows.Types.Null_Handle);

   function Load_Menu (Name : in GString) return Menu_Type;
   --  Loads a menu from a resource. If menu is not attached to a window
   --  it should be destroyed before closing the application
   --  To specify a numeric resource use #XXXX where XXXX is the resource ID

   function Create_Menu return Menu_Type;
   --  Create a blank menu bar.  If menu is not attached to a window
   --  it should be destroyed before closing the application

   function Create_Popup return Menu_Type;
   --  Creates a blank pop up to be placed on a menu bar or as a sub menu

   procedure Destroy_Menu (Menu : in out Menu_Type);
   --  Destroys a menu

   function Get_Sub_Menu (Menu     : Menu_Type;
                          Position : Positive)
                         return Menu_Type;
   --  Return a pop up menu in the Menu

   type Location_Type is (Command, Position);
   --  Determines how to look up items in a menu. When using Command
   --  the Command ID of the menu item is looked for, so the Menu passed
   --  in can be the Menu Bar or other menu and sub menus will be looked
   --  up. If Position is used, which menu or submenu is significant as
   --  that is the one affected.

   type State_Type is (Enabled, Disabled, Grayed);

   procedure Append_Menu (Menu     : in Menu_Type;
                          Text     : in GString;
                          Add_Menu : in Menu_Type);
   procedure Append_Menu (Menu     : in Menu_Type;
                          Text     : in GString;
                          Add_Menu : in Positive);
   procedure Append_Menu (Menu     : in Menu_Type;
                          Text     : in GString;
                          Add_Menu : in Menu_Type;
                          State    : in State_Type);
   procedure Append_Menu (Menu     : in Menu_Type;
                          Text     : in GString;
                          Add_Menu : in Positive;
                          State    : in State_Type);
   --  Append menu

   procedure Append_Item (Menu    : in Menu_Type;
                          Text    : in GString;
                          Command : in Positive);
   --  Append item

   procedure Append_Separator (Menu : in Menu_Type);
   --  Append a separator

   procedure Insert_Menu (Menu      : in Menu_Type;
                          Locate_By : in Location_Type;
                          Locate_At : in Positive;
                          Text      : in GString;
                          Add_Menu  : in Menu_Type);
   --  Insert menu

   procedure Insert_Item (Menu      : in Menu_Type;
                          Locate_By : in Location_Type;
                          Locate_At : in Positive;
                          Text      : in GString;
                          Command   : in Positive);
   --  Insert item

   procedure Insert_Separator (Menu : in Menu_Type;
                               Locate_By : in Location_Type;
                               Locate_At : in Positive);
   --  Insert a separator

   procedure Delete_Item (Menu      : in Menu_Type;
                          Locate_By : in Location_Type;
                          Locate_At : in Positive);
   --  Delete a menu item

   function Remove_Menu (Menu     : Menu_Type;
                         Position : Positive)
                        return Menu_Type;
   --  Removes a submenu from a menu and returns it. The returned menu
   --  should be detroyed before progam exit if not attached later
   --  to another menu that is itself destroyed or attached to a window

   procedure State (Menu      : in Menu_Type;
                    Locate_By : in Location_Type;
                    Locate_At : in Positive;
                    State     : in State_Type);

   function State (Menu      : in Menu_Type;
                   Locate_By : in Location_Type;
                   Locate_At : in Positive)
                  return State_Type;
   --  Menu Item State

   procedure Radio_Check  (Menu         : in Menu_Type;
                           Locate_By    : in Location_Type;
                           First_Item   : in Positive;
                           Last_Item    : in Positive;
                           Checked_Item : in Positive);
   --  Check an item in range with a bullet and uncheck all items
   --  in the range.

   procedure Check (Menu      : in Menu_Type;
                    Locate_By : in Location_Type;
                    Locate_At : in Positive;
                    State     : in Boolean);

   function Check (Menu      : in Menu_Type;
                   Locate_By : in Location_Type;
                   Locate_At : in Positive)
                  return Boolean;
   --  Menu Item Check Mark

   procedure Hilite (Menu      : in Menu_Type;
                     Window    : in GWindows.Base.Base_Window_Type'Class;
                     Locate_By : in Location_Type;
                     Locate_At : in Positive;
                     State     : in Boolean);

   function Hilite (Menu      : in Menu_Type;
                    Locate_By : in Location_Type;
                    Locate_At : in Positive)
                  return Boolean;
   --  Menu Item Hilite

   function Count (Menu : in Menu_Type) return Natural;
   --  Number of items in menu

   function Command (Menu     : in Menu_Type;
                     Position : in Positive)
                    return Natural;
   --  Get command id of menu item at position

   procedure Text (Menu      : in Menu_Type;
                   Locate_By : in Location_Type;
                   Locate_At : in Positive;
                   New_Text  : in GString);
   --  Change text at location

   function Text (Menu      : in Menu_Type;
                  Locate_By : in Location_Type;
                  Locate_At : in Positive)
                 return GString;
   --  Text of menu item

   function Text_Length (Menu      : in Menu_Type;
                         Locate_By : in Location_Type;
                         Locate_At : in Positive)
                        return Natural;
   --  Return the length of the text for a menu item

   function Valid (Menu : in Menu_Type) return Boolean;
   --  Returns true if Menu is valid

   --  Execute immediately a popup menu (also called: "pop-up menu",
   --  "context menu", "contextual menu", or "shortcut menu").
   --  Default location is the current mouse cursor position.
   --
   procedure Immediate_Popup_Menu (
      Menu     : Menu_Type;
      Window   : GWindows.Base.Base_Window_Type'Class;
      Location : GWindows.Types.Point_Type :=
                    GWindows.Cursors.Get_Cursor_Position
   );

end GWindows.Menus;
