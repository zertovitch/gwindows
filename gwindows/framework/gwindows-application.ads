------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                 G W I N D O W S . A P P L I C A T I O N                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2024 David Botton                   --
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
--   https://sourceforge.net/projects/gnavi/                                --
--   https://github.com/zertovitch/gwindows                                 --
--                                                                          --
------------------------------------------------------------------------------

--  Application wide subprograms and general resource management

with Interfaces.C;

with GWindows.Base;
with GWindows.Types;

package GWindows.Application is

   function hInstance return GWindows.Types.Handle;
   --  hInstance of GWindows application.

   procedure Set_hInstance (Process_hInstance : GWindows.Types.Handle);
   --  When running in GWindows in a DLL, Set_hInstance
   --  should be called in the DllMain function with
   --  the process's hInstance.

   function Load_String (ID       : in Interfaces.C.unsigned;
                         Max_Size : in Integer := 256)
   return GString;
   --  Load a string resource at ID of Max_Size.

   procedure Message_Loop;
   --  Start a message loop to handle Windows messages in this thread
   --  Windows requires a message loop to be running in any thread
   --  that creates windows.

   procedure Modal_Loop (Window : in out GWindows.Base.Base_Window_Type'Class);
   --  Creates a modal loop.

   procedure Message_Check;
   --  Does a check to see if there is a windows message pending and if
   --  so process it. This allows for creation of a message loop in
   --  a task select statement.

   procedure End_Loop;
   --  Ends the message loop in this thread.

   procedure End_Application;
   --  Ends message loop in main thread of application.
   --  If additional message loops are still
   --  running, the application will not close down.
   --  To force a close of the application, use GNAT.OS_Lib.OS_Exit (0);

   procedure Show_Modal (Window : in out GWindows.Base.Base_Window_Type'Class);
   --  Display window and return only after window is closed.

   procedure Show_Modal (Window : in out GWindows.Base.Base_Window_Type'Class;
                         Parent : in out GWindows.Base.Base_Window_Type'Class);
   --  Display window, disable parent, and
   --  return only after window is closed.

   function Display_Dialog (Parent : in GWindows.Base.Base_Window_Type'Class;
                            Name   : in GString;
                            Center : in Boolean := True)
                           return Integer;
   function Display_Dialog (Name   : in GString;
                            Center : in Boolean := True)
                           return Integer;
   --  Display a modal dialog box from resource
   --  (use #XXXX for numeric resource) and return modal result

   procedure Display_Dialog
     (Parent : in out GWindows.Base.Base_Window_Type'Class;
      Name   : in GString;
      Center : in Boolean := True);
   procedure Display_Dialog (Name   : in GString;
                             Center : in Boolean := True);
   --  Display a modal dialog box from resource
   --  (use #XXXX for numeric resource) and discard modal result

   procedure Show_Dialog
     (Window : in out GWindows.Base.Base_Window_Type'Class);
   function Show_Dialog
     (Window : in GWindows.Base.Base_Window_Type'Class) return Integer;

   procedure Show_Dialog
     (Window : in out GWindows.Base.Base_Window_Type'Class;
      Parent : in out GWindows.Base.Base_Window_Type'Class);
   function Show_Dialog (Window : in GWindows.Base.Base_Window_Type'Class;
                         Parent : in GWindows.Base.Base_Window_Type'Class)
                        return Integer;
   --  Display dialog and only return after window is closed
   --  Any control that performs its default action (e.g. a click) and
   --  has one of the dialog constant ids or is a Dialog_Button_Type,
   --  will cause the window to close and modal result set to the
   --  Button's ID.

   procedure Detach_From_Console;
   --  You can detach from the console by using this method if started
   --  as a console application
   --
   --  GWindows applications start as console based applications, i.e.
   --  there is a dos box type window available
   --
   --  To start as a GUI app use:
   --       pragma Linker_Options ("-mwindows");

   function Get_Active_Window
     return GWindows.Base.Pointer_To_Base_Window_Class;
   --  Returns the currently active window for this thread

   function Get_Window_At_Location
     (X, Y : Integer)
     return GWindows.Base.Pointer_To_Base_Window_Class;
   --  Returns the window that contains the point (X, Y).
   --  It can be a window of another GWindows application.

   function Get_Window_Text_At_Location (X, Y : Integer) return GString;
   --  Returns the text of the window that contains the point (X, Y).
   --  It can be a window of any application, including the desktop.

   function Get_Window_Class_Name_At_Location (X, Y : Integer) return GString;
   --  Returns the class name of the window that contains the point (X, Y).
   --  It can be a window of any application, including the desktop.

   function Get_Window_Root_Class_Name_At_Location (X, Y : Integer)
      return GString;
   --  Returns the root class name of the window that contains
   --  the point (X, Y).
   --  It can be a window of any application, including the desktop.

   function Is_Desktop_At_Location (X, Y : Integer) return Boolean;

   function Explorer_Path_At_Location (X, Y : Integer) return GString;
   --  Returns the path from a Windows Explorer window (if meaningful),
   --  or the Desktop's path if (X, Y) points to the Desktop.
   --  In any other case, the returned string is empty.

   function Desktop_Width return Natural;
   --  Returns width of desktop (on the primary monitor).

   function Desktop_Height return Natural;
   --  Returns height of desktop (on the primary monitor).

   type Monitor_Dimensions is access
      procedure (Rectangle : GWindows.Types.Rectangle_Type);

   procedure Enumerate_Display_Monitors (M : Monitor_Dimensions);
   --  Reveals the rectangles corresponding to each monitor.
   --  * The primary monitor matches Desktop_Width, Desktop_Height.
   --  * The primary monitor is not always the first monitor
   --      in the enumeration!
   --
   --  A version of Enumerate_Display_Monitors using generics
   --  instead of a call-back causes an unexpected exit with
   --  GNAT GPL 2017, the last 32-bit version of GNAT.

   type Screen_Visibility_Type is (Good, Fair, Poor);

   function Screen_Visibility
     (Left_Top_Corner : Types.Point_Type;
      Minimum_Width   : Positive := 200;
      Minimum_Height  : Positive := 50)
      return Screen_Visibility_Type;
   --  Determines the potential visibility of a window's (left, top) corner.
   --  The check is done for all available monitors.

   procedure Add_To_Recent_Documents (File_Name : GString);
   --  Add a file name to various "recent document" lists
   --  on Windows Explorer / Desktop. For instance, Windows 7+'s Task Bar.

end GWindows.Application;
