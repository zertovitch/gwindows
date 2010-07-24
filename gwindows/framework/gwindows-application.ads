------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                 G W I N D O W S . A P P L I C A T I O N                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2005 David Botton                   --
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
-- be located on the web at http://www.gnavi.org/gwindows                   --
--                                                                          --
------------------------------------------------------------------------------

--  Application wide subprograms and general resource management

with Interfaces.C;

with GWindows.Base;
with GWindows.Types;

package GWindows.Application is

   function hInstance return GWindows.Types.Handle;
   --  hInstance of GWindows application

   procedure Set_hInstance (hInstance : GWindows.Types.Handle);
   --  When running in GWindows in a DLL, Set_hInstance
   --  should be called in the DllMain function with
   --  the process's hInstance

   function Load_String (ID       : in Interfaces.C.unsigned;
                         Max_Size : in Integer := 256)
                        return GString;
   --  Load a string resource at ID of Max_Size

   procedure Message_Loop;
   --  Start a message loop to handle Windows messages in this thread
   --  Windows requires a message loop to be running in any thread
   --  that creates windows

   procedure Modal_Loop (Window : in out GWindows.Base.Base_Window_Type'Class);
   --  Creates a modal loop

   procedure Message_Check;
   --  Does a check to see if there is a windows message pending and if
   --  so process it. This allows for creation of a message loop in
   --  a task select statement.

   procedure End_Loop;
   --  Ends the message loop in this thread

   procedure End_Application;
   --  Ends message loop in main thread of application
   --  If additional message loops are still running the application
   --  will not close down.
   --  To force a close of the application use GNAT.OS_Lib.Exit (0);

   procedure Show_Modal (Window : in out GWindows.Base.Base_Window_Type'Class);
   --  Display window and return only after window is closed

   procedure Show_Modal (Window : in out GWindows.Base.Base_Window_Type'Class;
                         Parent : in out GWindows.Base.Base_Window_Type'Class);
   --  Display window, disable parent, and return only after window is
   --  closed

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
   --  Returns the window that contains the points X, Y

   function Desktop_Width return Natural;
   --  Returns width of desktop

   function Desktop_Height return Natural;
   --  Returns height of desktop

end GWindows.Application;
