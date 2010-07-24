------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                    G W I N D O W S . I N T E R N A L                     --
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

--  Internal interfaces to the OS

with GWindows.Base;
with GWindows.GStrings;
with GWindows.Types;
pragma Elaborate_All (GWindows.GStrings);

private package GWindows.Internal is

   pragma Elaborate_Body;

   Window_Class_Name : GString_C :=
     GWindows.GStrings.To_GString_C ("GWindows_Class");
   --  Class name used for all GWindows windows

   GWindows_Object_Property_Name : GString_C :=
     GWindows.GStrings.To_GString_C ("GWindows_Oject");
   --  Name to use for custom property to hold reference to object

   GWindows_Object_Property_Atom : Interfaces.C.unsigned_short := 0;
   --  Atomized version of custom property

   Main_Thread_ID : Interfaces.C.unsigned_long;
   --  Main application thread ID

   Current_hInstance : GWindows.Types.Handle;
   --  Current hInstance

   function Desktop_Width return Natural;

   function Desktop_Height return Natural;

   type Keyboard_Control;
   type Pointer_To_Keyboard_Control is access all Keyboard_Control;

   type Keyboard_Control is
      record
         Window : GWindows.Base.Pointer_To_Base_Window_Class := null;
         Next   : Pointer_To_Keyboard_Control := null;
      end record;

   Top_Keyboard_Control    : Pointer_To_Keyboard_Control := null;
   Bottom_Keyboard_Control : Pointer_To_Keyboard_Control := null;

   procedure Add_Keyboard_Control
     (Window : GWindows.Base.Pointer_To_Base_Window_Class);
   --  Add window to list of windows to handle keyboard control

   procedure Remove_Keyboard_Control
     (Window : GWindows.Base.Pointer_To_Base_Window_Class);
   --  Add window to list of windows to handle keyboard control

end GWindows.Internal;
