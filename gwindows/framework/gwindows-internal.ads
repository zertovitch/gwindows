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
