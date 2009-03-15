------------------------------------------------------------------------------
--                                                                          --
--          GNATREG - Win32 GNAT Standard Library Registry Tool             --
--                                                                          --
--                          G N A T R E G _ L I B                           --
--                                                                          --
--                                 B o d y                                  --
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
-- More information about GWINDOWS and the most current public version can  --
-- be located on the web at http://www.adapower.com/gwindows                --
--                                                                          --
------------------------------------------------------------------------------

with GWindows.Registry; use GWindows.Registry;

package body GNATREG_Lib is

   Key_Name : constant GWindows.GString :=
     "SOFTWARE\Ada Core Technologies\GNAT\Standard Libraries";

   --------------------
   -- Delete_Library --
   --------------------

   procedure Delete_Library (Library : GWindows.GString) is
   begin
      Delete_Value (Key_Name, Library, HKEY_LOCAL_MACHINE);
   end Delete_Library;

   -----------------
   -- Set_Library --
   -----------------

   procedure Set_Library (Library, Path : GWindows.GString) is
   begin
      Register (Key_Name, Library, Get_Short_Directory_Name (Path),
                HKEY_LOCAL_MACHINE);
   end Set_Library;

   -------------------
   -- Get_Libraries --
   -------------------

   function Get_Libraries return Library_Array
   is
      Result : Value_Name_Array := Get_Value_Names (Key_Name,
                                                    HKEY_LOCAL_MACHINE);
   begin
      return Library_Array (Result);
   end Get_Libraries;

   --------------
   -- Get_Path --
   --------------

   function Get_Path (Library : GWindows.GString) return GWindows.GString
   is
   begin
      return Get_Value (Key_Name, Library, HKEY_LOCAL_MACHINE);
   end Get_Path;

end GNATREG_Lib;
