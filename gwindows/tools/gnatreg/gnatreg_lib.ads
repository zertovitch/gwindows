------------------------------------------------------------------------------
--                                                                          --
--          GNATREG - Win32 GNAT Standard Library Registry Tool             --
--                                                                          --
--                          G N A T R E G _ L I B                           --
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
-- More information about GWINDOWS and the most current public version can  --
-- be located on the web at http://www.adapower.com/gwindows                --
--                                                                          --
------------------------------------------------------------------------------

with GWindows.Registry;
with GWindows;

package GNATREG_Lib is

   procedure Set_Library (Library, Path : GWindows.GString);
   --  Set Library as a standard library with path

   procedure Delete_Library (Library : GWindows.GString);
   --  Delete standard library

   type Library_Array is new GWindows.Registry.Value_Name_Array;

   function Get_Libraries return Library_Array;
   --  Get list of libraries

   function Get_Path (Library : GWindows.GString) return GWindows.GString;
   --  Get path of library

end GNATREG_Lib;
