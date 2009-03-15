------------------------------------------------------------------------------
--                                                                          --
--                 GBManager - Win32 COM Binding Manager                    --
--                                                                          --
--                       G B M A N A G E R _ L I B                          --
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

with GWindows.GStrings;
with GNATREG_Lib;

package body GBManager_Lib is

   -------------------------
   -- List_Type_Libraries --
   -------------------------

   function List_Type_Libraries return GWindows.Registry.Key_Name_Array is
   begin
      return GWindows.Registry.Get_Sub_Keys
        (Key_Name => "TypeLib",
         Root_Key => GWindows.Registry.HKEY_CLASSES_ROOT);
   end List_Type_Libraries;

   --------------------------------
   -- List_Type_Library_Versions --
   --------------------------------

   function List_Type_Library_Versions
     (LIBIID : GWindows.GString)
     return GWindows.Registry.Key_Name_Array
   is
   begin
      return GWindows.Registry.Get_Sub_Keys
        (Key_Name => "TypeLib\" & LIBIID,
         Root_Key => GWindows.Registry.HKEY_CLASSES_ROOT);
   end List_Type_Library_Versions;

   -----------------------
   -- Type_Library_Name --
   -----------------------

   function Type_Library_Name
     (LIBIID  : GWindows.GString;
      Version : GWindows.GString)
     return GWindows.GString
   is
   begin
      return GWindows.Registry.Get_Value
        (Key_Name => "TypeLib\" & LIBIID & "\" & Version,
         Name     => "",
         Root_Key => GWindows.Registry.HKEY_CLASSES_ROOT);
   end Type_Library_Name;

   ----------------------------
   -- Type_Library_Directory --
   ----------------------------

   function Type_Library_Location
     (LIBIID  : GWindows.GString;
      Version : GWindows.GString)
     return GWindows.GString
   is
      Loc_Vals : GWindows.Registry.Key_Name_Array :=
        GWindows.Registry.Get_Sub_Keys
        (Key_Name => "TypeLib\" & LIBIID & "\" & Version,
         Root_Key => GWindows.Registry.HKEY_CLASSES_ROOT);

   begin
      begin
         return GWindows.Registry.Get_Value
           (Key_Name => "TypeLib\" & LIBIID & "\" & Version &
              "\" & GWindows.GStrings.To_GString_From_Unbounded
                (Loc_Vals (Loc_Vals'First)) &
              "\win32",
            Name     => "",
            Root_Key => GWindows.Registry.HKEY_CLASSES_ROOT);
      exception
         when others =>
            return "";
      end;
   end Type_Library_Location;

   --------------------------
   -- Set_Binding_Location --
   --------------------------

   procedure Set_Binding_Location (Location : GWindows.GString)
   is
   begin
      GWindows.Registry.Register
        (Key_Name => "SOFTWARE\Ada Core Technologies\GBMANAGER",
         Name     => "LOC",
         Value    => Location,
         Root_Key => GWindows.Registry.HKEY_LOCAL_MACHINE);
      GNATREG_Lib.Set_Library ("GBMANAGER_REPOSITORY", Location);
   end Set_Binding_Location;

   --------------------------
   -- Get_Binding_Location --
   --------------------------

   function Get_Binding_Location return GWindows.GString
   is
   begin
      return GWindows.Registry.Get_Value
        (Key_Name => "SOFTWARE\Ada Core Technologies\GBMANAGER",
         Name     => "LOC",
         Root_Key => GWindows.Registry.HKEY_LOCAL_MACHINE);
   exception
      when others =>
         return "";
   end Get_Binding_Location;

   --------------------------------
   -- Change_To_Binding_Location --
   --------------------------------

   procedure Change_To_Binding_Location
   is
      Path : constant GWindows.GString_C :=
        GWindows.GStrings.To_GString_C (Get_Binding_Location);

      procedure SetCurrentDirectory
        (PathName : GWindows.GString_C := Path);
      pragma Import (StdCall, SetCurrentDirectory,
                     "SetCurrentDirectory" &
                     GWindows.Character_Mode_Identifier);
   begin
      SetCurrentDirectory;
   end Change_To_Binding_Location;

end GBManager_Lib;
