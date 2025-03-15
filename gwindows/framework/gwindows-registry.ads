------------------------------------------------------------------------------
--                                                                          --
--           GWINDOWS - Ada 95 Framework for Windows Development            --
--                                                                          --
--                   G W I N D O W S . R E G I S T R Y                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2020 David Botton                   --
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
--   https://sourceforge.net/projects/gnavi/                                --
--   https://github.com/zertovitch/gwindows                                 --
--                                                                          --
------------------------------------------------------------------------------

package GWindows.Registry is

   function Current_Directory return GString;
   --  Return the current directory

   function Get_Short_Directory_Name (Long_Directory_Name : GString)
                                     return GString;
   --  Returns the short directory name of a long directory name

   --  Common Root Keys Constants
   HKEY_CLASSES_ROOT     : constant := -2147483648;
   HKEY_CURRENT_USER     : constant := -2147483647;
   HKEY_LOCAL_MACHINE    : constant := -2147483646;
   HKEY_USERS            : constant := -2147483645;
   HKEY_PERFORMANCE_DATA : constant := -2147483644;
   HKEY_CURRENT_CONFIG   : constant := -2147483643;
   HKEY_DYN_DATA         : constant := -2147483642;

   procedure Register (Key_Name, Name, Value : in GString;
                       Root_Key              : in Integer);
   --  Place a Name / Value pair in the Windows NT Registry
   --  A blank name implies the default value for the key
   --  If the Key does not exist it is created

   procedure Delete_Value (Key_Name, Name : in GString;
                           Root_Key       : in Integer);
   --  Delete the Name / Value pair at Key_Name
   --  If the Key does not exist it is created

   type Value_Name_Array is array (Natural range <>) of GString_Unbounded;

   function Get_Value_Names (Key_Name : in GString;
                             Root_Key : in Integer)
                            return Value_Name_Array;
   --  Get list of value names

   function Get_Value (Key_Name, Name : in GString;
                       Root_Key       : in Integer)
                      return GString;
   --  Get value for Name
   --  Use Name = "" for default value

   procedure Unregister (Key_Name : in GString;
                         Root_Key : in Integer);
   --  Removes a key and its Name / Value pairs from the Windows NT Registry
   --  All child keys of a Key_Name must first be removed before Unregister
   --  will remove a Key_Name from the registry.

   type Key_Name_Array is array (Natural range <>) of GString_Unbounded;

   function Get_Sub_Keys (Key_Name : in GString;
                          Root_Key : in Integer)
                         return Key_Name_Array;
   --  Returns a list of all the subkeys under Key_Name

   REGISTRY_ERROR : exception;
   --  Unable to process file name

end GWindows.Registry;
