------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                   G W I N D O W S . R E G I S T R Y                      --
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
