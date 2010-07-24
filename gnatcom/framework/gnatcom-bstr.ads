------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                        G N A T C O M . B S T R                           --
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
-- More information about GNATCOM and the most current version can          --
-- be located on the web at http://www.gnavi.org/gnatcom                    --
--                                                                          --
------------------------------------------------------------------------------

--  BSTRs are COM strings. These functions allow easy use in Ada programs
--  BSTRs returned from COM methods and BSTRs created using these methods
--  should be deallocated with the Free procedure

with Interfaces.C;

with GNATCOM.Types;

package GNATCOM.BSTR is

   function Is_Empty (This : GNATCOM.Types.BSTR) return Boolean;
   --  If the BSTR is a null pointer or contains a zero size
   --  returns true

   function Length (Source : GNATCOM.Types.BSTR)
     return Natural;
   --  Length of BSTR even if the BSTR contains embedded nulls

   procedure Free (This : in GNATCOM.Types.BSTR);
   --  Deallocates a BSTR. BSTRs can be created in one process and deallocated
   --  in another.

   function To_BSTR (From : String) return GNATCOM.Types.BSTR;

   function To_BSTR_From_Wide (From : Wide_String) return GNATCOM.Types.BSTR;
   --  Allocates a new BSTR from an Ada String

   function To_BSTR_From_C (From : Interfaces.C.char_array)
                           return GNATCOM.Types.BSTR;

   function To_BSTR_From_Wide_C (From : Interfaces.C.wchar_array)
                                return GNATCOM.Types.BSTR;
   --  Allocates a new BSTR from a C String
   --
   --  Since char_array and wchar_array can not be null strings
   --  use: To_BSTR("") to create a BSTR with a null string.
   --  Most of the time a null can be passed for a BSTR in place of
   --  a BSTR with a null string and is generally preferable.

   function To_Ada (From : GNATCOM.Types.BSTR;
                    Free : Boolean            := True)
                   return String;

   function To_Ada_Wide (From : GNATCOM.Types.BSTR;
                         Free : Boolean            := True)
                        return Wide_String;
   --  Returns an Ada string and deallocates the BSTR if Free is true
   --  Non-standard use of BSTRs, ie. those that contain nulls in the
   --  string contents are not supported by these functions

   function To_C (From : GNATCOM.Types.BSTR;
                  Free : Boolean            := True)
                 return Interfaces.C.char_array;

   function To_C_Wide (From : GNATCOM.Types.BSTR;
                       Free : Boolean            := True)
                      return Interfaces.C.wchar_array;
   --  Returns a C string and dallocates the BSTR if Free is true
   --  The BSTR can not be empty or BSTR_ERROR will be raised
   --  Non-standard use of BSTRs, ie. those that contain nulls in the
   --  string contents are not supported by these functions

   BSTR_ERROR : exception;
   --  Raised when an attempt to convert a null or empty  BSTR
   --  to a C string.

end GNATCOM.BSTR;
