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
