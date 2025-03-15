------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                   G W I N D O W S . G S T R I N G S                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2014 David Botton                   --
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

--  Facilities for working with Generic (ANSI or Unicode depending on buid)
--  strings

with GNATCOM.Types;

package GWindows.GStrings is

   function To_GString_From_C (Value : GString_C) return GString;
   --  Convert from a C GString to a GString

   function To_GString_From_Unbounded
     (Value : GString_Unbounded) return GString;
   --  Convert from an unbounded GString to a bounded GString

   function To_GString_C (Value : GString) return GString_C;
   --  Convert from a GString in to a C GString

   procedure To_GString_C (Value : GString; Destination : in out GString_C);
   --  Convert from a GString in to a C GString of determined
   --  length. If Value is too long, it will be truncated.

   function To_GString_Unbounded (Value : GString) return GString_Unbounded;
   --  Convert from a GString to an unbounded GString

   function To_String (Value : GString) return String;
   --  Convert from a GString in to an Ada String regarless if GString is
   --  ANSI or Unicode

   function To_Wide_String (Value : GString) return Wide_String;
   --  Convert from a GString to an Ada Wide_String regardless if GString
   --  is ANSI or Unicode

   function To_GString_From_String (Value : String) return GString;
   --  Create a GString from a String

   function To_GString_From_Wide_String (Value : Wide_String) return GString;
   --  Create a GString from a Wide_String

   function Image (Value : Integer) return GString;
   --  Returns a GString for the Image of an Integer

   generic
      type Enumeral_Type is (<>);
   function Enum_Image (Value : Enumeral_Type) return GString;
   --  Returns a GString for the Image of an Enumerated type

   function To_GString_From_BSTR (Value : GNATCOM.Types.BSTR;
                                  Free  : Boolean            := True)
                                 return GString;

   function To_BSTR_From_GString (Value : GString) return GNATCOM.Types.BSTR;
   --  Convert between COM BSTRs and GStrings

   function To_GString_From_VARIANT (Value : GNATCOM.Types.VARIANT;
                                     Clear : Boolean               := True)
                                    return GString;

   function To_VARIANT_From_GString (Value : GString)
                                    return GNATCOM.Types.VARIANT;
   --  Convert between COM VARIANTs and GStrings

   procedure To_Upper (Value : in out GString);
   --  Converts case of GString to Upper

   procedure To_Lower (Value : in out GString);
   --  Converts case of GString to Lower

   procedure To_Mixed (Value : in out GString);
   --  Converts case of GString to Mixed case string

   function Resource_ID (ID : Integer) return GString;
   --  Constructs a resource ID string "#XXXX" from the ID

end GWindows.GStrings;
