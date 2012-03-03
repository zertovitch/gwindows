------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                   G W I N D O W S . G S T R I N G S                      --
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
