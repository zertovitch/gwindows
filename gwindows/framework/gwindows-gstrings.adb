------------------------------------------------------------------------------
--                                                                          --
--         GWINDOWS - Ada 95 Framework for Windows GUI Development          --
--                                                                          --
--                   G W I N D O W S . G S T R I N G S                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2012 David Botton                   --
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
-- be located on the web at one of the following places:                    --
--   http://sf.net/projects/gnavi/                                          --
--   http://www.gnavi.org/gwindows                                          --
--   http://www.adapower.com/gwindows                                       --
--                                                                          --
------------------------------------------------------------------------------

--  UNICODE Version

with Ada.Characters.Handling;

with GNAT.Case_Util;

with Interfaces.C; use Interfaces.C;

with GNATCOM.BSTR;
with GNATCOM.VARIANT;

with GWindows.GStrings.Unbounded;

package body GWindows.GStrings is

   ----------------
   -- To_GString --
   ----------------

   function To_GString_From_C (Value : GString_C) return GString is
   begin
      return To_Ada (Value);
   end To_GString_From_C;

   ----------------
   -- To_GString --
   ----------------

   function To_GString_From_Unbounded
     (Value : GString_Unbounded) return GString
   is
   begin
      return GWindows.GStrings.Unbounded.To_Wide_String (Value);
   end To_GString_From_Unbounded;

   ------------------
   -- To_GString_C --
   ------------------

   function To_GString_C (Value : GString) return GString_C is
   begin
      if Value'Length = 0 then
         declare
            Empty_GString : constant GString_C (0 .. 0) :=
              (others => GString_C_Null);
         begin
            return Empty_GString;
         end;
      else
         return To_C (Value);
      end if;
   end To_GString_C;

   --------------------------
   -- To_GString_Unbounded --
   --------------------------

   function To_GString_Unbounded (Value : GString) return GString_Unbounded is
   begin
      return GWindows.GStrings.Unbounded.To_Unbounded_Wide_String (Value);
   end To_GString_Unbounded;

   ---------------
   -- To_String --
   ---------------

   function To_String (Value : GString) return String
   is
   begin
      return Ada.Characters.Handling.To_String (Value);
   end To_String;

   --------------------
   -- To_Wide_String --
   --------------------

   function To_Wide_String (Value : GString) return Wide_String
   is
   begin
      return Value;
   end To_Wide_String;

   ----------------------------
   -- To_GString_From_String --
   ----------------------------

   function To_GString_From_String (Value : String) return GString
   is
   begin
      return GString (Ada.Characters.Handling.To_Wide_String (Value));
   end To_GString_From_String;

   ---------------------------------
   -- To_GString_From_Wide_String --
   ---------------------------------

   function To_GString_From_Wide_String (Value : Wide_String) return GString
   is
   begin
      return Value;
   end To_GString_From_Wide_String;

   -----------
   -- Image --
   -----------

   function Image (Value : Integer) return GString
   is
   begin
      return Integer'Wide_Image (Value);
   end Image;

   function Enum_Image (Value : Enumeral_Type) return GString
   is
   begin
      return Enumeral_Type'Wide_Image (Value);
   end Enum_Image;

   --------------------------
   -- To_GString_From_BSTR --
   --------------------------

   function To_GString_From_BSTR (Value : GNATCOM.Types.BSTR;
                                  Free  : Boolean            := True)
                                 return GString
   is
   begin
      return GNATCOM.BSTR.To_Ada_Wide (Value, Free);
   end To_GString_From_BSTR;

   ---------------------------
   -- To_BSTR_From_GString --
   ---------------------------

   function To_BSTR_From_GString (Value : GString) return GNATCOM.Types.BSTR
   is
   begin
      return GNATCOM.BSTR.To_BSTR_From_Wide (Value);
   end To_BSTR_From_GString;

   -----------------------------
   -- To_GString_From_VARIANT --
   -----------------------------

   function To_GString_From_VARIANT (Value : GNATCOM.Types.VARIANT;
                                     Clear : Boolean               := True)
                                    return GString
   is
   begin
      return GNATCOM.VARIANT.To_Ada_Wide (Value, Clear);
   end To_GString_From_VARIANT;

   -----------------------------
   -- To_VARIANT_From_GString --
   -----------------------------

   function To_VARIANT_From_GString (Value : GString)
                                    return GNATCOM.Types.VARIANT
   is
   begin
      return GNATCOM.VARIANT.To_VARIANT_From_Wide (Value);
   end To_VARIANT_From_GString;

   --------------
   -- To_Upper --
   --------------

   procedure To_Upper (Value : in out GString)
   is
      Temp : String := To_String (Value);
   begin
      GNAT.Case_Util.To_Upper (Temp);
      Value := To_GString_From_String (Temp);
   end To_Upper;

   --------------
   -- To_Lower --
   --------------

   procedure To_Lower (Value : in out GString)
   is
      Temp : String := To_String (Value);
   begin
      GNAT.Case_Util.To_Lower (Temp);
      Value := To_GString_From_String (Temp);
   end To_Lower;

   --------------
   -- To_Mixed --
   --------------

   procedure To_Mixed (Value : in out GString)
   is
      Temp : String := To_String (Value);
   begin
      GNAT.Case_Util.To_Mixed (Temp);
      Value := To_GString_From_String (Temp);
   end To_Mixed;

   -----------------
   -- Resource_ID --
   -----------------

   function Resource_ID (ID : Integer) return GString
   is
      X : constant GString := Image (ID);
   begin
      if X'Length > 1 then
         return "#" & X (X'First + 1 .. X'Last);
      else
         return "";
      end if;
   end Resource_ID;

end GWindows.GStrings;
