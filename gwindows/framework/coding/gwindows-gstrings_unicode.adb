------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                   G W I N D O W S . G S T R I N G S                      --
--                                                                          --
--                                 B o d y                                  --
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

   procedure To_GString_C (Value : GString; Destination : in out GString_C)
   is
      i : size_t := Destination'First;
   begin
      if Destination'Length = 0 then
         return;
      elsif Destination'Length = 1 then
         Destination (Destination'Last) := GString_C_Null;
         --  No other choice than just return an "empty" C string
      else
         for j in Value'Range loop
            Destination (i) := To_C (Value (j));
            i := i + 1;
            exit when i = Destination'Last;
            --  Truncate if source longer than destination
         end loop;
         Destination (i) := GString_C_Null;
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
