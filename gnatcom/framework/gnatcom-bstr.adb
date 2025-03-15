------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                        G N A T C O M . B S T R                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2018 David Botton                   --
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

with Ada.Characters.Handling;

package body GNATCOM.BSTR is

   procedure SysFreeString (bstr : GNATCOM.Types.BSTR);
   pragma Import (StdCall, SysFreeString, "SysFreeString");

   function SysStringLen (bstr : GNATCOM.Types.BSTR)
                         return Interfaces.C.unsigned;
   pragma Import (StdCall, SysStringLen, "SysStringLen");

   function SysAllocString (C_String : Interfaces.C.wchar_array)
                           return GNATCOM.Types.BSTR;
   pragma Import (StdCall, SysAllocString, "SysAllocString");

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (This : GNATCOM.Types.BSTR) return Boolean is
      use type GNATCOM.Types.BSTR;
   begin

      if This = null then
         return True;
      else
         if Length (This) < 1 then
            return True;
         end if;
      end if;

      return False;

   end Is_Empty;

   ----------
   -- Free --
   ----------

   procedure Free (This : in GNATCOM.Types.BSTR) is
   begin
      SysFreeString (This);
   end Free;

   -------------
   -- To_BSTR --
   -------------

   function To_BSTR (From : String) return GNATCOM.Types.BSTR
   is
   begin
      if From = "" then
         declare
            Empty_String : Interfaces.C.wchar_array (1 .. 1);
         begin
            Empty_String (1) := Interfaces.C.wide_nul;
            return SysAllocString (Empty_String);
         end;
      end if;

      return To_BSTR_From_Wide (Ada.Characters.Handling.To_Wide_String (From));
   end To_BSTR;

   -----------------------
   -- To_BSTR_From_Wide --
   -----------------------

   function To_BSTR_From_Wide (From : Wide_String) return GNATCOM.Types.BSTR
   is
   begin
      if From = "" then
         declare
            Empty_String : Interfaces.C.wchar_array (1 .. 1);
         begin
            Empty_String (1) := Interfaces.C.wide_nul;
            return SysAllocString (Empty_String);
         end;
      end if;

      return To_BSTR_From_Wide_C (Interfaces.C.To_C (From));
   end To_BSTR_From_Wide;

   --------------------
   -- To_BSTR_From_C --
   --------------------

   function To_BSTR_From_C (From : Interfaces.C.char_array)
     return GNATCOM.Types.BSTR
   is
   begin
      return To_BSTR (Interfaces.C.To_Ada (From));
   end To_BSTR_From_C;

   -------------------------
   -- TO_BSTR_From_Wide_C --
   -------------------------

   function To_BSTR_From_Wide_C (From : Interfaces.C.wchar_array)
                                return GNATCOM.Types.BSTR
   is
      use type GNATCOM.Types.BSTR;

      New_BSTR : GNATCOM.Types.BSTR;
   begin
      New_BSTR := SysAllocString (From);

      if New_BSTR = null then
         raise BSTR_ERROR;
      end if;

      return New_BSTR;

   end To_BSTR_From_Wide_C;

   ------------
   -- To_Ada --
   ------------

   function To_Ada (From : GNATCOM.Types.BSTR;
                    Free : Boolean            := True)
                   return String
   is
      use type GNATCOM.Types.BSTR;
   begin
      if From = null then
         return "";
      end if;

      declare
         Ada_String : constant String := GNATCOM.Types.To_Ada (From);
      begin

         if Free then
            GNATCOM.BSTR.Free (From);
         end if;

         return Ada_String;
      end;
   end To_Ada;

   -----------------
   -- To_Ada_Wide --
   -----------------

   function To_Ada_Wide (From : GNATCOM.Types.BSTR;
                         Free : Boolean            := True)
                        return Wide_String
   is
      use type GNATCOM.Types.BSTR;
   begin
      if From = null then
         return "";
      end if;

      declare
         Ada_String : constant Wide_String := GNATCOM.Types.To_Ada (From);
      begin
         if Free then
            GNATCOM.BSTR.Free (From);
         end if;

         return Ada_String;
      end;
   end To_Ada_Wide;

   ----------
   -- To_C --
   ----------

   function To_C (From : GNATCOM.Types.BSTR;
                  Free : Boolean            := True)
                 return Interfaces.C.char_array
   is
   begin
      return Interfaces.C.To_C (To_Ada (From, Free));
   end To_C;

   ---------------
   -- To_C_Wide --
   ---------------

   function To_C_Wide (From : GNATCOM.Types.BSTR;
                       Free : Boolean            := True)
                      return Interfaces.C.wchar_array
   is
   begin
      return Interfaces.C.To_C (To_Ada_Wide (From, Free));
   end To_C_Wide;

   ------------
   -- Length --
   ------------

   function Length (Source : GNATCOM.Types.BSTR)
     return Natural
   is
   begin
      return Natural (SysStringLen (Source));
   end Length;

end GNATCOM.BSTR;
