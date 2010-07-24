------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                        G N A T C O M . B S T R                           --
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
      use type Interfaces.C.wchar_array;
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
      use type Interfaces.C.wchar_array;
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
