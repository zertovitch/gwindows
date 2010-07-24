------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                        G N A T C O M . G U I D                           --
--                                                                          --
--                                B o d y                                   --
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
with Ada.Unchecked_Conversion;
with Interfaces.C;
with System;

with GNATCOM.Errors;

package body GNATCOM.GUID is

   package C renames Interfaces.C;

   function CLSIDFromString
     (lpsz   : GNATCOM.Types.LPWSTR;
      pclsid : GNATCOM.Types.Pointer_To_GUID)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, CLSIDFromString, "CLSIDFromString");

   function StringFromCLSID
     (rclsid : GNATCOM.Types.Pointer_To_GUID;
      lplpsz : GNATCOM.Types.Pointer_To_LPWSTR)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, StringFromCLSID, "StringFromCLSID");

   procedure CoTaskMemFree (pv : GNATCOM.Types.Pointer_To_Void);
   pragma Import (StdCall, CoTaskMemFree, "CoTaskMemFree");

   function CoCreateGuid (pguid : GNATCOM.Types.Pointer_To_GUID)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, CoCreateGuid, "CoCreateGuid");

   -----------------
   -- Create_GUID --
   -----------------

   function Create_GUID return GNATCOM.Types.GUID is
      use type GNATCOM.Types.HRESULT;
      use GNATCOM.Errors;

      New_GUID    : aliased GNATCOM.Types.GUID;
   begin
      if FAILED (CoCreateGuid (New_GUID'Unchecked_Access)) then
         raise GUID_Error;
      end if;

      return New_GUID;
   end Create_GUID;

   -------------
   -- To_GUID --
   -------------

   function To_GUID (From : String) return GNATCOM.Types.GUID is
      use type GNATCOM.Types.HRESULT;
      use GNATCOM.Errors;

      function To_LPWSTR is
         new Ada.Unchecked_Conversion (System.Address, GNATCOM.Types.LPWSTR);

      ID_String : C.wchar_array :=
        C.To_C (Ada.Characters.Handling.To_Wide_String (From));
      ID        : aliased GNATCOM.Types.GUID;
   begin
      if FAILED (CLSIDFromString (To_LPWSTR (ID_String'Address),
                                  ID'Unchecked_Access))
      then
         raise GUID_Error;
      end if;

      return ID;
   end To_GUID;

   ---------------
   -- To_String --
   ---------------

   function To_String (From : GNATCOM.Types.GUID) return String is
      use type GNATCOM.Types.HRESULT;
      use type GNATCOM.Types.BSTR;
      use GNATCOM.Errors;

      Ref_GUID    : aliased GNATCOM.Types.GUID := From;
      GUID_String : aliased GNATCOM.Types.LPWSTR;
   begin
      if SUCCEEDED (StringFromCLSID (Ref_GUID'Unchecked_Access,
                                     GUID_String'Unchecked_Access))
      then
         declare
            Ada_GUID_String : constant String :=
              GNATCOM.Types.To_Ada (GUID_String);
         begin
            CoTaskMemFree (GUID_String.all'Address);
            return Ada_GUID_String;
         end;
      else
         raise GUID_Error;
      end if;
   end To_String;

end GNATCOM.GUID;
