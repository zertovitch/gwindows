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
