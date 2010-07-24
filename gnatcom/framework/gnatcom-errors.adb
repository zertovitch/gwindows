------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                      G N A T C O M . E R R O R S                         --
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

with Ada.Unchecked_Conversion;
with Ada.Exceptions;
with Interfaces.C;
with System.Storage_Elements;

package body GNATCOM.Errors is

   use type GNATCOM.Types.HRESULT;

   function Strip (Image_String : String) return String;
   --  Strips the space prefix off an Type'Image

   TLS_Index : GNATCOM.Types.DWORD;
   --  Thread local storage index for last HRESULT

   function TlsAlloc return GNATCOM.Types.DWORD;
   pragma Import (StdCall, TlsAlloc, "TlsAlloc");
   --  Returns a TLS Index used to access OS allocated thread storage for
   --  last HRESULT

   type Uns is mod 2 ** Standard'Address_Size;

   procedure TlsSetValue (Index  : GNATCOM.Types.DWORD; Result : Uns);
   pragma Import (StdCall, TlsSetValue, "TlsSetValue");
   --  Sets the HRESULT in to the TLS storage

   function TlsGetValue (Index : GNATCOM.Types.DWORD) return Uns;
   pragma Import (StdCall, TlsGetValue, "TlsGetValue");
   --  Gets the HRESULT ouf of TLS storage

   ---------------
   -- SUCCEEDED --
   ---------------

   function SUCCEEDED (Result : GNATCOM.Types.HRESULT)
     return Boolean
   is
      use type Interfaces.C.long;

      function To_Long is
         new Ada.Unchecked_Conversion (GNATCOM.Types.HRESULT,
                                       Interfaces.C.long);
   begin
      if To_Long (Result) >= 0 then
         return True;
      else
         return False;
      end if;
   end SUCCEEDED;

   ------------
   -- FAILED --
   ------------

   function FAILED (Result : GNATCOM.Types.HRESULT)
     return Boolean
   is
   begin
      return not (SUCCEEDED (Result));
   end FAILED;

   ---------------
   -- To_String --
   ---------------

   function To_String (Result : GNATCOM.Types.HRESULT) return String is
      FORMAT_MESSAGE_FROM_SYSTEM : constant := 4096;
      function FormatMessage
        (dwFlags      : in     GNATCOM.Types.DWORD
         := FORMAT_MESSAGE_FROM_SYSTEM;
         lpSource     : in     System.Address := System.Null_Address;
         dwMessageId  : in     GNATCOM.Types.HRESULT;
         dwLanguageId : in     Interfaces.C.unsigned_long := 0;
         lpBuffer     : in     System.Address;
         nSize        : in     Interfaces.C.unsigned_long;
         Arguments    : in     System.Address := System.Null_Address)
         return Integer;
      pragma Import (StdCall, FormatMessage, "FormatMessageA");

      Message : String (1 .. 1024);
      Len     : Integer;
   begin
      Len := FormatMessage (dwMessageId => Result,
                            lpBuffer    => Message'Address,
                            nSize       => Message'Length);
      return Message (1 .. Len);
   end To_String;

   -----------------
   -- Error_Check --
   -----------------

   procedure Error_Check (Result : in GNATCOM.Types.HRESULT) is
   begin
      Set_Last_HRESULT (Result);

      if FAILED (Result) then
         declare
            Message : constant String := To_String (Result);
         begin
            case Result is
               when E_NOTIMPL =>
                  Ada.Exceptions.Raise_Exception
                    (NOT_IMPLEMENTED_ERROR'Identity,
                     Message);
               when E_OUTOFMEMORY =>
                  Ada.Exceptions.Raise_Exception
                    (OUT_OF_MEMORY_ERROR'Identity,
                     Message);
               when E_INVALIDARG =>
                  Ada.Exceptions.Raise_Exception
                    (INVALID_ARGUMENT_ERROR'Identity,
                     Message);
               when E_NOINTERFACE =>
                  Ada.Exceptions.Raise_Exception
                    (NO_INTERFACE_ERROR'Identity,
                     Message);
               when E_POINTER =>
                  Ada.Exceptions.Raise_Exception
                    (INVALID_POINTER_ERROR'Identity,
                     Message);
               when E_ABORT =>
                  Ada.Exceptions.Raise_Exception
                    (ABORT_ERROR'Identity,
                     Message);
               when E_FAIL =>
                  Ada.Exceptions.Raise_Exception
                    (COM_ERROR'Identity,
                     Message);
               when E_ACCESSDENIED =>
                  Ada.Exceptions.Raise_Exception
                    (ACCESS_DENIED_ERROR'Identity,
                     Message);
               when E_UNEXPECTED =>
                  Ada.Exceptions.Raise_Exception
                    (UNEXPECTED_ERROR'Identity,
                     Message);
               when CO_E_OBJNOTCONNECTED =>
                  Ada.Exceptions.Raise_Exception
                    (OBJECT_NOT_CONNECTED_ERROR'Identity,
                     Message);
               when others =>
                  Ada.Exceptions.Raise_Exception
                    (COM_ERROR'Identity,
                     "HRESULT (" &
                     Strip (GNATCOM.Types.HRESULT'Image (Result)) & ") : " &
                     Message);
            end case;
         end;
      end if;
   end Error_Check;

   -------------------
   -- Logical_Check --
   -------------------

   function Logical_Check (Result : in GNATCOM.Types.HRESULT)
     return Boolean
   is
   begin
      Set_Last_HRESULT (Result);

      if Result = S_FALSE then
         return False;
      elsif SUCCEEDED (Result) then
         return True;
      else
         Error_Check (Result);

         --  Should never be called since Error_Check will raise and
         --  exception. Placed here to avoid compiler warnings.
         return False;
      end if;

   end Logical_Check;

   ---------------
   -- To_String --
   ---------------

   function To_String (Address : System.Address) return String
   is
      use System.Storage_Elements;
   begin
      return Strip (To_Integer (Address)'Img);
   end To_String;

   ----------------------
   -- Get_Last_HRESULT --
   ----------------------

   function Get_Last_HRESULT return GNATCOM.Types.HRESULT is
   begin
      return GNATCOM.Types.HRESULT (TlsGetValue (TLS_Index));
   end Get_Last_HRESULT;

   ----------------------
   -- Set_Last_HRESULT --
   ----------------------

   procedure Set_Last_HRESULT (Result : GNATCOM.Types.HRESULT) is
   begin
      TlsSetValue (TLS_Index, Uns (Result));
   end Set_Last_HRESULT;

   -----------
   -- Strip --
   -----------

   function Strip (Image_String : String) return String
   is
   begin
      return Image_String (Image_String'First + 1 .. Image_String'Last);
   end Strip;

begin
   TLS_Index := TlsAlloc;
end GNATCOM.Errors;
