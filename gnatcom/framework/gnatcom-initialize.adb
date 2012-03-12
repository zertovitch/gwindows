------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                   G N A T C O M . I N I T I A L I Z E                    --
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

with Interfaces.C;
with System;

with GNATCOM.Types;

package body GNATCOM.Initialize is

   use type GNATCOM.Types.HRESULT;

   function CoInitialize
     (pvReserved : Types.Pointer_To_Void := System.Null_Address)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, CoInitialize, "CoInitialize");

   COINIT_MULTITHREADED : constant := 0;

   function CoInitializeEx
     (pvReserved : Types.Pointer_To_Void := System.Null_Address;
      dwCoInit   : Interfaces.C.unsigned_long := COINIT_MULTITHREADED)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, CoInitializeEx, "CoInitializeEx");

   procedure CoUninitialize;
   pragma Import (StdCall, CoUninitialize, "CoUninitialize");

   RPC_E_CHANGED_MODE : constant := 16#80010106#;
   --  Attempt to re-initilalize in wrong mode

   function sync_fetch_and_add (Ref : access Integer;
                                Add : Integer) return Integer;
   pragma Import (Intrinsic, sync_fetch_and_add,
                  "__sync_fetch_and_add_4");

   procedure InterlockedIncrement (Ref : access Integer) is
      Old : Integer;
      pragma Unreferenced (Old);
   begin
      Old := sync_fetch_and_add (Ref, 1);
   end InterlockedIncrement;

   procedure InterlockedDecrement (Ref : access Integer) is
      Old : Integer;
      pragma Unreferenced (Old);
   begin
      Old := sync_fetch_and_add (Ref, -1);
   end InterlockedDecrement;

   -- Initialize_COM --

   procedure Initialize_COM is
      use Interfaces.C;
   begin
      if CoInitialize = RPC_E_CHANGED_MODE then
         raise CHANGED_MODE_ERROR;
      end if;
      InterlockedIncrement (Initialize_Count'Access);
   end Initialize_COM;

   -- Initialize_COM_Multi_Threaded --

   procedure Initialize_COM_Multi_Threaded is
      use Interfaces.C;
   begin
      if CoInitializeEx = RPC_E_CHANGED_MODE then
         raise CHANGED_MODE_ERROR;
      end if;
      InterlockedIncrement (Initialize_Count'Access);
   end Initialize_COM_Multi_Threaded;

   -- Uninitialize_COM --

   procedure Uninitialize_COM is
      use Interfaces.C;
   begin
      CoUninitialize;
      InterlockedDecrement (Initialize_Count'Access);
   end Uninitialize_COM;

end GNATCOM.Initialize;
