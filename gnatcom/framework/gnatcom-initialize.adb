------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                   G N A T C O M . I N I T I A L I Z E                    --
--                                                                          --
--                                B o d y                                   --
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

   function sync_fetch_and_add
     (Ref : access Interfaces.Unsigned_32;
      Add : Interfaces.Unsigned_32)
      return Interfaces.Unsigned_32
   with
       Import,
       Convention => Intrinsic,
       External_Name => "__sync_fetch_and_add_4";

   function sync_fetch_and_sub
     (Ref : access Interfaces.Unsigned_32;
      Add : Interfaces.Unsigned_32)
      return Interfaces.Unsigned_32
     with
       Import,
       External_Name => "__sync_fetch_and_sub_4",
       Convention => Intrinsic;

   procedure InterlockedIncrement (Ref : access Interfaces.Unsigned_32) is
      Old : Interfaces.Unsigned_32;
      pragma Unreferenced (Old);
   begin
      Old := sync_fetch_and_add (Ref, 1);
   end InterlockedIncrement;

   procedure InterlockedDecrement (Ref : access Interfaces.Unsigned_32) is
      Old : Interfaces.Unsigned_32;
      pragma Unreferenced (Old);
   begin
      Old := sync_fetch_and_sub (Ref, 1);
   end InterlockedDecrement;

   -- Initialize_COM --

   procedure Initialize_COM is
   begin
      if CoInitialize = RPC_E_CHANGED_MODE then
         raise CHANGED_MODE_ERROR;
      end if;
      InterlockedIncrement (Initialize_Count'Access);
   end Initialize_COM;

   -- Initialize_COM_Multi_Threaded --

   procedure Initialize_COM_Multi_Threaded is
   begin
      if CoInitializeEx = RPC_E_CHANGED_MODE then
         raise CHANGED_MODE_ERROR;
      end if;
      InterlockedIncrement (Initialize_Count'Access);
   end Initialize_COM_Multi_Threaded;

   -- Uninitialize_COM --

   procedure Uninitialize_COM is
   begin
      CoUninitialize;
      InterlockedDecrement (Initialize_Count'Access);
   end Uninitialize_COM;

end GNATCOM.Initialize;
