------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--               G N A T C O M . C R E A T E . I N P R O C                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2026 David Botton                   --
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

with GNATCOM.Register;

package body GNATCOM.Create.Inproc is

   CLASS_E_CLASSNOTAVAILABLE : constant := 16#80040111#;

   TypeLibary_LIBID : GNATCOM.Types.GUID;

   DLL_PROCESS_DETACH : constant := 0;
   DLL_PROCESS_ATTACH : constant := 1;

   procedure Adainit;
   pragma Import (C, Adainit);

   procedure Adafinal;
   pragma Import (C, Adafinal);

   function DllMain
     (hinstDLL    : Interfaces.C.ptrdiff_t;
      fdwReason   : Interfaces.C.unsigned_short;
      lpvReserved : GNATCOM.Types.Pointer_To_Void)
     return Interfaces.C.int;
   pragma Export (StdCall, DllMain, "DllMain");

   ---------------------
   -- DllCanUnloadNow --
   ---------------------

   function DllCanUnloadNow return GNATCOM.Types.HRESULT is
      use type Interfaces.Unsigned_32;
   begin
      if
        (GNATCOM.Create.Server_Lock_Count = 0) and
        (GNATCOM.Create.Component_Count = 0)
      then
         return S_OK;
      else
         return S_FALSE;
      end if;
   end DllCanUnloadNow;

   -----------------------
   -- DllGetClassObject --
   -----------------------

   function DllGetClassObject
     (clsid : in     GNATCOM.Types.Pointer_To_GUID;
      riid  : in     GNATCOM.Types.Pointer_To_GUID;
      ppv   : access GNATCOM.Types.Pointer_To_Void)
     return GNATCOM.Types.HRESULT
   is
      use type GNATCOM.Types.GUID;
      use type GNATCOM.Create.Factory.Pointer_To_IClassFactory;
   begin
      Adainit;
      --  Adainit is called outside of DllMain to avoid
      --  a Win32 "feature", that if a thread is created in
      --  the dllmain, it must exit before dllmain can return.

      declare
         pFactory : GNATCOM.Create.Factory.Pointer_To_IClassFactory := null;
         hr       : GNATCOM.Types.HRESULT;
         refcount : Interfaces.C.unsigned_long;
         pragma Warnings (Off, refcount);
      begin
         for N in
           Factory_Map.all'First .. Factory_Map.all'Last
         loop
            if clsid.all = Factory_Map (N).CLSID then
               pFactory := new GNATCOM.Create.Factory.IClassFactory;
               pFactory.Create := Factory_Map (N).Create;
            end if;
         end loop;

         if pFactory = null then
            return CLASS_E_CLASSNOTAVAILABLE;
         end if;

         hr := GNATCOM.Create.Factory.QueryInterface (pFactory, riid, ppv);

         refcount := GNATCOM.Create.Factory.Release (pFactory);

         return hr;
      end;
   end DllGetClassObject;

   -----------------------
   -- DllRegisterServer --
   -----------------------

   function DllRegisterServer return GNATCOM.Types.HRESULT is
      use Ada.Strings.Unbounded;
      use type GNATCOM.Types.GUID_Array_Pointer;
      Threads : Unbounded_String;
   begin
      Adainit;

      GNATCOM.Register.Register_Type_Library (GNATCOM.Create.hInstance);

      case Use_Thread_Model is
         when Single =>
            Threads := To_Unbounded_String ("Apartment");
         when Multiple =>
            Threads := To_Unbounded_String ("Free");
         when Both =>
            Threads := To_Unbounded_String ("Both");
      end case;

      for N in
        Factory_Map.all'First .. Factory_Map.all'Last
      loop

         GNATCOM.Register.Register_Inproc_Server
           (hInstance    => GNATCOM.Create.hInstance,
            CLSID        => Factory_Map (N).CLSID,
            Name         => To_String (Factory_Map (N).Name),
            Version      => To_String (Factory_Map (N).Version),
            Description  => To_String (Factory_Map (N).Description),
            Thread_Model => To_String (Threads),
            Implemented_Categories =>
              (if Factory_Map (N).Implemented_Categories /= null then
                    Factory_Map (N).Implemented_Categories.all
               else GNATCOM.Types.GUID_Array'(2 .. 1 => <>)));
      end loop;

      return S_OK;
   end DllRegisterServer;

   -------------------------
   -- DllUnregisterServer --
   -------------------------

   function DllUnregisterServer return GNATCOM.Types.HRESULT is
      use Ada.Strings.Unbounded;
      use type GNATCOM.Types.GUID_Array_Pointer;
   begin
      Adainit;

      GNATCOM.Register.Unregister_Type_Library (TypeLibary_LIBID);

      for N in
        Factory_Map.all'First .. Factory_Map.all'Last
      loop
         GNATCOM.Register.Unregister_Server
           (CLSID       => Factory_Map (N).CLSID,
            Name        => To_String (Factory_Map (N).Name),
            Version     => To_String (Factory_Map (N).Version),
            Implemented_Categories =>
              (if Factory_Map (N).Implemented_Categories /= null then
                    Factory_Map (N).Implemented_Categories.all
               else GNATCOM.Types.GUID_Array'(2 .. 1 => <>)));
      end loop;

      return S_OK;
   end DllUnregisterServer;

   -----------------
   -- Init_Object --
   -----------------

   procedure Init_Object (LIBID : in GNATCOM.Types.GUID) is
   begin
      TypeLibary_LIBID  := LIBID;
   end Init_Object;

   -------------
   -- DllMain --
   -------------

   function DllMain
     (hinstDLL    : Interfaces.C.ptrdiff_t;
      fdwReason   : Interfaces.C.unsigned_short;
      lpvReserved : GNATCOM.Types.Pointer_To_Void)
     return Interfaces.C.int
   is
      pragma Warnings (Off, lpvReserved);
   begin
      case fdwReason is
         when DLL_PROCESS_ATTACH =>
            GNATCOM.Create.hInstance := hinstDLL;
            return 1;
         when DLL_PROCESS_DETACH =>
            Adafinal;
            return 1;
         when others =>
            return 1;
      end case;
   end DllMain;

end GNATCOM.Create.Inproc;
