------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--               G N A T C O M . C R E A T E . I N P R O C                  --
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
     (hinstDLL    : Interfaces.C.long;
      fdwReason   : Interfaces.C.unsigned_short;
      lpvReserved : GNATCOM.Types.Pointer_To_Void)
     return Interfaces.C.int;
   pragma Export (StdCall, DllMain, "DllMain");

   ---------------------
   -- DllCanUnloadNow --
   ---------------------

   function DllCanUnloadNow return GNATCOM.Types.HRESULT is
      use type Interfaces.C.long;
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
           Factory_Map.all'First .. (Factory_Map.all'Last)
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
        Factory_Map.all'First .. (Factory_Map.all'Last)
      loop

         GNATCOM.Register.Register_Inproc_Server
           (hInstance    => GNATCOM.Create.hInstance,
            CLSID        => Factory_Map (N).CLSID,
            Name         => To_String (Factory_Map (N).Name),
            Version      => To_String (Factory_Map (N).Version),
            Description  => To_String (Factory_Map (N).Description),
            Thread_Model => To_String (Threads));
      end loop;

      return S_OK;
   end DllRegisterServer;

   -------------------------
   -- DllUnregisterServer --
   -------------------------

   function DllUnregisterServer return GNATCOM.Types.HRESULT is
      use Ada.Strings.Unbounded;
   begin
      Adainit;

      GNATCOM.Register.Unregister_Type_Library (TypeLibary_LIBID);

      for N in
        Factory_Map.all'First .. (Factory_Map.all'Last)
      loop
         GNATCOM.Register.Unregister_Server
           (CLSID       => Factory_Map (N).CLSID,
            Name        => To_String (Factory_Map (N).Name),
            Version     => To_String (Factory_Map (N).Version));
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
     (hinstDLL    : Interfaces.C.long;
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
