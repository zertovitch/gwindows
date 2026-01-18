------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--         G N A T C O M . C R E A T E . L O C A L _ S E R V E R            --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with Ada.Text_IO;
with System;

with GNATCOM.Initialize;
with GNATCOM.Register;
with GNATCOM.Errors;
with GNATCOM.Utility;
with GNATOCX.IRunningObjectTable_Interface;
with GNATOCX.IMoniker_Interface;

package body GNATCOM.Create.Local_Server is

   CLSCTX_LOCAL_SERVER   : constant := 4;
   --  REGCLS_SINGLEUSE      : constant := 0;
   REGCLS_MULTIPLEUSE    : constant := 1;
   --  REGCLS_MULTI_SEPARATE : constant := 2;
   REGCLS_SUSPENDED      : constant := 4;

   procedure Error_Check (Result : in GNATCOM.Types.HRESULT);

   procedure Display_Help;
   --  Displays instructions on using the Local Server

   function CoRegisterClassObject
     (rclsid       : GNATCOM.Types.Pointer_To_GUID;
      punk         : GNATCOM.Types.Pointer_To_IUnknown;
      dwClsContext : Interfaces.C.unsigned_long;
      flags        : Interfaces.C.unsigned_long;
      lpdwRegister : GNATCOM.Types.Pointer_To_unsigned_long)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, CoRegisterClassObject, "CoRegisterClassObject");

   function CoRevokeClassObject
     (dwRegister : Interfaces.C.unsigned_long)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, CoRevokeClassObject, "CoRevokeClassObject");

   procedure CoResumeClassObjects;
   pragma Import (StdCall, CoResumeClassObjects, "CoResumeClassObjects");

   function Retrieve_hInstance return Interfaces.C.ptrdiff_t;
   pragma Import (C, Retrieve_hInstance, "rts_get_hInstance");

   procedure CoAddRefServerProcess;
   pragma Import (StdCall, CoAddRefServerProcess, "CoAddRefServerProcess");

   RPC_C_AUTHN_LEVEL_PKT    : constant := 4;
   RPC_C_IMP_LEVEL_IDENTITY : constant := 2;

   function CoInitializeSecurity
     (pSecDesc       : GNATCOM.Types.PSECURITY_DESCRIPTOR :=
        GNATCOM.Types.PSECURITY_DESCRIPTOR (System.Null_Address);
      cAuthSvc       : GNATCOM.Types.LONG := GNATCOM.Types.LONG (-1);
      asAuthSvc   : access GNATCOM.Types.SOLE_AUTHENTICATION_SERVICE := null;
      pReserved1     : GNATCOM.Types.Pointer_To_Void := System.Null_Address;
      dwAuthnLevel   : GNATCOM.Types.DWORD := RPC_C_AUTHN_LEVEL_PKT;
      dwImpLevel     : GNATCOM.Types.DWORD := RPC_C_IMP_LEVEL_IDENTITY;
      pAuthList      : GNATCOM.Types.Pointer_To_Void := System.Null_Address;
      dwCapabilities : GNATCOM.Types.DWORD := 0;
      pReserved3     : GNATCOM.Types.Pointer_To_Void := System.Null_Address)
      return GNATCOM.Types.HRESULT
     with
       Import, External_Name => "CoInitializeSecurity", Convention => StdCall;

   ------------------
   -- Display_Help --
   ------------------

   procedure Display_Help is
   begin
      Ada.Text_IO.Put_Line ("This is a local server for a COM object");
      Ada.Text_IO.Put_Line ("To register this server use:");
      Ada.Text_IO.Put_Line ("servername -RegServer");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line ("To unregister this server use:");
      Ada.Text_IO.Put_Line ("servername -UnregServer");
      Ada.Text_IO.New_Line;
      Ada.Text_IO.Put_Line
        ("To start the server up manually: (COM will normally do this)");
      Ada.Text_IO.Put_Line ("servername -Embedding");
   end Display_Help;

   -----------------
   -- Init_Object --
   -----------------

   procedure Init_Object (LIBID : in GNATCOM.Types.GUID; Run : Run_Mode) is
      use type Interfaces.C.unsigned_long;
      use type GNATCOM.Types.GUID_Array_Pointer;

      function To_Pointer_To_IUnknown is
         new Ada.Unchecked_Conversion
        (GNATCOM.Create.Factory.Pointer_To_IClassFactory,
         GNATCOM.Types.Pointer_To_IUnknown);

      refcount : Interfaces.C.unsigned_long;
      pragma Warnings (Off, refcount);

      ROT : GNATOCX.IRunningObjectTable_Interface.IRunningObjectTable_Type;
   begin
      --  Check command line for RegServer/UnRegServer/Embedding

      if Run = Embedding then

         --  Tell framework COM objects are not in an InprocServer
         --  and the CanClose procedure should shutdown the server
         --  when Component_Count and Server_Lock_Count are zero.
         GNATCOM.Create.InProcServer := False;

         --  Store Main thread ID to allow for shut down of
         --  Mutli Threaded Servers
         Main_Thread_ID := GNATCOM.Utility.Get_Current_Thread_ID;

         --  Used to avoid dead locks on shutdown of server.
         --  CanClose will call CoReleaseServerProcess when
         --  server is ready to shut down that will suspend
         --  all COM access to server.
         CoAddRefServerProcess;

         --  Initialize Com Libraries
         case Use_Thread_Model is
            when Single =>
               GNATCOM.Initialize.Initialize_COM;
            when Multiple | Both =>
               GNATCOM.Initialize.Initialize_COM_Multi_Threaded;
         end case;
         Error_Check (CoInitializeSecurity);

         GNATCOM.Register.Register_Component_Category
           (GNATCOM.Create.Component_Categories);

         ROT := GNATOCX.IRunningObjectTable_Interface.GetRunningObjectTable;

         --  Start Factories and register them
         --  Creation of objects is suspended until every factory
         --  is registered.
         for N in
           Factory_Map.all'First .. Factory_Map.all'Last
         loop
            Factory_Map (N).pFactory :=
              new GNATCOM.Create.Factory.IClassFactory;
            Factory_Map (N).pFactory.Create := Factory_Map (N).Create;

            Error_Check
              (CoRegisterClassObject
                 (Factory_Map (N).CLSID'Access,
                  To_Pointer_To_IUnknown (Factory_Map (N).pFactory),
                  CLSCTX_LOCAL_SERVER,
                  REGCLS_MULTIPLEUSE or REGCLS_SUSPENDED,
                  Factory_Map (N).dwRegister'Access));

            if Factory_Map (N).Service_Name /= "" then
               declare
                  Mnk : constant GNATOCX.IMoniker_Interface.IMoniker_Type :=
                    GNATOCX.IMoniker_Interface.CreateClassMoniker
                      (Factory_Map (N).CLSID);
               begin
                  GNATOCX.IRunningObjectTable_Interface.Register
                    (ROT,
          GNATOCX.IRunningObjectTable_Interface.ROTFLAGS_REGISTRATIONKEEPSALIVE
                     or
                  GNATOCX.IRunningObjectTable_Interface.ROTFLAGS_ALLOWANYCLIENT
                        ,
                     To_Pointer_To_IUnknown (Factory_Map (N).pFactory),
                     GNATOCX.IMoniker_Interface.Pointer (Mnk),
                     Factory_Map (N).dwRegisterROT'Access);
               end;
            end if;
         end loop;

         --  All factories are registered, start allowing object
         --  creation.
         CoResumeClassObjects;

         --  Start Windows Message Loop
         GNATCOM.Utility.Message_Loop;

         --  Stop Factories and clean up
         for N in
           Factory_Map.all'First .. Factory_Map.all'Last
         loop
            Error_Check (CoRevokeClassObject (Factory_Map (N).dwRegister));

            if Factory_Map (N).Service_Name /= "" then
               GNATOCX.IRunningObjectTable_Interface.Revoke
                 (ROT,
                  Factory_Map (N).dwRegisterROT);
            end if;

            refcount :=
              GNATCOM.Create.Factory.Release (Factory_Map (N).pFactory);
         end loop;

         --  Uninitialize the COM libraries
         GNATCOM.Initialize.Uninitialize_COM;
      elsif Run = Regserver then
         GNATCOM.Register.Register_Type_Library (Retrieve_hInstance);

         GNATCOM.Initialize.Initialize_COM;

         --  Loop through objects and register them
         for F of Factory_Map.all loop
            GNATCOM.Register.Register_Local_Server
              (hInstance    => Retrieve_hInstance,
               CLSID        => F.CLSID,
               Name         => To_String (F.Name),
               Version      => To_String (F.Version),
               Description  => To_String (F.Description),
               Implemented_Categories =>
                 (if F.Implemented_Categories /= null
                  then F.Implemented_Categories.all
                  else GNATCOM.Types.GUID_Array'(2 .. 1 => <>)),
               Service_Name => To_String (F.Service_Name),
               APPID        => F.APPID);
         end loop;

         GNATCOM.Initialize.Uninitialize_COM;
      elsif Run = Unregserver then
         begin
            GNATCOM.Register.Unregister_Type_Library (LIBID);

            --  Loop through objects and unregister them
            for N in
              Factory_Map.all'First .. Factory_Map.all'Last
            loop
               GNATCOM.Register.Unregister_Server
                 (CLSID   => Factory_Map (N).CLSID,
                  Name    => To_String (Factory_Map (N).Name),
                  Version => To_String (Factory_Map (N).Version),
                  Implemented_Categories =>
                    (if Factory_Map (N).Implemented_Categories /= null then
                          Factory_Map (N).Implemented_Categories.all
                     else GNATCOM.Types.GUID_Array'(2 .. 1 => <>)));
            end loop;
         exception
            when GNATCOM.Register.REGISTRY_ERROR =>
               Ada.Text_IO.Put_Line ("Class not registered");
         end;
      else
         Display_Help;
      end if;
   end Init_Object;

   -----------------
   -- Error_Check --
   -----------------

   procedure Error_Check (Result : in GNATCOM.Types.HRESULT) is
   begin
      if GNATCOM.Errors.FAILED (Result) then
         case Result is
            when CO_E_OBJISREG =>
               raise ALREADY_REGISTERED_ERROR;
            when others =>
               GNATCOM.Errors.Error_Check (Result);
         end case;
      end if;
   end Error_Check;

end GNATCOM.Create.Local_Server;
