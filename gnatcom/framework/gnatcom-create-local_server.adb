------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--         G N A T C O M . C R E A T E . L O C A L _ S E R V E R            --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Unchecked_Conversion;

with GNAT.IO; use GNAT.IO;

with GNATCOM.Initialize;
with GNATCOM.Register;
with GNATCOM.Errors;
with GNATCOM.Utility;

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

   function Retrieve_hInstance return Interfaces.C.long;
   pragma Import (C, Retrieve_hInstance, "rts_get_hInstance");

   procedure CoAddRefServerProcess;
   pragma Import (StdCall, CoAddRefServerProcess, "CoAddRefServerProcess");

   ------------------
   -- Display_Help --
   ------------------

   procedure Display_Help is
   begin
      Put_Line ("This is a local server for a COM object");
      Put_Line ("To register this server use:");
      Put_Line ("servername -RegServer");
      New_Line;
      Put_Line ("To unregister this server use:");
      Put_Line ("servername -UnregServer");
      New_Line;
      Put_Line ("To start the server up manually: (COM will normally do" &
                " this)");
      Put_Line ("servername -Embedding");
   end Display_Help;

   -----------------
   -- Init_Object --
   -----------------

   procedure Init_Object (LIBID : in GNATCOM.Types.GUID) is
      use type Interfaces.C.unsigned_long;

      function To_Pointer_To_IUnknown is
         new Ada.Unchecked_Conversion
        (GNATCOM.Create.Factory.Pointer_To_IClassFactory,
         GNATCOM.Types.Pointer_To_IUnknown);

      refcount : Interfaces.C.unsigned_long;
      pragma Warnings (Off, refcount);
   begin
      --  Check command line for RegServer/UnRegServer/Embedding

      if Argument_Count /= 1 then
         Display_Help;
      else
         if
           (Argument (1) = "/Embedding")
           or
           (Argument (1) = "-Embedding")
         then
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

            --  Start Factories and register them
            --  Creation of objects is suspended until every factory
            --  is registered.
            for N in
              Factory_Map.all'First .. (Factory_Map.all'Last)
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
            end loop;

            --  All factories are registered, start allowing object
            --  creation.
            CoResumeClassObjects;

            --  Start Windows Message Loop
            GNATCOM.Utility.Message_Loop;

            --  Stop Factories and clean up
            for N in
              Factory_Map.all'First .. (Factory_Map.all'Last)
            loop
               Error_Check (CoRevokeClassObject (Factory_Map (N).dwRegister));
               refcount :=
                 GNATCOM.Create.Factory.Release (Factory_Map (N).pFactory);
            end loop;

            --  Uninitialize the COM libraries
            GNATCOM.Initialize.Uninitialize_COM;
         elsif
           (Argument (1) = "/RegServer")
           or
           (Argument (1) = "-RegServer")
         then
            GNATCOM.Register.Register_Type_Library (Retrieve_hInstance);

            --  Loop through objects and register them
            for N in
              Factory_Map.all'First .. (Factory_Map.all'Last)
            loop
               GNATCOM.Register.Register_Local_Server
                 (hInstance    => Retrieve_hInstance,
                  CLSID        => Factory_Map (N).CLSID,
                  Name         => To_String (Factory_Map (N).Name),
                  Version      => To_String (Factory_Map (N).Version),
                  Description  => To_String (Factory_Map (N).Description));
            end loop;
         elsif
           (Argument (1) = "/UnregServer")
           or
           (Argument (1) = "-UnregServer")
         then
            begin
               GNATCOM.Register.Unregister_Type_Library (LIBID);

               --  Loop through objects and unregister them
               for N in
                 Factory_Map.all'First .. (Factory_Map.all'Last)
               loop
                  GNATCOM.Register.Unregister_Server
                    (CLSID   => Factory_Map (N).CLSID,
                     Name    => To_String (Factory_Map (N).Name),
                     Version => To_String (Factory_Map (N).Version));
               end loop;
            exception
               when GNATCOM.Register.REGISTRY_ERROR =>
                  Put_Line ("Class not registered");
            end;
         else
            Display_Help;
         end if;
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
