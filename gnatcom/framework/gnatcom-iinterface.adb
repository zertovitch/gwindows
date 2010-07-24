------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                    G N A T C O M . I N T E R F A C E                     --
--                                                                          --
--                                B o d y                                   --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2006 David Botton                   --
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

with Ada.Exceptions;

with GNATCOM.Errors;
with GNATCOM.BSTR;
with GNATCOM.VARIANT;

package body GNATCOM.Iinterface is
   use type System.Address;

   GIT : Interface_Type;

   function Get_GIT return GNATCOM.Types.Pointer_To_IGlobalInterfaceTable;
   --  Check to see if GIT was already retrieved and if not
   --  retrieve it for use

   procedure Error_Check (Result : in GNATCOM.Types.HRESULT);
   --  Check for COM Interface specific errors

   type COSERVERINFO is
      record
         dwReserverd1 : GNATCOM.Types.DWORD := 0;
         pwszName     : GNATCOM.Types.BSTR;
         pAuthInfo    : System.Address := System.Null_Address;
         dwReserverd2 : GNATCOM.Types.DWORD := 0;
      end record;
   pragma Convention (C_Pass_By_Copy, COSERVERINFO);

   type MULTI_QI is
      record
         pIID : GNATCOM.Types.Pointer_To_GUID;
         pItf : GNATCOM.Types.Pointer_To_Void := System.Null_Address;
         hr   : GNATCOM.Types.HRESULT := 0;
      end record;
   pragma Convention (C_Pass_By_Copy, MULTI_QI);

   type Array_Of_MULTI_QI is array (1 .. 1) of MULTI_QI;

   function To_Pointer_To_IGlobalInterfaceTable is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void,
      GNATCOM.Types.Pointer_To_IGlobalInterfaceTable);

   function CoCreateInstance
     (rclsid       : GNATCOM.Types.Pointer_To_GUID;
      pUnkOuter    : GNATCOM.Types.Pointer_To_Void;
      dwClsContext : GNATCOM.Types.CLSCTX;
      riid         : GNATCOM.Types.Pointer_To_GUID;
      ppv          : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, CoCreateInstance, "CoCreateInstance");

   function CoCreateInstanceEx
     (rclsid       : GNATCOM.Types.Pointer_To_GUID;
      pUnkOuter    : GNATCOM.Types.Pointer_To_Void;
      dwClsContext : GNATCOM.Types.CLSCTX;
      pServerInfo  : access COSERVERINFO;
      cmq          : Integer;
      pResults     : Array_Of_MULTI_QI)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, CoCreateInstanceEx, "CoCreateInstanceEx");

   function CLSIDFromProgID (lpszProgID : GNATCOM.Types.BSTR;
                             lpClsid    : GNATCOM.Types.Pointer_To_GUID)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, CLSIDFromProgID, "CLSIDFromProgID");

   function CoGetClassObject
     (rclsid       : GNATCOM.Types.Pointer_To_GUID;
      dwClsContext : GNATCOM.Types.CLSCTX;
      pvReserved   : GNATCOM.Types.Pointer_To_Void;
      riid         : GNATCOM.Types.Pointer_To_GUID;
      ppv          : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, CoGetClassObject, "CoGetClassObject");

   function CoGetObject
     (pszName      : GNATCOM.Types.BSTR;
      pBindOptions : System.Address := System.Null_Address;
      riid         : GNATCOM.Types.Pointer_To_GUID;
      ppv          : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Import (StdCall, CoGetObject, "CoGetObject");

   ------------
   -- AddRef --
   ------------

   procedure AddRef (This : in Interface_Type) is
   begin
      if Address (This) /= System.Null_Address then
         declare
            Result : Interfaces.C.unsigned_long;
            pragma Warnings (Off, Result);
         begin
            Result := Pointer (This).Vtbl.AddRef (Pointer (This));
         end;
      end if;
   end AddRef;

   -------------
   -- Address --
   -------------

   function Address (This : Interface_Type) return System.Address is
   begin
      return This.Interface_Address;
   end Address;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (This : in out Interface_Type) is
   begin
      AddRef (This);
   end Adjust;

   ------------
   -- Attach --
   ------------

   procedure Attach
     (This : in out Interface_Type;
      From : in     System.Address)
   is
   begin
      Free (This);

      This.Interface_Address := From;
   end Attach;

   ------------
   -- Attach --
   ------------

   procedure Attach
     (This : in out Interface_Type;
      From : in     GNATCOM.Types.Pointer_To_IUnknown)
   is
      Temp : Interface_Type;
   begin
      Free (This);

      Attach (Temp, From.all'Address);

      Query (This, Temp);
   end Attach;

   ------------
   -- Attach --
   ------------

   procedure Attach
     (This : in out Interface_Type;
      From : in     GNATCOM.Types.VARIANT)
   is
   begin
      Attach (This, GNATCOM.VARIANT.To_Pointer_To_IUnknown (From));
   end Attach;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Interface_Type) is
   begin
      Release (This);
      This.Interface_Address := System.Null_Address;
   end Free;

   ------------
   -- Create --
   ------------

   procedure Create
     (This        : in out Interface_Type;
      From        : in     GNATCOM.Types.GUID;
      Server_Type : in     GNATCOM.Types.CLSCTX := GNATCOM.Types.CLSCTX_ALL)
   is
      Ref_To_CLSID : aliased GNATCOM.Types.GUID := From;
      IUnknown     : Interface_Type;
   begin
      Free (This);

      Error_Check (CoCreateInstance
                   (rclsid       =>
                      Ref_To_CLSID'Unchecked_Access,
                    pUnkOuter    => System.Null_Address,
                    dwClsContext => Server_Type,
                    riid         => GNATCOM.Types.IID_IUnknown'Access,
                    ppv          =>
                      IUnknown.Interface_Address'Unchecked_Access));

      Error_Check (QueryInterface (IUnknown,
                                   This.IID,
                                   This.Interface_Address'Access));
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (This        : in out Interface_Type;
      From        : in     String;
      Server_Type : in     GNATCOM.Types.CLSCTX := GNATCOM.Types.CLSCTX_ALL)
   is
      Prog_ID  : constant GNATCOM.Types.BSTR := GNATCOM.BSTR.To_BSTR (From);
      Class_ID : aliased GNATCOM.Types.GUID;
   begin
      Error_Check (CLSIDFromProgID (Prog_ID, Class_ID'Unchecked_Access));
      GNATCOM.BSTR.Free (Prog_ID);
      Create (This, Class_ID, Server_Type);
   end Create;

   ------------
   -- Create --
   ------------

   procedure Create
     (This        : in out Interface_Type;
      From        : in     GNATCOM.Types.GUID;
      Key         : in     GNATCOM.Types.BSTR;
      Free_Key    : in     Boolean              := True;
      Server_Type : in     GNATCOM.Types.CLSCTX := GNATCOM.Types.CLSCTX_ALL)
   is
      function To_Pointer_To_IFactory2 is
         new Ada.Unchecked_Conversion
           (System.Address,
            GNATCOM.Types.Pointer_To_IClassFactory2);

      Ref_To_CLSID    : aliased GNATCOM.Types.GUID := From;
      Factory_Address : aliased System.Address := System.Null_Address;
      Factory         : GNATCOM.Types.Pointer_To_IClassFactory2;
   begin
      Free (This);

      Error_Check (CoGetClassObject
                   (rclsid       => Ref_To_CLSID'Unchecked_Access,
                    dwClsContext => Server_Type,
                    pvReserved   => System.Null_Address,
                    riid         => GNATCOM.Types.IID_IClassFactory2'Access,
                    ppv          => Factory_Address'Unchecked_Access));

      Factory := To_Pointer_To_IFactory2 (Factory_Address);

      Error_Check (Factory.Vtbl.CreateInstanceLic
                   (Factory,
                    pUnkOuter    => null,
                    pUnkReserved => null,
                    riid         => This.IID'Unchecked_Access,
                    bstrKey      => Key,
                    ppv          => This.Interface_Address'Unchecked_Access));

      declare
         Result : Interfaces.C.unsigned_long;
         pragma Warnings (Off, Result);
      begin
         Result := Factory.Vtbl.Release (Factory);
      end;

      if Free_Key then
         GNATCOM.BSTR.Free (Key);
      end if;

   end Create;

   -----------------------
   -- CreateFromMoniker --
   -----------------------

   procedure CreateFromMoniker
     (This : in out Interface_Type;
      From : in     String)
   is
      Name : constant GNATCOM.Types.BSTR := GNATCOM.BSTR.To_BSTR (From);
   begin
      Error_Check
        (CoGetObject (pszName => Name,
                      riid    => This.IID'Unchecked_Access,
                      ppv     => This.Interface_Address'Unchecked_Access));

      GNATCOM.BSTR.Free (Name);
   end CreateFromMoniker;

   ------------------
   -- CreateRemote --
   ------------------

   procedure CreateRemote
     (This   : in out Interface_Type;
      From   : in     GNATCOM.Types.GUID;
      Server : in     String)
   is
      ServerInfo   : aliased COSERVERINFO;
      MQs          : Array_Of_MULTI_QI;
      Ref_To_CLSID : aliased GNATCOM.Types.GUID := From;
   begin
      Free (This);

      ServerInfo.pwszName := GNATCOM.BSTR.To_BSTR (Server);
      MQs (1).pIID := This.IID'Unchecked_Access;

      Error_Check (CoCreateInstanceEx
                   (rclsid       =>
                      Ref_To_CLSID'Unchecked_Access,
                    pUnkOuter    => System.Null_Address,
                    dwClsContext => GNATCOM.Types.CLSCTX_REMOTE_SERVER,
                    pServerInfo  => ServerInfo'Access,
                    cmq          => MQs'Last,
                    pResults     => MQs));

      Error_Check (MQs (1).hr);

      This.Interface_Address := MQs (1).pItf;

      GNATCOM.BSTR.Free (ServerInfo.pwszName);
   end CreateRemote;

   ------------------
   -- CreateRemote --
   ------------------

   procedure CreateRemote
     (This   : in out Interface_Type;
      From   : in     String;
      Server : in     String)
   is
      Prog_ID  : constant GNATCOM.Types.BSTR := GNATCOM.BSTR.To_BSTR (From);
      Class_ID : aliased GNATCOM.Types.GUID;
   begin
      Error_Check (CLSIDFromProgID (Prog_ID, Class_ID'Unchecked_Access));
      GNATCOM.BSTR.Free (Prog_ID);
      CreateRemote (This, Class_ID, Server);
   end CreateRemote;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Interface_Type) is
   begin
      Free (This);
   end Finalize;

   ----------
   -- Free --
   ----------

   procedure Free (This : GNATCOM.Types.BSTR) is
   begin
      GNATCOM.BSTR.Free (This);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (This : GNATCOM.Types.VARIANT) is
   begin
      GNATCOM.VARIANT.Free (This);
   end Free;

   ---------
   -- IID --
   ---------

   function IID (This : Interface_Type) return GNATCOM.Types.GUID is
   begin
      return This.IID;
   end IID;

   -----------------
   -- Is_Attached --
   -----------------

   function Is_Attached (This : Interface_Type) return Boolean is
   begin
      return This.Interface_Address /= System.Null_Address;
   end Is_Attached;

   -------------
   -- Pointer --
   -------------

   function Pointer
     (This : Interface_Type)
      return GNATCOM.Types.Pointer_To_IUnknown
   is
   begin
      return To_Pointer_To_IUnknown (Address (This));
   end Pointer;

   -------------------------------
   -- To_VARIANT_From_Interface --
   -------------------------------

   function To_VARIANT_From_Interface (From : Interface_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      AddRef (From);
      return GNATCOM.VARIANT.To_VARIANT
        (To_Pointer_To_IUnknown (Address (From)));
   end To_VARIANT_From_Interface;

   -----------
   -- Query --
   -----------

   procedure Query
     (This : in out Interface_Type;
      From : in     Interface_Type'class)
   is
   begin
      Error_Check (QueryInterface (From,
                                   This.IID,
                                   This.Interface_Address'Access));
   end Query;

   -----------
   -- Query --
   -----------

   procedure Query
     (This    : in out Interface_Type;
      From    : in     Interface_Type'Class;
      Success : in out Boolean)
   is
   begin
      if GNATCOM.Errors.SUCCEEDED
        (QueryInterface (From, This.IID, This.Interface_Address'Access))
      then
         Success := True;
      else
         Success := False;
      end if;
   end Query;

   --------------------
   -- QueryInterface --
   --------------------

   function QueryInterface
     (This               : in     Interface_Type;
      IID                : in     GNATCOM.Types.GUID;
      Pointer_To_Pointer : access GNATCOM.Types.Pointer_To_Void)
      return GNATCOM.Types.HRESULT
   is
      Ref_To_IID : aliased GNATCOM.Types.GUID := IID;
      Result     : aliased GNATCOM.Types.Pointer_To_Void :=
        System.Null_Address;
      HR         : GNATCOM.Types.HRESULT;
   begin
      HR := Pointer (This).Vtbl.QueryInterface (Pointer (This),
                                                  Ref_To_IID'Unchecked_Access,
                                                  Result'Unchecked_Access);
      Pointer_To_Pointer.all := Result;
      return HR;
   end QueryInterface;

   -------------
   -- Release --
   -------------

   procedure Release (This : in Interface_Type) is
   begin
      if Address (This) /= System.Null_Address then
         declare
            Result : Interfaces.C.unsigned_long;
            pragma Warnings (Off, Result);
         begin
            Result := Pointer (This).Vtbl.Release (Pointer (This));
         end;
      end if;
   end Release;

   -------------
   -- Set_IID --
   -------------

   procedure Set_IID
     (This : in out Interface_Type;
      IID  : in     GNATCOM.Types.GUID)
   is
   begin
      This.IID := IID;
   end Set_IID;

   -------------
   -- IsEqual --
   -------------

   function IsEqual (Left  : in Interface_Type;
                     Right : in Interface_Type'Class)
                    return Boolean
   is
      Left_Object  : Interface_Type;
      Right_Object : Interface_Type;
   begin
      Query (Left_Object, Left);
      Query (Right_Object, Right);
      return Address (Left_Object) = Address (Right_Object);
   end IsEqual;

   -------------
   -- Get_GIT --
   -------------

   function Get_GIT return GNATCOM.Types.Pointer_To_IGlobalInterfaceTable is
   begin
      if Address (GIT) = System.Null_Address then
         Set_IID (GIT, GNATCOM.Types.IID_IGlobalInterfaceTable);
         Create (GIT, GNATCOM.Types.CLSID_StdGlobalInterfaceTable);
      end if;

      return To_Pointer_To_IGlobalInterfaceTable (Address (GIT));
   end Get_GIT;

   ----------------
   -- Put_In_GIT --
   ----------------

   function Put_In_GIT (This : Interface_Type) return GIT_Cookie is
      PGIT   : constant GNATCOM.Types.Pointer_To_IGlobalInterfaceTable :=
        Get_GIT;
      Cookie : aliased Interfaces.C.unsigned_long;
      IID    : aliased GNATCOM.Types.GUID := This.IID;
   begin
      Error_Check
        (PGIT.Vtbl.RegisterInterfaceInGlobal (PGIT,
                                              Pointer (This),
                                              IID'Unchecked_Access,
                                              Cookie'Unchecked_Access));
      return GIT_Cookie (Cookie);
   end Put_In_GIT;

   ---------------------
   -- Remove_From_GIT --
   ---------------------

   procedure Remove_From_GIT (Cookie : in GIT_Cookie)
   is
      PGIT : constant GNATCOM.Types.Pointer_To_IGlobalInterfaceTable :=
        Get_GIT;
   begin
      Error_Check (PGIT.Vtbl.RevokeInterfaceFromGlobal
                   (PGIT,
                    Interfaces.C.unsigned_long (Cookie)));
   end Remove_From_GIT;

   ---------------------
   -- Attach_From_GIT --
   ---------------------

   procedure Attach_From_GIT (This   : in out Interface_Type;
                              Cookie : in     GIT_Cookie)
   is
      PGIT : constant GNATCOM.Types.Pointer_To_IGlobalInterfaceTable :=
        Get_GIT;
   begin
      Free (This);

      Error_Check (PGIT.Vtbl.GetInterfaceFromGlobal
                   (PGIT,
                    Interfaces.C.unsigned_long (Cookie),
                    This.IID'Unchecked_Access,
                    This.Interface_Address'Unchecked_Access));
   end Attach_From_GIT;

   -----------------
   -- Error_Check --
   -----------------

   procedure Error_Check (Result : in GNATCOM.Types.HRESULT) is
   begin
      GNATCOM.Errors.Set_Last_HRESULT (Result);

      if GNATCOM.Errors.FAILED (Result) then
         declare
            Message : constant String := GNATCOM.Errors.To_String (Result);
         begin
            case Result is
               when REGDB_E_CLASSNOTREG =>
                  Ada.Exceptions.Raise_Exception
                    (CLASS_NOT_REGISTERED_ERROR'Identity,
                     Message);
               when CLASS_E_CLASSNOTAVAILABLE =>
                  Ada.Exceptions.Raise_Exception
                    (CLASS_NOT_AVAILABLE_ERROR'Identity,
                     Message);
               when CLASS_E_CLASSNOTLICENSED =>
                  Ada.Exceptions.Raise_Exception
                    (CLASS_NOT_LICENSED_ERROR'Identity,
                     Message);
               when CO_E_CLASSSTRING =>
                  Ada.Exceptions.Raise_Exception
                    (INVALID_PROGID_ERROR'Identity,
                     Message);
               when CO_E_APPNOTFOUND =>
                  Ada.Exceptions.Raise_Exception
                    (SERVER_FILE_NOT_FOUND_ERROR'Identity,
                     Message);
               when CO_E_DLLNOTFOUND =>
                  Ada.Exceptions.Raise_Exception
                    (SERVER_FILE_NOT_FOUND_ERROR'Identity,
                     Message);
               when CO_E_ERRORINDLL =>
                  Ada.Exceptions.Raise_Exception
                    (SERVER_ERROR'Identity,
                     Message);
               when CO_E_APPDIDNTREG =>
                  Ada.Exceptions.Raise_Exception
                    (SERVER_ERROR'Identity,
                     Message);
               when others =>
                  GNATCOM.Errors.Error_Check (Result);
            end case;
         end;
      end if;
   end Error_Check;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Object : in GNATCOM.Types.GUID) return GNATCOM.Types.BSTR
   is
      function To_Pointer_To_IFactory2 is
         new Ada.Unchecked_Conversion
        (System.Address,
         GNATCOM.Types.Pointer_To_IClassFactory2);

      Ref_To_CLSID    : aliased GNATCOM.Types.GUID := Object;
      Factory_Address : aliased System.Address := System.Null_Address;
      Factory         : GNATCOM.Types.Pointer_To_IClassFactory2;
      Key_Local       : aliased GNATCOM.Types.BSTR;
   begin
      Error_Check (CoGetClassObject
                   (rclsid       => Ref_To_CLSID'Unchecked_Access,
                    dwClsContext => GNATCOM.Types.CLSCTX_ALL,
                    pvReserved   => System.Null_Address,
                    riid         => GNATCOM.Types.IID_IClassFactory2'Access,
                    ppv          => Factory_Address'Unchecked_Access));

      Factory := To_Pointer_To_IFactory2 (Factory_Address);

      Error_Check (Factory.Vtbl.RequestLicKey
                   (Factory,
                    0,
                    Key_Local'Unchecked_Access));

      declare
         Result : Interfaces.C.unsigned_long;
         pragma Warnings (Off, Result);
      begin
         Result := Factory.Vtbl.Release (Factory);
      end;

      return Key_Local;
   end Get_Key;

end GNATCOM.Iinterface;
