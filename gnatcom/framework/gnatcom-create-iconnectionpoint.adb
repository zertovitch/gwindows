with Ada.Unchecked_Conversion;
with System;
with GNATCOM.Dispinterface;
with GNATCOM.Errors;

package body GNATCOM.Create.IConnectionPoint is

   procedure IDispatch_Fire
     (ConnectionPoint : ConnectionPoint_Type;
      Dispid          : Interfaces.C.long)
   is
   begin
      for Connection of ConnectionPoint.Connections loop
         declare
            Disp : GNATCOM.Dispinterface.Dispinterface_Type;
         begin
            GNATCOM.Iinterface.AddRef (Connection);
            GNATCOM.Dispinterface.Initialize (Disp);
            GNATCOM.Dispinterface.Attach
              (Disp, GNATCOM.Dispinterface.To_Pointer_To_IDispatch
                 (GNATCOM.Iinterface.Address (Connection)));
            GNATCOM.Dispinterface.Invoke (Disp, Dispid);
         exception
            when others =>
               null;
         end;
      end loop;
   end IDispatch_Fire;

   procedure IPropertyNotifySink_Fire
     (ConnectionPoint : ConnectionPoint_Type;
      Dispid          : Interfaces.C.long)
   is
   begin
      for Connection of ConnectionPoint.Connections loop
         declare
            function To_Pointer_To_IPropertyNotifySink is
              new Ada.Unchecked_Conversion
                (System.Address,
                 GNATCOM.Types.Pointer_To_IPropertyNotifySink);

            Sink : GNATCOM.Types.Pointer_To_IPropertyNotifySink;
            Hr   : GNATCOM.Types.HRESULT;
         begin
            Sink := To_Pointer_To_IPropertyNotifySink
              (GNATCOM.Iinterface.Address (Connection));
            Hr   := Sink.Vtbl.OnChanged (Sink, Dispid);
         end;
      end loop;
   end IPropertyNotifySink_Fire;

   function IPropertyNotifySink_Advise
     (Object : not null Pointer_To_ConnectionPoint_Type;
      Sink   : not null access GNATCOM.Iinterface.Interface_Type;
      Cookie : in out GNATCOM.Types.DWORD)
     return GNATCOM.Types.HRESULT
   is
      use GNATCOM.Types;
   begin
      Object.Connections.Include (Object.Cookie, Sink.all);
      Cookie        := Object.Cookie;
      Object.Cookie := Object.Cookie + 1;
      return S_OK;
   end IPropertyNotifySink_Advise;

   function IDispatch_Advise
     (Object : not null Pointer_To_ConnectionPoint_Type;
      Sink   : not null access GNATCOM.Iinterface.Interface_Type;
      Cookie : in out GNATCOM.Types.DWORD)
     return GNATCOM.Types.HRESULT
   is
      use GNATCOM.Types;
      Success : Boolean := False;
   begin
      GNATCOM.Iinterface.Set_IID (Sink.all, IID_IDispatch);
      GNATCOM.Iinterface.Query (Sink.all, Sink.all, Success);
      if Success then
         Object.Connections.Include (Object.Cookie, Sink.all);
         Cookie        := Object.Cookie;
         Object.Cookie := Object.Cookie + 1;
         return GNATCOM.S_OK;
      else
         return GNATCOM.CONNECT_E_CANNOTCONNECT;
      end if;
   end IDispatch_Advise;

   function IConnectionPoint_GetConnectionInterface
     (This : access COM_Interface.COM_Interface_Type;
      pIID : GNATCOM.Types.Pointer_To_GUID)
      return GNATCOM.Types.HRESULT
   is
      use type GNATCOM.Types.Pointer_To_GUID;
      Object : constant Pointer_To_ConnectionPoint_Type :=
        Pointer_To_ConnectionPoint_Type (This.CoClass);
   begin
      if pIID = null then
         return GNATCOM.E_POINTER;
      else
         pIID.all := Object.Event_IID.all;
         return GNATCOM.S_OK;
      end if;
   end IConnectionPoint_GetConnectionInterface;

   function IConnectionPoint_GetConnectionPointContainer
     (This  : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      ppCPC : GNATCOM.Types.Pointer_To_Pointer_To_IConnectionPointContainer)
      return GNATCOM.Types.HRESULT
   is
      use type GNATCOM.Types.Pointer_To_Pointer_To_IConnectionPointContainer;
      function To_Pointer_To_Pointer_To_Void is
        new Ada.Unchecked_Conversion
          (GNATCOM.Types.Pointer_To_Pointer_To_IConnectionPointContainer,
           GNATCOM.Types.Pointer_To_Pointer_To_Void);
      Object : constant Pointer_To_ConnectionPoint_Type :=
        Pointer_To_ConnectionPoint_Type (This.CoClass);
      Result : GNATCOM.Types.HRESULT;
   begin
      if ppCPC = null then
         Result := GNATCOM.E_POINTER;
      else
         Result := COM_Interface.QueryInterface
           (Object.Container,
            GNATCOM.Types.IID_IConnectionPointContainer'Access,
            To_Pointer_To_Pointer_To_Void (ppCPC));
         if GNATCOM.Errors.SUCCEEDED (Result) then
            Result := GNATCOM.S_OK;
         else
            Result := GNATCOM.E_UNEXPECTED;
         end if;
      end if;
      return Result;
   end IConnectionPoint_GetConnectionPointContainer;

   function IConnectionPoint_Advise
     (This      : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pUnkSink  :        GNATCOM.Types.Pointer_To_IUnknown;
      pdwCookie : access GNATCOM.Types.DWORD)
      return GNATCOM.Types.HRESULT
   is
      use GNATCOM.Types;
      Object   : constant Pointer_To_ConnectionPoint_Type :=
        Pointer_To_ConnectionPoint_Type (This.CoClass);
      Sink     : aliased GNATCOM.Iinterface.Interface_Type;
      Success  : Boolean := False;
      Result   : GNATCOM.Types.HRESULT;
   begin
      if pUnkSink /= null and then pdwCookie /= null then
         GNATCOM.Iinterface.Set_IID (Sink, Object.Event_IID.all);
         GNATCOM.Iinterface.Attach (Sink, pUnkSink, Success);
         if Success then
            Result := Object.Advise.all (Object, Sink'Access, pdwCookie.all);
         else
            Result := GNATCOM.CONNECT_E_CANNOTCONNECT;
         end if;
      else
         Result := GNATCOM.E_POINTER;
      end if;
      return Result;
   exception
      when others =>
         return GNATCOM.E_UNEXPECTED;
   end IConnectionPoint_Advise;

   function IConnectionPoint_Unadvise
     (This     : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      dwCookie : GNATCOM.Types.DWORD)
      return GNATCOM.Types.HRESULT
   is
      Object : constant Pointer_To_ConnectionPoint_Type :=
        Pointer_To_ConnectionPoint_Type (This.CoClass);
      Result : GNATCOM.Types.HRESULT;
   begin
      if Object.Connections.Contains (dwCookie) then
         Object.Connections.Delete (dwCookie);
         Result := GNATCOM.S_OK;
      else
         Result := GNATCOM.E_POINTER;
      end if;
      return Result;
   exception
      when others =>
         return GNATCOM.E_UNEXPECTED;
   end IConnectionPoint_Unadvise;

   function IConnectionPoint_EnumConnections
     (This   : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      ppEnum : GNATCOM.Types.Pointer_To_Pointer_To_IEnumConnections)
      return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, ppEnum);
   begin
      return GNATCOM.E_NOTIMPL;
   end IConnectionPoint_EnumConnections;

   function Create
     (Event_IID : GNATCOM.Types.Pointer_To_GUID;
      Container : COM_Interface.Pointer_To_COM_Interface_Type)
      return COM_Interface.Pointer_To_COM_Interface_Type
   is
      use GNATCOM.Types;
      Object : constant Pointer_To_ConnectionPoint_Type :=
        new ConnectionPoint_Type;
   begin
      Object.Event_IID := Event_IID;
      Object.Container := Container;
      if Event_IID.all = GNATCOM.Types.IID_IPropertyNotifySink then
         Object.Advise := IPropertyNotifySink_Advise'Access;
         Object.Fire   := IPropertyNotifySink_Fire'Access;
      else
         Object.Advise := IDispatch_Advise'Access;
         Object.Fire   := IDispatch_Fire'Access;
      end if;
      return GNATCOM.Create.COM_Interface.Create_Object
        (COM_Interface.Pointer_To_CoClass (Object));
   end Create;

   procedure Fire
     (ConnectionPoint : ConnectionPoint_Type;
      Dispid          : Interfaces.C.long)
   is
   begin
      ConnectionPoint.Fire.all (ConnectionPoint, Dispid);
   end Fire;

end GNATCOM.Create.IConnectionPoint;
