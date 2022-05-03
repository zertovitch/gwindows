with Ada.Containers.Ordered_Maps;
with GNATCOM.Create.COM_Interface;
with GNATCOM.Iinterface;
with GNATCOM.Types;

package GNATCOM.Create.IConnectionPoint is

   type af_IConnectionPoint_GetConnectionInterface is access
     function (This : access COM_Interface.COM_Interface_Type;
               pIID : GNATCOM.Types.Pointer_To_GUID)
               return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type af_IConnectionPoint_GetConnectionPointContainer is access
     function (This  : access COM_Interface.COM_Interface_Type;
               ppCPC :
               GNATCOM.Types.Pointer_To_Pointer_To_IConnectionPointContainer)
               return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type af_IConnectionPoint_Advise is access
     function (This      : access COM_Interface.COM_Interface_Type;
               pUnkSink  : GNATCOM.Types.Pointer_To_IUnknown;
               pdwCookie : access GNATCOM.Types.DWORD)
               return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type af_IConnectionPoint_Unadvise is access
     function (This     : access
                 GNATCOM.Create.COM_Interface.COM_Interface_Type;
               dwCookie : GNATCOM.Types.DWORD)
               return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type af_IConnectionPoint_EnumConnections is access
     function (This   : access COM_Interface.COM_Interface_Type;
               ppEnum : GNATCOM.Types.Pointer_To_Pointer_To_IEnumConnections)
               return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   function IConnectionPoint_GetConnectionInterface
     (This : access COM_Interface.COM_Interface_Type;
      pIID : GNATCOM.Types.Pointer_To_GUID)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   function IConnectionPoint_GetConnectionPointContainer
     (This  : access COM_Interface.COM_Interface_Type;
      ppCPC : GNATCOM.Types.Pointer_To_Pointer_To_IConnectionPointContainer)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   function IConnectionPoint_Advise
     (This      : access COM_Interface.COM_Interface_Type;
      pUnkSink  : GNATCOM.Types.Pointer_To_IUnknown;
      pdwCookie : access GNATCOM.Types.DWORD)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   function IConnectionPoint_Unadvise
     (This     : access COM_Interface.COM_Interface_Type;
      dwCookie : GNATCOM.Types.DWORD)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   function IConnectionPoint_EnumConnections
     (This   : access COM_Interface.COM_Interface_Type;
      ppEnum : GNATCOM.Types.Pointer_To_Pointer_To_IEnumConnections)
      return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type IConnectionPoint_Vtbl_Record is
      record
         IUnknown                    : COM_Interface.IUnknown_Vtbl_Record;
         GetConnectionInterface      :
         af_IConnectionPoint_GetConnectionInterface :=
           IConnectionPoint_GetConnectionInterface'Access;
         GetConnectionPointContainer :
         af_IConnectionPoint_GetConnectionPointContainer :=
           IConnectionPoint_GetConnectionPointContainer'Access;
         Advise                      : af_IConnectionPoint_Advise :=
           IConnectionPoint_Advise'Access;
         Unadvise                    : af_IConnectionPoint_Unadvise :=
           IConnectionPoint_Unadvise'Access;
         EnumConnections             : af_IConnectionPoint_EnumConnections :=
           IConnectionPoint_EnumConnections'Access;
      end record
     with Convention => C_Pass_By_Copy;

   IConnectionPoint_Vtbl : aliased IConnectionPoint_Vtbl_Record;

   GUID_Map : aliased COM_Interface.GUID_Record_Array :=
     (1 =>
        (GNATCOM.Types.IID_IConnectionPoint, IConnectionPoint_Vtbl'Address));

   package DWORD_Interface_Maps is new Ada.Containers.Ordered_Maps
     (GNATCOM.Types.DWORD,
      GNATCOM.Iinterface.Interface_Type,
      GNATCOM.Types."<",
      GNATCOM.Iinterface."=");

   type ConnectionPoint_Type is
     new COM_Interface.CoClass_Type (GUID_Map'Access) with
      record
         Event_IID   : GNATCOM.Types.Pointer_To_GUID;
         Container   : COM_Interface.Pointer_To_COM_Interface_Type;
         Cookie      : GNATCOM.Types.DWORD := 1;
         Connections : DWORD_Interface_Maps.Map;
         --  Saves the IDispatch references to the clients.
      end record;

   type Pointer_To_ConnectionPoint_Type is access all ConnectionPoint_Type;

   function Create
     (Event_IID : GNATCOM.Types.Pointer_To_GUID;
      Container : COM_Interface.Pointer_To_COM_Interface_Type)
      return COM_Interface.Pointer_To_COM_Interface_Type;

   procedure Fire
     (ConnectionPoint : ConnectionPoint_Type;
      Dispid          : Interfaces.C.long);

end GNATCOM.Create.IConnectionPoint;
