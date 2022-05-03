with GNATCOM.Create.COM_Interface;
with GNATCOM.Types;

generic
   with function EnumConnectionPoints
     (This   : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      ppEnum : GNATCOM.Types.Pointer_To_Pointer_To_IEnumConnectionPoints)
      return GNATCOM.Types.HRESULT;

   with function FindConnectionPoint
     (This : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      riid : GNATCOM.Types.Pointer_To_GUID;
      ppCP : GNATCOM.Types.Pointer_To_Pointer_To_IConnectionPoint)
      return GNATCOM.Types.HRESULT;
package GNATCOM.Create.IConnectionPointContainer is

   type Pointer_To_COM_Interface_Array is array (Positive range <>) of
     COM_Interface.Pointer_To_COM_Interface_Type;

   function FindConnectionPoint
     (Connections : Pointer_To_COM_Interface_Array;
      riid        : GNATCOM.Types.Pointer_To_GUID;
      ppCP        : GNATCOM.Types.Pointer_To_Pointer_To_IConnectionPoint)
      return GNATCOM.Types.HRESULT;

   function EnumConnectionPoints
     (Connections : Pointer_To_COM_Interface_Array;
      ppEnum      : GNATCOM.Types.Pointer_To_Pointer_To_IEnumConnectionPoints)
      return GNATCOM.Types.HRESULT;

   type af_IConnectionPointContainer_EnumConnectionPoints is access
     function (This   : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
               ppEnum :
               GNATCOM.Types.Pointer_To_Pointer_To_IEnumConnectionPoints)
               return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   type af_IConnectionPointContainer_FindConnectionPoint is access
     function (This : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
               riid : GNATCOM.Types.Pointer_To_GUID;
               ppCP : GNATCOM.Types.Pointer_To_Pointer_To_IConnectionPoint)
               return GNATCOM.Types.HRESULT
     with Convention => Stdcall;

   function IConnectionPointContainer_EnumConnectionPoints
     (This   : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      ppEnum : GNATCOM.Types.Pointer_To_Pointer_To_IEnumConnectionPoints)
      return GNATCOM.Types.HRESULT
   is (EnumConnectionPoints (This, ppEnum))
     with Convention => Stdcall;

   function IConnectionPointContainer_FindConnectionPoint
     (This : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      riid : GNATCOM.Types.Pointer_To_GUID;
      ppCP : GNATCOM.Types.Pointer_To_Pointer_To_IConnectionPoint)
      return GNATCOM.Types.HRESULT
   is (FindConnectionPoint (This, riid, ppCP))
     with Convention => Stdcall;

   type IConnectionPointContainer_Vtbl_Record is
      record
         IUnknown : COM_Interface.IUnknown_Vtbl_Record;
         EnumConnectionPoints :
         af_IConnectionPointContainer_EnumConnectionPoints :=
           IConnectionPointContainer_EnumConnectionPoints'Access;
         FindConnectionPoint :
         af_IConnectionPointContainer_FindConnectionPoint :=
           IConnectionPointContainer_FindConnectionPoint'Access;
      end record
     with Convention => C_Pass_By_Copy;

   IConnectionPointContainer_Vtbl :
   aliased IConnectionPointContainer_Vtbl_Record;

end GNATCOM.Create.IConnectionPointContainer;
