with GNATCOM.Create.COM_Interface;
with GNATCOM.Types;

generic
   type Cursor is private;
   with function Has_Element (Position : Cursor) return Boolean;
   with procedure Next (Position : in out Cursor);
   with function To_VARIANT (Position : Cursor) return GNATCOM.Types.VARIANT;
package GNATCOM.Create.IEnumVARIANTs is

   type VARIANT_Array is array (Natural) of GNATCOM.Types.VARIANT;
   type VARIANT_ARRAY_Access is access all VARIANT_Array;

   function IEnumVARIANT_Next
     (This         : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      celt         : GNATCOM.Types.ULONG;
      rgVar        : VARIANT_ARRAY_Access;
      pCeltFetched : GNATCOM.Types.Pointer_To_ULONG)
      return GNATCOM.Types.HRESULT
     with Convention => StdCall;

   function IEnumVARIANT_Skip
     (This : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      celt : GNATCOM.Types.ULONG)
      return GNATCOM.Types.HRESULT
     with Convention => StdCall;

   function IEnumVARIANT_Reset
     (This : access GNATCOM.Create.COM_Interface.COM_Interface_Type)
      return GNATCOM.Types.HRESULT
     with Convention => StdCall;

   function IEnumVARIANT_Clone
     (This   : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      ppEnum : GNATCOM.Types.Pointer_To_Pointer_To_IEnumVARIANT)
      return GNATCOM.Types.HRESULT
     with Convention => StdCall;

   type af_IEnumVARIANT_Next is access
     function (This : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
               celt         : GNATCOM.Types.ULONG;
               rgVar        : VARIANT_ARRAY_Access;
               pCeltFetched : GNATCOM.Types.Pointer_To_ULONG)
               return GNATCOM.Types.HRESULT
     with Convention => StdCall;

   type af_IEnumVARIANT_Skip is access
     function (This : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
               celt : GNATCOM.Types.ULONG)
               return GNATCOM.Types.HRESULT
     with Convention => StdCall;

   type af_IEnumVARIANT_Reset is access
     function (This : access GNATCOM.Create.COM_Interface.COM_Interface_Type)
               return GNATCOM.Types.HRESULT
     with Convention => StdCall;

   type af_IEnumVARIANT_Clone is access
     function (This   : access GNATCOM.Create.COM_Interface.COM_Interface_Type;
               ppEnum : GNATCOM.Types.Pointer_To_Pointer_To_IEnumVARIANT)
               return GNATCOM.Types.HRESULT
     with Convention => StdCall;

   type IEnumVARIANT_Vtbl_Record is
      record
         IUnknown : GNATCOM.Create.COM_Interface.IUnknown_Vtbl_Record;
         Next     : af_IEnumVARIANT_Next  := IEnumVARIANT_Next'Access;
         Skip     : af_IEnumVARIANT_Skip  := IEnumVARIANT_Skip'Access;
         Reset    : af_IEnumVARIANT_Reset := IEnumVARIANT_Reset'Access;
         Clone    : af_IEnumVARIANT_Clone := IEnumVARIANT_Clone'Access;
      end record
     with Convention => C_Pass_By_Copy;

   IEnumVARIANT_Vtbl : aliased IEnumVARIANT_Vtbl_Record;

   GUID_Map : aliased GNATCOM.Create.COM_Interface.GUID_Record_Array :=
     (1 => (GNATCOM.Types.IID_IEnumVARIANT, IEnumVARIANT_Vtbl'Address));

   type EnumVARIANT_Type is
     new GNATCOM.Create.COM_Interface.CoClass_Type (GUID_Map'Access) with
      record
         First    : Cursor;
         Position : Cursor;
      end record;

   type Pointer_To_EnumVARIANT_Type is access all EnumVARIANT_Type;

   function Create (First : Cursor)
             return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type;

end GNATCOM.Create.IEnumVARIANTs;
