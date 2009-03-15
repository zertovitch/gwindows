
package vbtoada.VBtoAdaClass is

   function IVBtoAda_Display
     (This    : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      Message : GNATCOM.Types.BSTR)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IVBtoAda_Display);
   --  Display Message

   type IVBtoAda_Vtbl_Record is
      record
         IUnknown : GNATCOM.Create.COM_Interface.IUnknown_Vtbl_Record;
         Display  : af_IVBtoAda_Display :=
           IVBtoAda_Display'Access;
      end record;
   pragma Convention (C_Pass_By_Copy, IVBtoAda_Vtbl_Record);

   type Pointer_To_IVBtoAda_Vtbl_Record is access all IVBtoAda_Vtbl_Record;

   IVBtoAda_Vtbl : aliased IVBtoAda_Vtbl_Record;

      GUID_Map : aliased GNATCOM.Create.COM_Interface.GUID_Record_Array :=
        (1 => (IID_IVBtoAda, IVBtoAda_Vtbl'Address));


   type VBtoAdaClass_Type is
     new GNATCOM.Create.COM_Interface.CoClass_Type (GUID_Map'Access) with
      record
         null;
      end record;

   type Pointer_To_VBtoAdaClass_Type is
     access all VBtoAdaClass_Type;

   function Create
     return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type;

end vbtoada.VBtoAdaClass;

