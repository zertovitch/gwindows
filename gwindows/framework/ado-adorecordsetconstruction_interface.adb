with GNATCOM.Errors;

package body ADO.ADORecordsetConstruction_Interface is

   procedure Initialize (This : in out ADORecordsetConstruction_Type) is
   begin
      Set_IID (This, IID_ADORecordsetConstruction);
   end Initialize;

   function Pointer (This : ADORecordsetConstruction_Type)
     return Pointer_To_ADORecordsetConstruction
   is
   begin
      return To_Pointer_To_ADORecordsetConstruction (Address (This));
   end Pointer;

   procedure Attach (This    : in out ADORecordsetConstruction_Type;
                     Pointer : in     Pointer_To_ADORecordsetConstruction)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_Rowset
     (This     : ADORecordsetConstruction_Type)
     return GNATCOM.Types.Pointer_To_IUnknown
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IUnknown;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Rowset
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Rowset;

   procedure Put_Rowset
     (This     : ADORecordsetConstruction_Type;
      ppRowset : GNATCOM.Types.Pointer_To_IUnknown)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Rowset
         (Pointer (This),
          ppRowset));

   end Put_Rowset;

   function Get_Chapter
     (This      : ADORecordsetConstruction_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Chapter
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Chapter;

   procedure Put_Chapter
     (This      : ADORecordsetConstruction_Type;
      plChapter : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Chapter
         (Pointer (This),
          plChapter));

   end Put_Chapter;

   function Get_RowPosition
     (This     : ADORecordsetConstruction_Type)
     return GNATCOM.Types.Pointer_To_IUnknown
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IUnknown;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_RowPosition
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_RowPosition;

   procedure Put_RowPosition
     (This     : ADORecordsetConstruction_Type;
      ppRowPos : GNATCOM.Types.Pointer_To_IUnknown)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_RowPosition
         (Pointer (This),
          ppRowPos));

   end Put_RowPosition;

end ADO.ADORecordsetConstruction_Interface;
