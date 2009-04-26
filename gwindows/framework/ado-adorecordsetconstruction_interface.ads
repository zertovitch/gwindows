with GNATCOM.Iinterface;

package ADO.ADORecordsetConstruction_Interface is

   type ADORecordsetConstruction_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out ADORecordsetConstruction_Type);

   function Pointer (This : ADORecordsetConstruction_Type)
     return Pointer_To_ADORecordsetConstruction;

   procedure Attach (This    : in out ADORecordsetConstruction_Type;
                     Pointer : in     Pointer_To_ADORecordsetConstruction);

   function Get_Rowset
     (This     : ADORecordsetConstruction_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;

   procedure Put_Rowset
     (This     : ADORecordsetConstruction_Type;
      ppRowset : GNATCOM.Types.Pointer_To_IUnknown);

   function Get_Chapter
     (This      : ADORecordsetConstruction_Type)
     return Interfaces.C.long;

   procedure Put_Chapter
     (This      : ADORecordsetConstruction_Type;
      plChapter : Interfaces.C.long);

   function Get_RowPosition
     (This     : ADORecordsetConstruction_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;

   procedure Put_RowPosition
     (This     : ADORecordsetConstruction_Type;
      ppRowPos : GNATCOM.Types.Pointer_To_IUnknown);

end ADO.ADORecordsetConstruction_Interface;
