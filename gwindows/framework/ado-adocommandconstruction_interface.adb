with GNATCOM.Errors;

package body ADO.ADOCommandConstruction_Interface is

   procedure Initialize (This : in out ADOCommandConstruction_Type) is
   begin
      Set_IID (This, IID_ADOCommandConstruction);
   end Initialize;

   function Pointer (This : ADOCommandConstruction_Type)
     return Pointer_To_ADOCommandConstruction
   is
   begin
      return To_Pointer_To_ADOCommandConstruction (Address (This));
   end Pointer;

   procedure Attach (This    : in out ADOCommandConstruction_Type;
                     Pointer : in     Pointer_To_ADOCommandConstruction)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_OLEDBCommand
     (This           : ADOCommandConstruction_Type)
     return GNATCOM.Types.Pointer_To_IUnknown
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IUnknown;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_OLEDBCommand
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_OLEDBCommand;

   procedure Put_OLEDBCommand
     (This           : ADOCommandConstruction_Type;
      ppOLEDBCommand : GNATCOM.Types.Pointer_To_IUnknown)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_OLEDBCommand
         (Pointer (This),
          ppOLEDBCommand));

   end Put_OLEDBCommand;

end ADO.ADOCommandConstruction_Interface;
