with GNATCOM.Iinterface;

with GNATCOM.Errors;

package body ADO.uADO_Interface is

   procedure Initialize (This : in out uADO_Type) is
   begin
      Set_IID (This, IID_uADO);
   end Initialize;

   function Pointer (This : uADO_Type)
     return Pointer_To_uADO
   is
   begin
      return To_Pointer_To_uADO (Address (This));
   end Pointer;

   procedure Attach (This    : in out uADO_Type;
                     Pointer : in     Pointer_To_uADO)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_Properties
     (This      : uADO_Type)
     return Pointer_To_Properties
   is
       RetVal : aliased Pointer_To_Properties;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Properties
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Properties;

end ADO.uADO_Interface;
