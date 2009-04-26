with GNATCOM.Iinterface;

with GNATCOM.Errors;

package body ADO.Fields15_Interface is

   procedure Initialize (This : in out Fields15_Type) is
   begin
      Set_IID (This, IID_Fields15);
   end Initialize;

   function Pointer (This : Fields15_Type)
     return Pointer_To_Fields15
   is
   begin
      return To_Pointer_To_Fields15 (Address (This));
   end Pointer;

   procedure Attach (This    : in out Fields15_Type;
                     Pointer : in     Pointer_To_Fields15)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_Count
     (This : Fields15_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Count
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Count;

   function uNewEnum
     (This      : Fields15_Type)
     return GNATCOM.Types.Pointer_To_IUnknown
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IUnknown;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.uNewEnum
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end uNewEnum;

   procedure Refresh
     (This : Fields15_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Refresh
         (Pointer (This)));

   end Refresh;

   function Get_Item
     (This      : Fields15_Type;
      Index     : GNATCOM.Types.VARIANT;
      Free      : Boolean := True)
     return Pointer_To_Field
   is
       RetVal : aliased Pointer_To_Field;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Item
         (Pointer (This),
          Index,
          RetVal'Unchecked_Access));

      if Free then
               GNATCOM.Iinterface.Free (Index);

      end if;

      return RetVal;
   end Get_Item;

end ADO.Fields15_Interface;
