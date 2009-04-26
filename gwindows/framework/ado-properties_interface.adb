with GNATCOM.Iinterface;

with GNATCOM.Errors;

package body ADO.Properties_Interface is

   procedure Initialize (This : in out Properties_Type) is
   begin
      Set_IID (This, IID_Properties);
   end Initialize;

   function Pointer (This : Properties_Type)
     return Pointer_To_Properties
   is
   begin
      return To_Pointer_To_Properties (Address (This));
   end Pointer;

   procedure Attach (This    : in out Properties_Type;
                     Pointer : in     Pointer_To_Properties)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_Count
     (This : Properties_Type)
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
     (This      : Properties_Type)
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
     (This : Properties_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Refresh
         (Pointer (This)));

   end Refresh;

   function Get_Item
     (This      : Properties_Type;
      Index     : GNATCOM.Types.VARIANT;
      Free      : Boolean := True)
     return Pointer_To_Property
   is
       RetVal : aliased Pointer_To_Property;
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

end ADO.Properties_Interface;
