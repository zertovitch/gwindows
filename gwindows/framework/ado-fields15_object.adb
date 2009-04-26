package body ADO.Fields15_Object is

   function Get_Count
     (This : Fields15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Fields15_Get_Count);
   end Get_Count;

   function uNewEnum
     (This : Fields15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, Fields15_uNewEnum);
   end uNewEnum;

   procedure Refresh
     (This : Fields15_Type)
   is
   begin
      Invoke (This, Fields15_Refresh);
   end Refresh;

   function Get_Item
     (This  : Fields15_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get
        (This,
         Fields15_Get_Item,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Index),
         Free);
   end Get_Item;

end ADO.Fields15_Object;
