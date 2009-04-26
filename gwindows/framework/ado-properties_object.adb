package body ADO.Properties_Object is

   function Get_Count
     (This : Properties_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Properties_Get_Count);
   end Get_Count;

   function uNewEnum
     (This : Properties_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, Properties_uNewEnum);
   end uNewEnum;

   procedure Refresh
     (This : Properties_Type)
   is
   begin
      Invoke (This, Properties_Refresh);
   end Refresh;

   function Get_Item
     (This  : Properties_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get
        (This,
         Properties_Get_Item,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Index),
         Free);
   end Get_Item;

end ADO.Properties_Object;
