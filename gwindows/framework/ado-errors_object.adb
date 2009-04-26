package body ADO.Errors_Object is

   function Get_Count
     (This : Errors_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Errors_Get_Count);
   end Get_Count;

   function uNewEnum
     (This : Errors_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, Errors_uNewEnum);
   end uNewEnum;

   procedure Refresh
     (This : Errors_Type)
   is
   begin
      Invoke (This, Errors_Refresh);
   end Refresh;

   function Get_Item
     (This  : Errors_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get
        (This,
         Errors_Get_Item,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Index),
         Free);
   end Get_Item;

   procedure Clear
     (This : Errors_Type)
   is
   begin
      Invoke (This, Errors_Clear);
   end Clear;

end ADO.Errors_Object;
