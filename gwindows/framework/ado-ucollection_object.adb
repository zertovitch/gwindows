package body ADO.uCollection_Object is

   function Get_Count
     (This : uCollection_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uCollection_Get_Count);
   end Get_Count;

   function uNewEnum
     (This : uCollection_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, uCollection_uNewEnum);
   end uNewEnum;

   procedure Refresh
     (This : uCollection_Type)
   is
   begin
      Invoke (This, uCollection_Refresh);
   end Refresh;

end ADO.uCollection_Object;
