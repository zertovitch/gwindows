package body ADO.Parameters_Object is

   function Get_Count
     (This : Parameters_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Parameters_Get_Count);
   end Get_Count;

   function uNewEnum
     (This : Parameters_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, Parameters_uNewEnum);
   end uNewEnum;

   procedure Refresh
     (This : Parameters_Type)
   is
   begin
      Invoke (This, Parameters_Refresh);
   end Refresh;

   procedure Append
     (This   : Parameters_Type;
      Object : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
   is
   begin
      Invoke
        (This,
         Parameters_Append,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Object),
         Free);
   end Append;

   procedure Delete
     (This  : Parameters_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         Parameters_Delete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Index),
         Free);
   end Delete;

   function Get_Item
     (This  : Parameters_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get
        (This,
         Parameters_Get_Item,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Index),
         Free);
   end Get_Item;

end ADO.Parameters_Object;
