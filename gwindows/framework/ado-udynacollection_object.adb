package body ADO.uDynaCollection_Object is

   function Get_Count
     (This : uDynaCollection_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uDynaCollection_Get_Count);
   end Get_Count;

   function uNewEnum
     (This : uDynaCollection_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, uDynaCollection_uNewEnum);
   end uNewEnum;

   procedure Refresh
     (This : uDynaCollection_Type)
   is
   begin
      Invoke (This, uDynaCollection_Refresh);
   end Refresh;

   procedure Append
     (This   : uDynaCollection_Type;
      Object : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
   is
   begin
      Invoke
        (This,
         uDynaCollection_Append,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Object),
         Free);
   end Append;

   procedure Delete
     (This  : uDynaCollection_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         uDynaCollection_Delete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Index),
         Free);
   end Delete;

end ADO.uDynaCollection_Object;
