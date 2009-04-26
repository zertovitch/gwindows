package body ADO.Error_Object is

   function Get_Number
     (This : Error_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Error_Get_Number);
   end Get_Number;

   function Get_Source
     (This : Error_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Error_Get_Source);
   end Get_Source;

   function Get_Description
     (This : Error_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Error_Get_Description);
   end Get_Description;

   function Get_HelpFile
     (This : Error_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Error_Get_HelpFile);
   end Get_HelpFile;

   function Get_HelpContext
     (This : Error_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Error_Get_HelpContext);
   end Get_HelpContext;

   function Get_SQLState
     (This : Error_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Error_Get_SQLState);
   end Get_SQLState;

   function Get_NativeError
     (This : Error_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, Error_Get_NativeError);
   end Get_NativeError;

end ADO.Error_Object;
