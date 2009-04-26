package body ADO.uADO_Object is

   function Get_Properties
     (This : uADO_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uADO_Get_Properties);
   end Get_Properties;

end ADO.uADO_Object;
