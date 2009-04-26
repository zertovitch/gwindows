with GNATCOM.Dispinterface;

package ADO.Error_Object is

   type Error_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Number
     (This : Error_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Source
     (This : Error_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Description
     (This : Error_Type)
     return GNATCOM.Types.VARIANT;

   function Get_HelpFile
     (This : Error_Type)
     return GNATCOM.Types.VARIANT;

   function Get_HelpContext
     (This : Error_Type)
     return GNATCOM.Types.VARIANT;

   function Get_SQLState
     (This : Error_Type)
     return GNATCOM.Types.VARIANT;

   function Get_NativeError
     (This : Error_Type)
     return GNATCOM.Types.VARIANT;

end ADO.Error_Object;
