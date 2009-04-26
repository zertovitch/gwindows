with GNATCOM.Dispinterface;

package ADO.uADO_Object is

   type uADO_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Properties
     (This : uADO_Type)
     return GNATCOM.Types.VARIANT;

end ADO.uADO_Object;
