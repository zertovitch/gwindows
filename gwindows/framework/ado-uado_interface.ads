with GNATCOM.Dispinterface;

package ADO.uADO_Interface is

   type uADO_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out uADO_Type);

   function Pointer (This : uADO_Type)
     return Pointer_To_uADO;

   procedure Attach (This    : in out uADO_Type;
                     Pointer : in     Pointer_To_uADO);

   function Get_Properties
     (This      : uADO_Type)
     return Pointer_To_Properties;

end ADO.uADO_Interface;
