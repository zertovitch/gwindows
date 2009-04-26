with GNATCOM.Dispinterface;

package ADO.Properties_Interface is

   type Properties_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out Properties_Type);

   function Pointer (This : Properties_Type)
     return Pointer_To_Properties;

   procedure Attach (This    : in out Properties_Type;
                     Pointer : in     Pointer_To_Properties);

   function Get_Count
     (This : Properties_Type)
     return Interfaces.C.long;

   function uNewEnum
     (This      : Properties_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;

   procedure Refresh
     (This : Properties_Type);

   function Get_Item
     (This      : Properties_Type;
      Index     : GNATCOM.Types.VARIANT;
      Free      : Boolean := True)
     return Pointer_To_Property;

end ADO.Properties_Interface;
