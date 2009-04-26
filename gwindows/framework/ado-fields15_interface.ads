with GNATCOM.Dispinterface;

package ADO.Fields15_Interface is

   type Fields15_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out Fields15_Type);

   function Pointer (This : Fields15_Type)
     return Pointer_To_Fields15;

   procedure Attach (This    : in out Fields15_Type;
                     Pointer : in     Pointer_To_Fields15);

   function Get_Count
     (This : Fields15_Type)
     return Interfaces.C.long;

   function uNewEnum
     (This      : Fields15_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;

   procedure Refresh
     (This : Fields15_Type);

   function Get_Item
     (This      : Fields15_Type;
      Index     : GNATCOM.Types.VARIANT;
      Free      : Boolean := True)
     return Pointer_To_Field;

end ADO.Fields15_Interface;
