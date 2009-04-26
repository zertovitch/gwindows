with GNATCOM.Dispinterface;

package ADO.uCollection_Interface is

   type uCollection_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out uCollection_Type);

   function Pointer (This : uCollection_Type)
     return Pointer_To_uCollection;

   procedure Attach (This    : in out uCollection_Type;
                     Pointer : in     Pointer_To_uCollection);

   function Get_Count
     (This : uCollection_Type)
     return Interfaces.C.long;

   function uNewEnum
     (This      : uCollection_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;

   procedure Refresh
     (This : uCollection_Type);

end ADO.uCollection_Interface;
