with GNATCOM.Dispinterface;

package ADO.uDynaCollection_Interface is

   type uDynaCollection_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out uDynaCollection_Type);

   function Pointer (This : uDynaCollection_Type)
     return Pointer_To_uDynaCollection;

   procedure Attach (This    : in out uDynaCollection_Type;
                     Pointer : in     Pointer_To_uDynaCollection);

   function Get_Count
     (This : uDynaCollection_Type)
     return Interfaces.C.long;

   function uNewEnum
     (This      : uDynaCollection_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;

   procedure Refresh
     (This : uDynaCollection_Type);

   procedure Append
     (This   : uDynaCollection_Type;
      Object : GNATCOM.Types.Pointer_To_IDispatch);

   procedure Delete
     (This  : uDynaCollection_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);

end ADO.uDynaCollection_Interface;
