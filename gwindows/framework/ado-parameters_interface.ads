with GNATCOM.Dispinterface;

package ADO.Parameters_Interface is

   type Parameters_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out Parameters_Type);

   function Pointer (This : Parameters_Type)
     return Pointer_To_Parameters;

   procedure Attach (This    : in out Parameters_Type;
                     Pointer : in     Pointer_To_Parameters);

   function Get_Count
     (This : Parameters_Type)
     return Interfaces.C.long;

   function uNewEnum
     (This      : Parameters_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;

   procedure Refresh
     (This : Parameters_Type);

   procedure Append
     (This   : Parameters_Type;
      Object : GNATCOM.Types.Pointer_To_IDispatch);

   procedure Delete
     (This  : Parameters_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);

   function Get_Item
     (This      : Parameters_Type;
      Index     : GNATCOM.Types.VARIANT;
      Free      : Boolean := True)
     return Pointer_To_uParameter;

end ADO.Parameters_Interface;
