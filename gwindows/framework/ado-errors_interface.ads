with GNATCOM.Dispinterface;

package ADO.Errors_Interface is

   type Errors_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out Errors_Type);

   function Pointer (This : Errors_Type)
     return Pointer_To_Errors;

   procedure Attach (This    : in out Errors_Type;
                     Pointer : in     Pointer_To_Errors);

   function Get_Count
     (This : Errors_Type)
     return Interfaces.C.long;

   function uNewEnum
     (This      : Errors_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;

   procedure Refresh
     (This : Errors_Type);

   function Get_Item
     (This      : Errors_Type;
      Index     : GNATCOM.Types.VARIANT;
      Free      : Boolean := True)
     return Pointer_To_Error;

   procedure Clear
     (This : Errors_Type);

end ADO.Errors_Interface;
