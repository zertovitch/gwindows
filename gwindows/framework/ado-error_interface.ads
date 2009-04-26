with GNATCOM.Dispinterface;

package ADO.Error_Interface is

   type Error_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out Error_Type);

   function Pointer (This : Error_Type)
     return Pointer_To_Error;

   procedure Attach (This    : in out Error_Type;
                     Pointer : in     Pointer_To_Error);

   function Get_Number
     (This : Error_Type)
     return Interfaces.C.long;

   function Get_Source
     (This  : Error_Type)
     return GNATCOM.Types.BSTR;

   function Get_Description
     (This  : Error_Type)
     return GNATCOM.Types.BSTR;

   function Get_HelpFile
     (This  : Error_Type)
     return GNATCOM.Types.BSTR;

   function Get_HelpContext
     (This : Error_Type)
     return Interfaces.C.long;

   function Get_SQLState
     (This  : Error_Type)
     return GNATCOM.Types.BSTR;

   function Get_NativeError
     (This : Error_Type)
     return Interfaces.C.long;

end ADO.Error_Interface;
