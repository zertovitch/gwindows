with GNATCOM.Iinterface;

with GNATCOM.Errors;

package body ADO.Error_Interface is

   procedure Initialize (This : in out Error_Type) is
   begin
      Set_IID (This, IID_Error);
   end Initialize;

   function Pointer (This : Error_Type)
     return Pointer_To_Error
   is
   begin
      return To_Pointer_To_Error (Address (This));
   end Pointer;

   procedure Attach (This    : in out Error_Type;
                     Pointer : in     Pointer_To_Error)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_Number
     (This : Error_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Number
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Number;

   function Get_Source
     (This  : Error_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Source
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Source;

   function Get_Description
     (This  : Error_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Description
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Description;

   function Get_HelpFile
     (This  : Error_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_HelpFile
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_HelpFile;

   function Get_HelpContext
     (This : Error_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_HelpContext
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_HelpContext;

   function Get_SQLState
     (This  : Error_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_SQLState
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_SQLState;

   function Get_NativeError
     (This : Error_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_NativeError
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_NativeError;

end ADO.Error_Interface;
