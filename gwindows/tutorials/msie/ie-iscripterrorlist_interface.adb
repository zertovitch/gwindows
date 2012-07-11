with GNATCOM.IInterface;

with GNATCOM.Errors;

package body IE.IScriptErrorList_Interface is

   procedure Initialize (This : in out IScriptErrorList_Type) is
   begin
      Set_IID (This, IID_IScriptErrorList);
   end Initialize;

   function Pointer (This : IScriptErrorList_Type)
     return Pointer_To_IScriptErrorList
   is
   begin
      return To_Pointer_To_IScriptErrorList (Address (This));
   end Pointer;

   procedure Attach (This    : in out IScriptErrorList_Type;
                     Pointer : in     Pointer_To_IScriptErrorList)
   is
   begin
      Attach (This, GNATCOM.IInterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure advanceError
     (This : IScriptErrorList_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.advanceError
         (Pointer (This)));

   end advanceError;

   procedure retreatError
     (This : IScriptErrorList_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.retreatError
         (Pointer (This)));

   end retreatError;

   function canAdvanceError
     (This         : IScriptErrorList_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.canAdvanceError
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end canAdvanceError;

   function canRetreatError
     (This         : IScriptErrorList_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.canRetreatError
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end canRetreatError;

   function getErrorLine
     (This   : IScriptErrorList_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.getErrorLine
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end getErrorLine;

   function getErrorChar
     (This   : IScriptErrorList_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.getErrorChar
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end getErrorChar;

   function getErrorCode
     (This   : IScriptErrorList_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.getErrorCode
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end getErrorCode;

   function getErrorMsg
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.getErrorMsg
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end getErrorMsg;

   function getErrorUrl
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.getErrorUrl
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end getErrorUrl;

   function getAlwaysShowLockState
     (This               : IScriptErrorList_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.getAlwaysShowLockState
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end getAlwaysShowLockState;

   function getDetailsPaneOpen
     (This              : IScriptErrorList_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.getDetailsPaneOpen
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end getDetailsPaneOpen;

   procedure setDetailsPaneOpen
     (This             : IScriptErrorList_Type;
      fDetailsPaneOpen : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.setDetailsPaneOpen
         (Pointer (This),
          fDetailsPaneOpen));

   end setDetailsPaneOpen;

   function getPerErrorDisplay
     (This              : IScriptErrorList_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.getPerErrorDisplay
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end getPerErrorDisplay;

   procedure setPerErrorDisplay
     (This             : IScriptErrorList_Type;
      fPerErrorDisplay : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.setPerErrorDisplay
         (Pointer (This),
          fPerErrorDisplay));

   end setPerErrorDisplay;

end IE.IScriptErrorList_Interface;

