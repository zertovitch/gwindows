package body IE.IScriptErrorList_Object is

   procedure advanceError
     (This : IScriptErrorList_Type)
   is
   begin
      Invoke (This, IScriptErrorList_advanceError);
   end advanceError;

   procedure retreatError
     (This : IScriptErrorList_Type)
   is
   begin
      Invoke (This, IScriptErrorList_retreatError);
   end retreatError;

   function canAdvanceError
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IScriptErrorList_canAdvanceError);
   end canAdvanceError;

   function canRetreatError
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IScriptErrorList_canRetreatError);
   end canRetreatError;

   function getErrorLine
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IScriptErrorList_getErrorLine);
   end getErrorLine;

   function getErrorChar
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IScriptErrorList_getErrorChar);
   end getErrorChar;

   function getErrorCode
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IScriptErrorList_getErrorCode);
   end getErrorCode;

   function getErrorMsg
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IScriptErrorList_getErrorMsg);
   end getErrorMsg;

   function getErrorUrl
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IScriptErrorList_getErrorUrl);
   end getErrorUrl;

   function getAlwaysShowLockState
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IScriptErrorList_getAlwaysShowLockState);
   end getAlwaysShowLockState;

   function getDetailsPaneOpen
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IScriptErrorList_getDetailsPaneOpen);
   end getDetailsPaneOpen;

   procedure setDetailsPaneOpen
     (This             : IScriptErrorList_Type;
      fDetailsPaneOpen : GNATCOM.Types.VARIANT;
      Free             : Boolean := True)
   is
   begin
      Invoke
        (This,
         IScriptErrorList_setDetailsPaneOpen,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => fDetailsPaneOpen),
         Free);
   end setDetailsPaneOpen;

   function getPerErrorDisplay
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IScriptErrorList_getPerErrorDisplay);
   end getPerErrorDisplay;

   procedure setPerErrorDisplay
     (This             : IScriptErrorList_Type;
      fPerErrorDisplay : GNATCOM.Types.VARIANT;
      Free             : Boolean := True)
   is
   begin
      Invoke
        (This,
         IScriptErrorList_setPerErrorDisplay,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => fPerErrorDisplay),
         Free);
   end setPerErrorDisplay;

end IE.IScriptErrorList_Object;

