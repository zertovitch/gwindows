with GNATCOM.Dispinterface;

package IE.IScriptErrorList_Object is

   type IScriptErrorList_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure advanceError
     (This : IScriptErrorList_Type);

   procedure retreatError
     (This : IScriptErrorList_Type);

   function canAdvanceError
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.VARIANT;

   function canRetreatError
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.VARIANT;

   function getErrorLine
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.VARIANT;

   function getErrorChar
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.VARIANT;

   function getErrorCode
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.VARIANT;

   function getErrorMsg
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.VARIANT;

   function getErrorUrl
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.VARIANT;

   function getAlwaysShowLockState
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.VARIANT;

   function getDetailsPaneOpen
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.VARIANT;

   procedure setDetailsPaneOpen
     (This             : IScriptErrorList_Type;
      fDetailsPaneOpen : GNATCOM.Types.VARIANT;
      Free             : Boolean := True);

   function getPerErrorDisplay
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.VARIANT;

   procedure setPerErrorDisplay
     (This             : IScriptErrorList_Type;
      fPerErrorDisplay : GNATCOM.Types.VARIANT;
      Free             : Boolean := True);

end IE.IScriptErrorList_Object;

