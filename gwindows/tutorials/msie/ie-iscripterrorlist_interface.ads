with GNATCOM.Dispinterface;

package IE.IScriptErrorList_Interface is

   type IScriptErrorList_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out IScriptErrorList_Type);

   function Pointer (This : IScriptErrorList_Type)
     return Pointer_To_IScriptErrorList;

   procedure Attach (This    : in out IScriptErrorList_Type;
                     Pointer : in     Pointer_To_IScriptErrorList);

   procedure advanceError
     (This : IScriptErrorList_Type);

   procedure retreatError
     (This : IScriptErrorList_Type);

   function canAdvanceError
     (This         : IScriptErrorList_Type)
     return Interfaces.C.long;

   function canRetreatError
     (This         : IScriptErrorList_Type)
     return Interfaces.C.long;

   function getErrorLine
     (This   : IScriptErrorList_Type)
     return Interfaces.C.long;

   function getErrorChar
     (This   : IScriptErrorList_Type)
     return Interfaces.C.long;

   function getErrorCode
     (This   : IScriptErrorList_Type)
     return Interfaces.C.long;

   function getErrorMsg
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.BSTR;

   function getErrorUrl
     (This : IScriptErrorList_Type)
     return GNATCOM.Types.BSTR;

   function getAlwaysShowLockState
     (This               : IScriptErrorList_Type)
     return Interfaces.C.long;

   function getDetailsPaneOpen
     (This              : IScriptErrorList_Type)
     return Interfaces.C.long;

   procedure setDetailsPaneOpen
     (This             : IScriptErrorList_Type;
      fDetailsPaneOpen : Interfaces.C.long);

   function getPerErrorDisplay
     (This              : IScriptErrorList_Type)
     return Interfaces.C.long;

   procedure setPerErrorDisplay
     (This             : IScriptErrorList_Type;
      fPerErrorDisplay : Interfaces.C.long);

end IE.IScriptErrorList_Interface;

