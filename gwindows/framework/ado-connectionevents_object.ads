with GNATCOM.Dispinterface;

package ADO.ConnectionEvents_Object is

   type ConnectionEvents_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function InfoMessage
     (This        : ConnectionEvents_Type;
      pError      : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT;
      Free        : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function BeginTransComplete
     (This             : ConnectionEvents_Type;
      TransactionLevel : GNATCOM.Types.VARIANT;
      pError           : GNATCOM.Types.VARIANT;
      adStatus         : GNATCOM.Types.VARIANT;
      pConnection      : GNATCOM.Types.VARIANT;
      Free             : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function CommitTransComplete
     (This        : ConnectionEvents_Type;
      pError      : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT;
      Free        : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function RollbackTransComplete
     (This        : ConnectionEvents_Type;
      pError      : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT;
      Free        : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function WillExecute
     (This        : ConnectionEvents_Type;
      Source      : GNATCOM.Types.VARIANT;
      CursorType  : GNATCOM.Types.VARIANT;
      LockType    : GNATCOM.Types.VARIANT;
      Options     : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pCommand    : GNATCOM.Types.VARIANT;
      pRecordset  : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT;
      Free        : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function ExecuteComplete
     (This            : ConnectionEvents_Type;
      RecordsAffected : GNATCOM.Types.VARIANT;
      pError          : GNATCOM.Types.VARIANT;
      adStatus        : GNATCOM.Types.VARIANT;
      pCommand        : GNATCOM.Types.VARIANT;
      pRecordset      : GNATCOM.Types.VARIANT;
      pConnection     : GNATCOM.Types.VARIANT;
      Free            : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function WillConnect
     (This             : ConnectionEvents_Type;
      ConnectionString : GNATCOM.Types.VARIANT;
      UserID           : GNATCOM.Types.VARIANT;
      Password         : GNATCOM.Types.VARIANT;
      Options          : GNATCOM.Types.VARIANT;
      adStatus         : GNATCOM.Types.VARIANT;
      pConnection      : GNATCOM.Types.VARIANT;
      Free             : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function ConnectComplete
     (This        : ConnectionEvents_Type;
      pError      : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT;
      Free        : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function Disconnect
     (This        : ConnectionEvents_Type;
      adStatus    : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT;
      Free        : Boolean := True)
     return GNATCOM.Types.VARIANT;

end ADO.ConnectionEvents_Object;
