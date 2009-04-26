package body ADO.ConnectionEvents_Object is

   function InfoMessage
     (This        : ConnectionEvents_Type;
      pError      : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT;
      Free        : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ConnectionEvents_InfoMessage,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pConnection,
          2 => adStatus,
          3 => pError),
         Free);
   end InfoMessage;

   function BeginTransComplete
     (This             : ConnectionEvents_Type;
      TransactionLevel : GNATCOM.Types.VARIANT;
      pError           : GNATCOM.Types.VARIANT;
      adStatus         : GNATCOM.Types.VARIANT;
      pConnection      : GNATCOM.Types.VARIANT;
      Free             : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ConnectionEvents_BeginTransComplete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pConnection,
          2 => adStatus,
          3 => pError,
          4 => TransactionLevel),
         Free);
   end BeginTransComplete;

   function CommitTransComplete
     (This        : ConnectionEvents_Type;
      pError      : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT;
      Free        : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ConnectionEvents_CommitTransComplete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pConnection,
          2 => adStatus,
          3 => pError),
         Free);
   end CommitTransComplete;

   function RollbackTransComplete
     (This        : ConnectionEvents_Type;
      pError      : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT;
      Free        : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ConnectionEvents_RollbackTransComplete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pConnection,
          2 => adStatus,
          3 => pError),
         Free);
   end RollbackTransComplete;

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
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ConnectionEvents_WillExecute,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pConnection,
          2 => pRecordset,
          3 => pCommand,
          4 => adStatus,
          5 => Options,
          6 => LockType,
          7 => CursorType,
          8 => Source),
         Free);
   end WillExecute;

   function ExecuteComplete
     (This            : ConnectionEvents_Type;
      RecordsAffected : GNATCOM.Types.VARIANT;
      pError          : GNATCOM.Types.VARIANT;
      adStatus        : GNATCOM.Types.VARIANT;
      pCommand        : GNATCOM.Types.VARIANT;
      pRecordset      : GNATCOM.Types.VARIANT;
      pConnection     : GNATCOM.Types.VARIANT;
      Free            : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ConnectionEvents_ExecuteComplete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pConnection,
          2 => pRecordset,
          3 => pCommand,
          4 => adStatus,
          5 => pError,
          6 => RecordsAffected),
         Free);
   end ExecuteComplete;

   function WillConnect
     (This             : ConnectionEvents_Type;
      ConnectionString : GNATCOM.Types.VARIANT;
      UserID           : GNATCOM.Types.VARIANT;
      Password         : GNATCOM.Types.VARIANT;
      Options          : GNATCOM.Types.VARIANT;
      adStatus         : GNATCOM.Types.VARIANT;
      pConnection      : GNATCOM.Types.VARIANT;
      Free             : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ConnectionEvents_WillConnect,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pConnection,
          2 => adStatus,
          3 => Options,
          4 => Password,
          5 => UserID,
          6 => ConnectionString),
         Free);
   end WillConnect;

   function ConnectComplete
     (This        : ConnectionEvents_Type;
      pError      : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT;
      Free        : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ConnectionEvents_ConnectComplete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pConnection,
          2 => adStatus,
          3 => pError),
         Free);
   end ConnectComplete;

   function Disconnect
     (This        : ConnectionEvents_Type;
      adStatus    : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT;
      Free        : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         ConnectionEvents_Disconnect,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pConnection,
          2 => adStatus),
         Free);
   end Disconnect;

end ADO.ConnectionEvents_Object;
