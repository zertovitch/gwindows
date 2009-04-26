package body ADO.ConnectionEvents_Events is

   procedure Invoke
     (dispidMember : in Interfaces.C.long;
      wFlags       : in Interfaces.C.unsigned_short;
      pdispparams  : in GNATCOM.Types.Pointer_To_DISPPARAMS;
      Event_Object : in GNATCOM.Events.Event_Object.Event_Pointer)
   is
      use type Interfaces.C.long;
      pragma Unreferenced (wFlags);
   begin
      case dispidMember is
         when ConnectionEvents_InfoMessage =>
            InfoMessage
              (ConnectionEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when ConnectionEvents_BeginTransComplete =>
            BeginTransComplete
              (ConnectionEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (3),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when ConnectionEvents_CommitTransComplete =>
            CommitTransComplete
              (ConnectionEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when ConnectionEvents_RollbackTransComplete =>
            RollbackTransComplete
              (ConnectionEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when ConnectionEvents_WillExecute =>
            WillExecute
              (ConnectionEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (7),
               pdispparams.rgvarg (6),
               pdispparams.rgvarg (5),
               pdispparams.rgvarg (4),
               pdispparams.rgvarg (3),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when ConnectionEvents_ExecuteComplete =>
            ExecuteComplete
              (ConnectionEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (5),
               pdispparams.rgvarg (4),
               pdispparams.rgvarg (3),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when ConnectionEvents_WillConnect =>
            WillConnect
              (ConnectionEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (5),
               pdispparams.rgvarg (4),
               pdispparams.rgvarg (3),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when ConnectionEvents_ConnectComplete =>
            ConnectComplete
              (ConnectionEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when ConnectionEvents_Disconnect =>
            Disconnect
              (ConnectionEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when others =>
            null;
      end case;
   end Invoke;

   function Create (From : in GNATCOM.Events.Event_Object.Event_Pointer)
     return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type
   is
   begin
      return GNATCOM.Events.Event_Object.Create
        (Invoke'Access,
         IID_ConnectionEvents,
         From);
   end Create;

   procedure Set_Events
     (This            : in out GNATCOM.Events.IConnectionPoint_Type;
      For_Object      : in     GNATCOM.Iinterface.Interface_Type'Class;
      Event_Interface :
        access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      Free            : Boolean := True)
   is
   begin
      GNATCOM.Events.Set_Events
        (This,
         For_Object,
         IID_ConnectionEvents,
         Event_Interface,
         Free);
   end Set_Events;

   procedure InfoMessage
     (This        : ConnectionEvents_Event;
      pError      : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end InfoMessage;

   procedure BeginTransComplete
     (This             : ConnectionEvents_Event;
      TransactionLevel : GNATCOM.Types.VARIANT;
      pError           : GNATCOM.Types.VARIANT;
      adStatus         : GNATCOM.Types.VARIANT;
      pConnection      : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end BeginTransComplete;

   procedure CommitTransComplete
     (This        : ConnectionEvents_Event;
      pError      : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end CommitTransComplete;

   procedure RollbackTransComplete
     (This        : ConnectionEvents_Event;
      pError      : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end RollbackTransComplete;

   procedure WillExecute
     (This        : ConnectionEvents_Event;
      Source      : GNATCOM.Types.VARIANT;
      CursorType  : GNATCOM.Types.VARIANT;
      LockType    : GNATCOM.Types.VARIANT;
      Options     : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pCommand    : GNATCOM.Types.VARIANT;
      pRecordset  : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end WillExecute;

   procedure ExecuteComplete
     (This            : ConnectionEvents_Event;
      RecordsAffected : GNATCOM.Types.VARIANT;
      pError          : GNATCOM.Types.VARIANT;
      adStatus        : GNATCOM.Types.VARIANT;
      pCommand        : GNATCOM.Types.VARIANT;
      pRecordset      : GNATCOM.Types.VARIANT;
      pConnection     : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end ExecuteComplete;

   procedure WillConnect
     (This             : ConnectionEvents_Event;
      ConnectionString : GNATCOM.Types.VARIANT;
      UserID           : GNATCOM.Types.VARIANT;
      Password         : GNATCOM.Types.VARIANT;
      Options          : GNATCOM.Types.VARIANT;
      adStatus         : GNATCOM.Types.VARIANT;
      pConnection      : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end WillConnect;

   procedure ConnectComplete
     (This        : ConnectionEvents_Event;
      pError      : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end ConnectComplete;

   procedure Disconnect
     (This        : ConnectionEvents_Event;
      adStatus    : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end Disconnect;

end ADO.ConnectionEvents_Events;
