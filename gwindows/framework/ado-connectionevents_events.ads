with GNATCOM.Events.Event_Object;
with GNATCOM.Create.COM_Interface;
with GNATCOM.Iinterface;

package ADO.ConnectionEvents_Events is

   type ConnectionEvents_Event is
     new GNATCOM.Events.Event_Object.Event_Type with null record;

   function Create (From : in GNATCOM.Events.Event_Object.Event_Pointer)
     return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type;

   procedure Set_Events
     (This            : in out GNATCOM.Events.IConnectionPoint_Type;
      For_Object      : in     GNATCOM.Iinterface.Interface_Type'Class;
      Event_Interface :
        access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      Free            : Boolean := True);

   procedure InfoMessage
     (This        : ConnectionEvents_Event;
      pError      : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT);

   procedure BeginTransComplete
     (This             : ConnectionEvents_Event;
      TransactionLevel : GNATCOM.Types.VARIANT;
      pError           : GNATCOM.Types.VARIANT;
      adStatus         : GNATCOM.Types.VARIANT;
      pConnection      : GNATCOM.Types.VARIANT);

   procedure CommitTransComplete
     (This        : ConnectionEvents_Event;
      pError      : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT);

   procedure RollbackTransComplete
     (This        : ConnectionEvents_Event;
      pError      : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT);

   procedure WillExecute
     (This        : ConnectionEvents_Event;
      Source      : GNATCOM.Types.VARIANT;
      CursorType  : GNATCOM.Types.VARIANT;
      LockType    : GNATCOM.Types.VARIANT;
      Options     : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pCommand    : GNATCOM.Types.VARIANT;
      pRecordset  : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT);

   procedure ExecuteComplete
     (This            : ConnectionEvents_Event;
      RecordsAffected : GNATCOM.Types.VARIANT;
      pError          : GNATCOM.Types.VARIANT;
      adStatus        : GNATCOM.Types.VARIANT;
      pCommand        : GNATCOM.Types.VARIANT;
      pRecordset      : GNATCOM.Types.VARIANT;
      pConnection     : GNATCOM.Types.VARIANT);

   procedure WillConnect
     (This             : ConnectionEvents_Event;
      ConnectionString : GNATCOM.Types.VARIANT;
      UserID           : GNATCOM.Types.VARIANT;
      Password         : GNATCOM.Types.VARIANT;
      Options          : GNATCOM.Types.VARIANT;
      adStatus         : GNATCOM.Types.VARIANT;
      pConnection      : GNATCOM.Types.VARIANT);

   procedure ConnectComplete
     (This        : ConnectionEvents_Event;
      pError      : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT);

   procedure Disconnect
     (This        : ConnectionEvents_Event;
      adStatus    : GNATCOM.Types.VARIANT;
      pConnection : GNATCOM.Types.VARIANT);

end ADO.ConnectionEvents_Events;
