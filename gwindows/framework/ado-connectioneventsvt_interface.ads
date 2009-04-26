with GNATCOM.Iinterface;

package ADO.ConnectionEventsVt_Interface is

   type ConnectionEventsVt_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out ConnectionEventsVt_Type);

   function Pointer (This : ConnectionEventsVt_Type)
     return Pointer_To_ConnectionEventsVt;

   procedure Attach (This    : in out ConnectionEventsVt_Type;
                     Pointer : in     Pointer_To_ConnectionEventsVt);

   procedure InfoMessage
     (This        : ConnectionEventsVt_Type;
      pError      : Pointer_To_Error;
      adStatus    : Pointer_To_EventStatusEnum;
      pConnection : Pointer_To_uConnection);

   procedure BeginTransComplete
     (This             : ConnectionEventsVt_Type;
      TransactionLevel : Interfaces.C.long;
      pError           : Pointer_To_Error;
      adStatus         : Pointer_To_EventStatusEnum;
      pConnection      : Pointer_To_uConnection);

   procedure CommitTransComplete
     (This        : ConnectionEventsVt_Type;
      pError      : Pointer_To_Error;
      adStatus    : Pointer_To_EventStatusEnum;
      pConnection : Pointer_To_uConnection);

   procedure RollbackTransComplete
     (This        : ConnectionEventsVt_Type;
      pError      : Pointer_To_Error;
      adStatus    : Pointer_To_EventStatusEnum;
      pConnection : Pointer_To_uConnection);

   procedure WillExecute
     (This        : ConnectionEventsVt_Type;
      Source      : GNATCOM.Types.Pointer_To_BSTR;
      CursorType  : Pointer_To_CursorTypeEnum;
      LockType    : Pointer_To_LockTypeEnum;
      Options     : GNATCOM.Types.Pointer_To_long;
      adStatus    : Pointer_To_EventStatusEnum;
      pCommand    : Pointer_To_uCommand;
      pRecordset  : Pointer_To_uRecordset;
      pConnection : Pointer_To_uConnection);

   procedure ExecuteComplete
     (This            : ConnectionEventsVt_Type;
      RecordsAffected : Interfaces.C.long;
      pError          : Pointer_To_Error;
      adStatus        : Pointer_To_EventStatusEnum;
      pCommand        : Pointer_To_uCommand;
      pRecordset      : Pointer_To_uRecordset;
      pConnection     : Pointer_To_uConnection);

   procedure WillConnect
     (This             : ConnectionEventsVt_Type;
      ConnectionString : GNATCOM.Types.Pointer_To_BSTR;
      UserID           : GNATCOM.Types.Pointer_To_BSTR;
      Password         : GNATCOM.Types.Pointer_To_BSTR;
      Options          : GNATCOM.Types.Pointer_To_long;
      adStatus         : Pointer_To_EventStatusEnum;
      pConnection      : Pointer_To_uConnection);

   procedure ConnectComplete
     (This        : ConnectionEventsVt_Type;
      pError      : Pointer_To_Error;
      adStatus    : Pointer_To_EventStatusEnum;
      pConnection : Pointer_To_uConnection);

   procedure Disconnect
     (This        : ConnectionEventsVt_Type;
      adStatus    : Pointer_To_EventStatusEnum;
      pConnection : Pointer_To_uConnection);

end ADO.ConnectionEventsVt_Interface;
