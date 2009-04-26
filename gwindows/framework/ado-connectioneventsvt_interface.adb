with GNATCOM.Errors;

package body ADO.ConnectionEventsVt_Interface is

   procedure Initialize (This : in out ConnectionEventsVt_Type) is
   begin
      Set_IID (This, IID_ConnectionEventsVt);
   end Initialize;

   function Pointer (This : ConnectionEventsVt_Type)
     return Pointer_To_ConnectionEventsVt
   is
   begin
      return To_Pointer_To_ConnectionEventsVt (Address (This));
   end Pointer;

   procedure Attach (This    : in out ConnectionEventsVt_Type;
                     Pointer : in     Pointer_To_ConnectionEventsVt)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure InfoMessage
     (This        : ConnectionEventsVt_Type;
      pError      : Pointer_To_Error;
      adStatus    : Pointer_To_EventStatusEnum;
      pConnection : Pointer_To_uConnection)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.InfoMessage
         (Pointer (This),
          pError,
          adStatus,
          pConnection));

   end InfoMessage;

   procedure BeginTransComplete
     (This             : ConnectionEventsVt_Type;
      TransactionLevel : Interfaces.C.long;
      pError           : Pointer_To_Error;
      adStatus         : Pointer_To_EventStatusEnum;
      pConnection      : Pointer_To_uConnection)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.BeginTransComplete
         (Pointer (This),
          TransactionLevel,
          pError,
          adStatus,
          pConnection));

   end BeginTransComplete;

   procedure CommitTransComplete
     (This        : ConnectionEventsVt_Type;
      pError      : Pointer_To_Error;
      adStatus    : Pointer_To_EventStatusEnum;
      pConnection : Pointer_To_uConnection)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.CommitTransComplete
         (Pointer (This),
          pError,
          adStatus,
          pConnection));

   end CommitTransComplete;

   procedure RollbackTransComplete
     (This        : ConnectionEventsVt_Type;
      pError      : Pointer_To_Error;
      adStatus    : Pointer_To_EventStatusEnum;
      pConnection : Pointer_To_uConnection)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RollbackTransComplete
         (Pointer (This),
          pError,
          adStatus,
          pConnection));

   end RollbackTransComplete;

   procedure WillExecute
     (This        : ConnectionEventsVt_Type;
      Source      : GNATCOM.Types.Pointer_To_BSTR;
      CursorType  : Pointer_To_CursorTypeEnum;
      LockType    : Pointer_To_LockTypeEnum;
      Options     : GNATCOM.Types.Pointer_To_long;
      adStatus    : Pointer_To_EventStatusEnum;
      pCommand    : Pointer_To_uCommand;
      pRecordset  : Pointer_To_uRecordset;
      pConnection : Pointer_To_uConnection)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.WillExecute
         (Pointer (This),
          Source,
          CursorType,
          LockType,
          Options,
          adStatus,
          pCommand,
          pRecordset,
          pConnection));

   end WillExecute;

   procedure ExecuteComplete
     (This            : ConnectionEventsVt_Type;
      RecordsAffected : Interfaces.C.long;
      pError          : Pointer_To_Error;
      adStatus        : Pointer_To_EventStatusEnum;
      pCommand        : Pointer_To_uCommand;
      pRecordset      : Pointer_To_uRecordset;
      pConnection     : Pointer_To_uConnection)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ExecuteComplete
         (Pointer (This),
          RecordsAffected,
          pError,
          adStatus,
          pCommand,
          pRecordset,
          pConnection));

   end ExecuteComplete;

   procedure WillConnect
     (This             : ConnectionEventsVt_Type;
      ConnectionString : GNATCOM.Types.Pointer_To_BSTR;
      UserID           : GNATCOM.Types.Pointer_To_BSTR;
      Password         : GNATCOM.Types.Pointer_To_BSTR;
      Options          : GNATCOM.Types.Pointer_To_long;
      adStatus         : Pointer_To_EventStatusEnum;
      pConnection      : Pointer_To_uConnection)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.WillConnect
         (Pointer (This),
          ConnectionString,
          UserID,
          Password,
          Options,
          adStatus,
          pConnection));

   end WillConnect;

   procedure ConnectComplete
     (This        : ConnectionEventsVt_Type;
      pError      : Pointer_To_Error;
      adStatus    : Pointer_To_EventStatusEnum;
      pConnection : Pointer_To_uConnection)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ConnectComplete
         (Pointer (This),
          pError,
          adStatus,
          pConnection));

   end ConnectComplete;

   procedure Disconnect
     (This        : ConnectionEventsVt_Type;
      adStatus    : Pointer_To_EventStatusEnum;
      pConnection : Pointer_To_uConnection)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Disconnect
         (Pointer (This),
          adStatus,
          pConnection));

   end Disconnect;

end ADO.ConnectionEventsVt_Interface;
