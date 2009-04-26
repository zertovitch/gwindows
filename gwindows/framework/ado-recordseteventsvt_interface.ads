with GNATCOM.Iinterface;

package ADO.RecordsetEventsVt_Interface is

   type RecordsetEventsVt_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out RecordsetEventsVt_Type);

   function Pointer (This : RecordsetEventsVt_Type)
     return Pointer_To_RecordsetEventsVt;

   procedure Attach (This    : in out RecordsetEventsVt_Type;
                     Pointer : in     Pointer_To_RecordsetEventsVt);

   procedure WillChangeField
     (This       : RecordsetEventsVt_Type;
      cFields    : Interfaces.C.long;
      Fields     : GNATCOM.Types.VARIANT;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset;
      Free       : Boolean := True);

   procedure FieldChangeComplete
     (This       : RecordsetEventsVt_Type;
      cFields    : Interfaces.C.long;
      Fields     : GNATCOM.Types.VARIANT;
      pError     : Pointer_To_Error;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset;
      Free       : Boolean := True);

   procedure WillChangeRecord
     (This       : RecordsetEventsVt_Type;
      adReason   : EventReasonEnum;
      cRecords   : Interfaces.C.long;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset);

   procedure RecordChangeComplete
     (This       : RecordsetEventsVt_Type;
      adReason   : EventReasonEnum;
      cRecords   : Interfaces.C.long;
      pError     : Pointer_To_Error;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset);

   procedure WillChangeRecordset
     (This       : RecordsetEventsVt_Type;
      adReason   : EventReasonEnum;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset);

   procedure RecordsetChangeComplete
     (This       : RecordsetEventsVt_Type;
      adReason   : EventReasonEnum;
      pError     : Pointer_To_Error;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset);

   procedure WillMove
     (This       : RecordsetEventsVt_Type;
      adReason   : EventReasonEnum;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset);

   procedure MoveComplete
     (This       : RecordsetEventsVt_Type;
      adReason   : EventReasonEnum;
      pError     : Pointer_To_Error;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset);

   procedure EndOfRecordset
     (This       : RecordsetEventsVt_Type;
      fMoreData  : GNATCOM.Types.Pointer_To_VARIANT_BOOL;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset);

   procedure FetchProgress
     (This        : RecordsetEventsVt_Type;
      Progress    : Interfaces.C.long;
      MaxProgress : Interfaces.C.long;
      adStatus    : Pointer_To_EventStatusEnum;
      pRecordset  : Pointer_To_uRecordset);

   procedure FetchComplete
     (This       : RecordsetEventsVt_Type;
      pError     : Pointer_To_Error;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset);

end ADO.RecordsetEventsVt_Interface;
