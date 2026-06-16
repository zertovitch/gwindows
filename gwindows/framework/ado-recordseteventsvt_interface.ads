with GNATCOM.Iinterface;
with Win32_Types;

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
      cFields    : Win32_Types.Long;
      Fields     : GNATCOM.Types.VARIANT;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset;
      Free       : Boolean := True);

   procedure FieldChangeComplete
     (This       : RecordsetEventsVt_Type;
      cFields    : Win32_Types.Long;
      Fields     : GNATCOM.Types.VARIANT;
      pError     : Pointer_To_Error;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset;
      Free       : Boolean := True);

   procedure WillChangeRecord
     (This       : RecordsetEventsVt_Type;
      adReason   : EventReasonEnum;
      cRecords   : Win32_Types.Long;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset);

   procedure RecordChangeComplete
     (This       : RecordsetEventsVt_Type;
      adReason   : EventReasonEnum;
      cRecords   : Win32_Types.Long;
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
      Progress    : Win32_Types.Long;
      MaxProgress : Win32_Types.Long;
      adStatus    : Pointer_To_EventStatusEnum;
      pRecordset  : Pointer_To_uRecordset);

   procedure FetchComplete
     (This       : RecordsetEventsVt_Type;
      pError     : Pointer_To_Error;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset);

end ADO.RecordsetEventsVt_Interface;
