with GNATCOM.Errors;

package body ADO.RecordsetEventsVt_Interface is

   procedure Initialize (This : in out RecordsetEventsVt_Type) is
   begin
      Set_IID (This, IID_RecordsetEventsVt);
   end Initialize;

   function Pointer (This : RecordsetEventsVt_Type)
     return Pointer_To_RecordsetEventsVt
   is
   begin
      return To_Pointer_To_RecordsetEventsVt (Address (This));
   end Pointer;

   procedure Attach (This    : in out RecordsetEventsVt_Type;
                     Pointer : in     Pointer_To_RecordsetEventsVt)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure WillChangeField
     (This       : RecordsetEventsVt_Type;
      cFields    : Interfaces.C.long;
      Fields     : GNATCOM.Types.VARIANT;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset;
      Free       : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.WillChangeField
         (Pointer (This),
          cFields,
          Fields,
          adStatus,
          pRecordset));

      if Free then
               GNATCOM.Iinterface.Free (Fields);

      end if;

   end WillChangeField;

   procedure FieldChangeComplete
     (This       : RecordsetEventsVt_Type;
      cFields    : Interfaces.C.long;
      Fields     : GNATCOM.Types.VARIANT;
      pError     : Pointer_To_Error;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset;
      Free       : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.FieldChangeComplete
         (Pointer (This),
          cFields,
          Fields,
          pError,
          adStatus,
          pRecordset));

      if Free then
               GNATCOM.Iinterface.Free (Fields);

      end if;

   end FieldChangeComplete;

   procedure WillChangeRecord
     (This       : RecordsetEventsVt_Type;
      adReason   : EventReasonEnum;
      cRecords   : Interfaces.C.long;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.WillChangeRecord
         (Pointer (This),
          adReason,
          cRecords,
          adStatus,
          pRecordset));

   end WillChangeRecord;

   procedure RecordChangeComplete
     (This       : RecordsetEventsVt_Type;
      adReason   : EventReasonEnum;
      cRecords   : Interfaces.C.long;
      pError     : Pointer_To_Error;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RecordChangeComplete
         (Pointer (This),
          adReason,
          cRecords,
          pError,
          adStatus,
          pRecordset));

   end RecordChangeComplete;

   procedure WillChangeRecordset
     (This       : RecordsetEventsVt_Type;
      adReason   : EventReasonEnum;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.WillChangeRecordset
         (Pointer (This),
          adReason,
          adStatus,
          pRecordset));

   end WillChangeRecordset;

   procedure RecordsetChangeComplete
     (This       : RecordsetEventsVt_Type;
      adReason   : EventReasonEnum;
      pError     : Pointer_To_Error;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RecordsetChangeComplete
         (Pointer (This),
          adReason,
          pError,
          adStatus,
          pRecordset));

   end RecordsetChangeComplete;

   procedure WillMove
     (This       : RecordsetEventsVt_Type;
      adReason   : EventReasonEnum;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.WillMove
         (Pointer (This),
          adReason,
          adStatus,
          pRecordset));

   end WillMove;

   procedure MoveComplete
     (This       : RecordsetEventsVt_Type;
      adReason   : EventReasonEnum;
      pError     : Pointer_To_Error;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveComplete
         (Pointer (This),
          adReason,
          pError,
          adStatus,
          pRecordset));

   end MoveComplete;

   procedure EndOfRecordset
     (This       : RecordsetEventsVt_Type;
      fMoreData  : GNATCOM.Types.Pointer_To_VARIANT_BOOL;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.EndOfRecordset
         (Pointer (This),
          fMoreData,
          adStatus,
          pRecordset));

   end EndOfRecordset;

   procedure FetchProgress
     (This        : RecordsetEventsVt_Type;
      Progress    : Interfaces.C.long;
      MaxProgress : Interfaces.C.long;
      adStatus    : Pointer_To_EventStatusEnum;
      pRecordset  : Pointer_To_uRecordset)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.FetchProgress
         (Pointer (This),
          Progress,
          MaxProgress,
          adStatus,
          pRecordset));

   end FetchProgress;

   procedure FetchComplete
     (This       : RecordsetEventsVt_Type;
      pError     : Pointer_To_Error;
      adStatus   : Pointer_To_EventStatusEnum;
      pRecordset : Pointer_To_uRecordset)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.FetchComplete
         (Pointer (This),
          pError,
          adStatus,
          pRecordset));

   end FetchComplete;

end ADO.RecordsetEventsVt_Interface;
