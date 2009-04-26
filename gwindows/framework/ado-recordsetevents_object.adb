package body ADO.RecordsetEvents_Object is

   function WillChangeField
     (This       : RecordsetEvents_Type;
      cFields    : GNATCOM.Types.VARIANT;
      Fields     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         RecordsetEvents_WillChangeField,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pRecordset,
          2 => adStatus,
          3 => Fields,
          4 => cFields),
         Free);
   end WillChangeField;

   function FieldChangeComplete
     (This       : RecordsetEvents_Type;
      cFields    : GNATCOM.Types.VARIANT;
      Fields     : GNATCOM.Types.VARIANT;
      pError     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         RecordsetEvents_FieldChangeComplete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pRecordset,
          2 => adStatus,
          3 => pError,
          4 => Fields,
          5 => cFields),
         Free);
   end FieldChangeComplete;

   function WillChangeRecord
     (This       : RecordsetEvents_Type;
      adReason   : GNATCOM.Types.VARIANT;
      cRecords   : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         RecordsetEvents_WillChangeRecord,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pRecordset,
          2 => adStatus,
          3 => cRecords,
          4 => adReason),
         Free);
   end WillChangeRecord;

   function RecordChangeComplete
     (This       : RecordsetEvents_Type;
      adReason   : GNATCOM.Types.VARIANT;
      cRecords   : GNATCOM.Types.VARIANT;
      pError     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         RecordsetEvents_RecordChangeComplete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pRecordset,
          2 => adStatus,
          3 => pError,
          4 => cRecords,
          5 => adReason),
         Free);
   end RecordChangeComplete;

   function WillChangeRecordset
     (This       : RecordsetEvents_Type;
      adReason   : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         RecordsetEvents_WillChangeRecordset,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pRecordset,
          2 => adStatus,
          3 => adReason),
         Free);
   end WillChangeRecordset;

   function RecordsetChangeComplete
     (This       : RecordsetEvents_Type;
      adReason   : GNATCOM.Types.VARIANT;
      pError     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         RecordsetEvents_RecordsetChangeComplete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pRecordset,
          2 => adStatus,
          3 => pError,
          4 => adReason),
         Free);
   end RecordsetChangeComplete;

   function WillMove
     (This       : RecordsetEvents_Type;
      adReason   : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         RecordsetEvents_WillMove,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pRecordset,
          2 => adStatus,
          3 => adReason),
         Free);
   end WillMove;

   function MoveComplete
     (This       : RecordsetEvents_Type;
      adReason   : GNATCOM.Types.VARIANT;
      pError     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         RecordsetEvents_MoveComplete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pRecordset,
          2 => adStatus,
          3 => pError,
          4 => adReason),
         Free);
   end MoveComplete;

   function EndOfRecordset
     (This       : RecordsetEvents_Type;
      fMoreData  : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         RecordsetEvents_EndOfRecordset,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pRecordset,
          2 => adStatus,
          3 => fMoreData),
         Free);
   end EndOfRecordset;

   function FetchProgress
     (This        : RecordsetEvents_Type;
      Progress    : GNATCOM.Types.VARIANT;
      MaxProgress : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pRecordset  : GNATCOM.Types.VARIANT;
      Free        : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         RecordsetEvents_FetchProgress,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pRecordset,
          2 => adStatus,
          3 => MaxProgress,
          4 => Progress),
         Free);
   end FetchProgress;

   function FetchComplete
     (This       : RecordsetEvents_Type;
      pError     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         RecordsetEvents_FetchComplete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pRecordset,
          2 => adStatus,
          3 => pError),
         Free);
   end FetchComplete;

end ADO.RecordsetEvents_Object;
