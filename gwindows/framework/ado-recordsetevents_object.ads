with GNATCOM.Dispinterface;

package ADO.RecordsetEvents_Object is

   type RecordsetEvents_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function WillChangeField
     (This       : RecordsetEvents_Type;
      cFields    : GNATCOM.Types.VARIANT;
      Fields     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function FieldChangeComplete
     (This       : RecordsetEvents_Type;
      cFields    : GNATCOM.Types.VARIANT;
      Fields     : GNATCOM.Types.VARIANT;
      pError     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function WillChangeRecord
     (This       : RecordsetEvents_Type;
      adReason   : GNATCOM.Types.VARIANT;
      cRecords   : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function RecordChangeComplete
     (This       : RecordsetEvents_Type;
      adReason   : GNATCOM.Types.VARIANT;
      cRecords   : GNATCOM.Types.VARIANT;
      pError     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function WillChangeRecordset
     (This       : RecordsetEvents_Type;
      adReason   : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function RecordsetChangeComplete
     (This       : RecordsetEvents_Type;
      adReason   : GNATCOM.Types.VARIANT;
      pError     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function WillMove
     (This       : RecordsetEvents_Type;
      adReason   : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function MoveComplete
     (This       : RecordsetEvents_Type;
      adReason   : GNATCOM.Types.VARIANT;
      pError     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function EndOfRecordset
     (This       : RecordsetEvents_Type;
      fMoreData  : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function FetchProgress
     (This        : RecordsetEvents_Type;
      Progress    : GNATCOM.Types.VARIANT;
      MaxProgress : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pRecordset  : GNATCOM.Types.VARIANT;
      Free        : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function FetchComplete
     (This       : RecordsetEvents_Type;
      pError     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
     return GNATCOM.Types.VARIANT;

end ADO.RecordsetEvents_Object;
