with GNATCOM.Events.Event_Object;
with GNATCOM.Create.COM_Interface;
with GNATCOM.Iinterface;

package ADO.RecordsetEvents_Events is

   type RecordsetEvents_Event is
     new GNATCOM.Events.Event_Object.Event_Type with null record;

   function Create (From : in GNATCOM.Events.Event_Object.Event_Pointer)
     return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type;

   procedure Set_Events
     (This            : in out GNATCOM.Events.IConnectionPoint_Type;
      For_Object      : in     GNATCOM.Iinterface.Interface_Type'Class;
      Event_Interface :
        access GNATCOM.Create.COM_Interface.COM_Interface_Type;
      Free            : Boolean := True);

   procedure WillChangeField
     (This       : RecordsetEvents_Event;
      cFields    : GNATCOM.Types.VARIANT;
      Fields     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT);

   procedure FieldChangeComplete
     (This       : RecordsetEvents_Event;
      cFields    : GNATCOM.Types.VARIANT;
      Fields     : GNATCOM.Types.VARIANT;
      pError     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT);

   procedure WillChangeRecord
     (This       : RecordsetEvents_Event;
      adReason   : GNATCOM.Types.VARIANT;
      cRecords   : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT);

   procedure RecordChangeComplete
     (This       : RecordsetEvents_Event;
      adReason   : GNATCOM.Types.VARIANT;
      cRecords   : GNATCOM.Types.VARIANT;
      pError     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT);

   procedure WillChangeRecordset
     (This       : RecordsetEvents_Event;
      adReason   : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT);

   procedure RecordsetChangeComplete
     (This       : RecordsetEvents_Event;
      adReason   : GNATCOM.Types.VARIANT;
      pError     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT);

   procedure WillMove
     (This       : RecordsetEvents_Event;
      adReason   : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT);

   procedure MoveComplete
     (This       : RecordsetEvents_Event;
      adReason   : GNATCOM.Types.VARIANT;
      pError     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT);

   procedure EndOfRecordset
     (This       : RecordsetEvents_Event;
      fMoreData  : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT);

   procedure FetchProgress
     (This        : RecordsetEvents_Event;
      Progress    : GNATCOM.Types.VARIANT;
      MaxProgress : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pRecordset  : GNATCOM.Types.VARIANT);

   procedure FetchComplete
     (This       : RecordsetEvents_Event;
      pError     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT);

end ADO.RecordsetEvents_Events;
