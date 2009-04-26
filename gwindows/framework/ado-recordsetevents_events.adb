package body ADO.RecordsetEvents_Events is

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
         when RecordsetEvents_WillChangeField =>
            WillChangeField
              (RecordsetEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (3),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when RecordsetEvents_FieldChangeComplete =>
            FieldChangeComplete
              (RecordsetEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (4),
               pdispparams.rgvarg (3),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when RecordsetEvents_WillChangeRecord =>
            WillChangeRecord
              (RecordsetEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (3),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when RecordsetEvents_RecordChangeComplete =>
            RecordChangeComplete
              (RecordsetEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (4),
               pdispparams.rgvarg (3),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when RecordsetEvents_WillChangeRecordset =>
            WillChangeRecordset
              (RecordsetEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when RecordsetEvents_RecordsetChangeComplete =>
            RecordsetChangeComplete
              (RecordsetEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (3),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when RecordsetEvents_WillMove =>
            WillMove
              (RecordsetEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when RecordsetEvents_MoveComplete =>
            MoveComplete
              (RecordsetEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (3),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when RecordsetEvents_EndOfRecordset =>
            EndOfRecordset
              (RecordsetEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when RecordsetEvents_FetchProgress =>
            FetchProgress
              (RecordsetEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (3),
               pdispparams.rgvarg (2),
               pdispparams.rgvarg (1),
               pdispparams.rgvarg (0));
         when RecordsetEvents_FetchComplete =>
            FetchComplete
              (RecordsetEvents_Event'Class (Event_Object.all),
               pdispparams.rgvarg (2),
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
         IID_RecordsetEvents,
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
         IID_RecordsetEvents,
         Event_Interface,
         Free);
   end Set_Events;

   procedure WillChangeField
     (This       : RecordsetEvents_Event;
      cFields    : GNATCOM.Types.VARIANT;
      Fields     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end WillChangeField;

   procedure FieldChangeComplete
     (This       : RecordsetEvents_Event;
      cFields    : GNATCOM.Types.VARIANT;
      Fields     : GNATCOM.Types.VARIANT;
      pError     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end FieldChangeComplete;

   procedure WillChangeRecord
     (This       : RecordsetEvents_Event;
      adReason   : GNATCOM.Types.VARIANT;
      cRecords   : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end WillChangeRecord;

   procedure RecordChangeComplete
     (This       : RecordsetEvents_Event;
      adReason   : GNATCOM.Types.VARIANT;
      cRecords   : GNATCOM.Types.VARIANT;
      pError     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end RecordChangeComplete;

   procedure WillChangeRecordset
     (This       : RecordsetEvents_Event;
      adReason   : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end WillChangeRecordset;

   procedure RecordsetChangeComplete
     (This       : RecordsetEvents_Event;
      adReason   : GNATCOM.Types.VARIANT;
      pError     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end RecordsetChangeComplete;

   procedure WillMove
     (This       : RecordsetEvents_Event;
      adReason   : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end WillMove;

   procedure MoveComplete
     (This       : RecordsetEvents_Event;
      adReason   : GNATCOM.Types.VARIANT;
      pError     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end MoveComplete;

   procedure EndOfRecordset
     (This       : RecordsetEvents_Event;
      fMoreData  : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end EndOfRecordset;

   procedure FetchProgress
     (This        : RecordsetEvents_Event;
      Progress    : GNATCOM.Types.VARIANT;
      MaxProgress : GNATCOM.Types.VARIANT;
      adStatus    : GNATCOM.Types.VARIANT;
      pRecordset  : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end FetchProgress;

   procedure FetchComplete
     (This       : RecordsetEvents_Event;
      pError     : GNATCOM.Types.VARIANT;
      adStatus   : GNATCOM.Types.VARIANT;
      pRecordset : GNATCOM.Types.VARIANT)
   is
   begin
      null;
   end FetchComplete;

end ADO.RecordsetEvents_Events;
