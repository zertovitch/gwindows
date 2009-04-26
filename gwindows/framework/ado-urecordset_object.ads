with GNATCOM.Dispinterface;

package ADO.uRecordset_Object is

   type uRecordset_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Properties
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   function Get_AbsolutePosition
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_AbsolutePosition
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   procedure PutRef_ActiveConnection
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT);

   procedure Put_ActiveConnection
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_ActiveConnection
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   function Get_BOF
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Bookmark
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Bookmark
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_CacheSize
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_CacheSize
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_CursorType
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_CursorType
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_EOF
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Fields
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   function Get_LockType
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_LockType
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_MaxRecords
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_MaxRecords
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_RecordCount
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   procedure PutRef_Source
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT);

   procedure Put_Source
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Source
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   procedure AddNew
     (This      : uRecordset_Type;
      FieldList : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Values    : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free      : Boolean := True);

   procedure CancelUpdate
     (This : uRecordset_Type);

   procedure Close
     (This : uRecordset_Type);

   procedure Delete
     (This          : uRecordset_Type;
      AffectRecords : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free          : Boolean := True);

   function GetRows
     (This   : uRecordset_Type;
      Rows   : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Start  : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Fields : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure Move
     (This       : uRecordset_Type;
      NumRecords : GNATCOM.Types.VARIANT;
      Start      : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free       : Boolean := True);

   procedure MoveNext
     (This : uRecordset_Type);

   procedure MovePrevious
     (This : uRecordset_Type);

   procedure MoveFirst
     (This : uRecordset_Type);

   procedure MoveLast
     (This : uRecordset_Type);

   procedure Open
     (This             : uRecordset_Type;
      Source           : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      ActiveConnection : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      CursorType       : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      LockType         : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Options          : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free             : Boolean := True);

   procedure Requery
     (This    : uRecordset_Type;
      Options : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free    : Boolean := True);

   procedure uxResync
     (This          : uRecordset_Type;
      AffectRecords : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free          : Boolean := True);

   procedure Update
     (This   : uRecordset_Type;
      Fields : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Values : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free   : Boolean := True);

   function Get_AbsolutePage
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_AbsolutePage
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_EditMode
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Filter
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Filter
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_PageCount
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   function Get_PageSize
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_PageSize
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Sort
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Sort
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Status
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   function Get_State
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   function uxClone
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   procedure UpdateBatch
     (This          : uRecordset_Type;
      AffectRecords : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free          : Boolean := True);

   procedure CancelBatch
     (This          : uRecordset_Type;
      AffectRecords : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free          : Boolean := True);

   function Get_CursorLocation
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_CursorLocation
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function NextRecordset
     (This            : uRecordset_Type;
      RecordsAffected : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function Supports
     (This          : uRecordset_Type;
      CursorOptions : GNATCOM.Types.VARIANT;
      Free          : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function Get_Collect
     (This  : uRecordset_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure Put_Collect
     (This  : uRecordset_Type;
      Index : GNATCOM.Types.VARIANT;
      P2    : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);

   function Get_MarshalOptions
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_MarshalOptions
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   procedure Find
     (This            : uRecordset_Type;
      Criteria        : GNATCOM.Types.VARIANT;
      SkipRecords     : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      SearchDirection : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Start           : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True);

   procedure Cancel
     (This : uRecordset_Type);

   function Get_DataSource
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   procedure PutRef_DataSource
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT);

   procedure Save
     (This          : uRecordset_Type;
      FileName      : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      PersistFormat : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free          : Boolean := True);

   function Get_ActiveCommand
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_StayInSync
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_StayInSync
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   function GetString
     (This            : uRecordset_Type;
      StringFormat    : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      NumRows         : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      ColumnDelimeter : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      RowDelimeter    : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      NullExpr        : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function Get_DataMember
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_DataMember
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function CompareBookmarks
     (This      : uRecordset_Type;
      Bookmark1 : GNATCOM.Types.VARIANT;
      Bookmark2 : GNATCOM.Types.VARIANT;
      Free      : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function Clone
     (This     : uRecordset_Type;
      LockType : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free     : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure Resync
     (This          : uRecordset_Type;
      AffectRecords : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      ResyncValues  : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free          : Boolean := True);

end ADO.uRecordset_Object;
