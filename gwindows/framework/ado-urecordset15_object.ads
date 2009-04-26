with GNATCOM.Dispinterface;

package ADO.uRecordset15_Object is

   type uRecordset15_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Properties
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_AbsolutePosition
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_AbsolutePosition
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   procedure PutRef_ActiveConnection
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT);

   procedure Put_ActiveConnection
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_ActiveConnection
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_BOF
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Bookmark
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Bookmark
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_CacheSize
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_CacheSize
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_CursorType
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_CursorType
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_EOF
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Fields
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_LockType
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_LockType
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_MaxRecords
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_MaxRecords
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_RecordCount
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   procedure PutRef_Source
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT);

   procedure Put_Source
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Source
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   procedure AddNew
     (This      : uRecordset15_Type;
      FieldList : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Values    : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free      : Boolean := True);

   procedure CancelUpdate
     (This : uRecordset15_Type);

   procedure Close
     (This : uRecordset15_Type);

   procedure Delete
     (This          : uRecordset15_Type;
      AffectRecords : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free          : Boolean := True);

   function GetRows
     (This   : uRecordset15_Type;
      Rows   : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Start  : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Fields : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure Move
     (This       : uRecordset15_Type;
      NumRecords : GNATCOM.Types.VARIANT;
      Start      : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free       : Boolean := True);

   procedure MoveNext
     (This : uRecordset15_Type);

   procedure MovePrevious
     (This : uRecordset15_Type);

   procedure MoveFirst
     (This : uRecordset15_Type);

   procedure MoveLast
     (This : uRecordset15_Type);

   procedure Open
     (This             : uRecordset15_Type;
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
     (This    : uRecordset15_Type;
      Options : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free    : Boolean := True);

   procedure uxResync
     (This          : uRecordset15_Type;
      AffectRecords : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free          : Boolean := True);

   procedure Update
     (This   : uRecordset15_Type;
      Fields : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Values : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free   : Boolean := True);

   function Get_AbsolutePage
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_AbsolutePage
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_EditMode
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Filter
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Filter
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_PageCount
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_PageSize
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_PageSize
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Sort
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Sort
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Status
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_State
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   function uxClone
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   procedure UpdateBatch
     (This          : uRecordset15_Type;
      AffectRecords : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free          : Boolean := True);

   procedure CancelBatch
     (This          : uRecordset15_Type;
      AffectRecords : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free          : Boolean := True);

   function Get_CursorLocation
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_CursorLocation
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function NextRecordset
     (This            : uRecordset15_Type;
      RecordsAffected : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function Supports
     (This          : uRecordset15_Type;
      CursorOptions : GNATCOM.Types.VARIANT;
      Free          : Boolean := True)
     return GNATCOM.Types.VARIANT;

   function Get_Collect
     (This  : uRecordset15_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure Put_Collect
     (This  : uRecordset15_Type;
      Index : GNATCOM.Types.VARIANT;
      P2    : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);

   function Get_MarshalOptions
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_MarshalOptions
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   procedure Find
     (This            : uRecordset15_Type;
      Criteria        : GNATCOM.Types.VARIANT;
      SkipRecords     : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      SearchDirection : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Start           : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True);

end ADO.uRecordset15_Object;
