with GNATCOM.Dispinterface;

package ADO.uRecordset_Interface is

   type uRecordset_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out uRecordset_Type);

   function Pointer (This : uRecordset_Type)
     return Pointer_To_uRecordset;

   procedure Attach (This    : in out uRecordset_Type;
                     Pointer : in     Pointer_To_uRecordset);

   function Get_Properties
     (This      : uRecordset_Type)
     return Pointer_To_Properties;

   function Get_AbsolutePosition
     (This : uRecordset_Type)
     return PositionEnum;

   procedure Put_AbsolutePosition
     (This : uRecordset_Type;
      pl   : PositionEnum);

   procedure PutRef_ActiveConnection
     (This : uRecordset_Type;
      pvar : GNATCOM.Types.Pointer_To_IDispatch);

   procedure Put_ActiveConnection
     (This : uRecordset_Type;
      pvar : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_ActiveConnection
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   function Get_BOF
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT_BOOL;

   function Get_Bookmark
     (This       : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Bookmark
     (This       : uRecordset_Type;
      pvBookmark : GNATCOM.Types.VARIANT;
      Free       : Boolean := True);

   function Get_CacheSize
     (This : uRecordset_Type)
     return Interfaces.C.long;

   procedure Put_CacheSize
     (This : uRecordset_Type;
      pl   : Interfaces.C.long);

   function Get_CursorType
     (This         : uRecordset_Type)
     return CursorTypeEnum;

   procedure Put_CursorType
     (This         : uRecordset_Type;
      plCursorType : CursorTypeEnum);

   function Get_EOF
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT_BOOL;

   function Get_Fields
     (This      : uRecordset_Type)
     return Pointer_To_Fields;

   function Get_LockType
     (This       : uRecordset_Type)
     return LockTypeEnum;

   procedure Put_LockType
     (This       : uRecordset_Type;
      plLockType : LockTypeEnum);

   function Get_MaxRecords
     (This         : uRecordset_Type)
     return Interfaces.C.long;

   procedure Put_MaxRecords
     (This         : uRecordset_Type;
      plMaxRecords : Interfaces.C.long);

   function Get_RecordCount
     (This : uRecordset_Type)
     return Interfaces.C.long;

   procedure PutRef_Source
     (This     : uRecordset_Type;
      pvSource : GNATCOM.Types.Pointer_To_IDispatch);

   procedure Put_Source
     (This     : uRecordset_Type;
      pvSource : GNATCOM.Types.BSTR;
      Free     : Boolean := True);

   function Get_Source
     (This     : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   procedure AddNew
     (This      : uRecordset_Type;
      FieldList : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Values    : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Free      : Boolean := True);

   procedure CancelUpdate
     (This : uRecordset_Type);

   procedure Close
     (This : uRecordset_Type);

   procedure Delete
     (This          : uRecordset_Type;
      AffectRecords : AffectEnum);

   function GetRows
     (This   : uRecordset_Type;
      Rows   : Interfaces.C.long;
      Start  : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Fields : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure Move
     (This       : uRecordset_Type;
      NumRecords : Interfaces.C.long;
      Start      : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
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
      CursorType       : CursorTypeEnum;
      LockType         : LockTypeEnum;
      Options          : Interfaces.C.long;
      Free             : Boolean := True);

   procedure Requery
     (This    : uRecordset_Type;
      Options : Interfaces.C.long);

   procedure uxResync
     (This          : uRecordset_Type;
      AffectRecords : AffectEnum);

   procedure Update
     (This   : uRecordset_Type;
      Fields : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Values : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Free   : Boolean := True);

   function Get_AbsolutePage
     (This : uRecordset_Type)
     return PositionEnum;

   procedure Put_AbsolutePage
     (This : uRecordset_Type;
      pl   : PositionEnum);

   function Get_EditMode
     (This : uRecordset_Type)
     return EditModeEnum;

   function Get_Filter
     (This     : uRecordset_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Filter
     (This     : uRecordset_Type;
      Criteria : GNATCOM.Types.VARIANT;
      Free     : Boolean := True);

   function Get_PageCount
     (This : uRecordset_Type)
     return Interfaces.C.long;

   function Get_PageSize
     (This : uRecordset_Type)
     return Interfaces.C.long;

   procedure Put_PageSize
     (This : uRecordset_Type;
      pl   : Interfaces.C.long);

   function Get_Sort
     (This     : uRecordset_Type)
     return GNATCOM.Types.BSTR;

   procedure Put_Sort
     (This     : uRecordset_Type;
      Criteria : GNATCOM.Types.BSTR;
      Free     : Boolean := True);

   function Get_Status
     (This : uRecordset_Type)
     return Interfaces.C.long;

   function Get_State
     (This       : uRecordset_Type)
     return Interfaces.C.long;

   function uxClone
     (This      : uRecordset_Type)
     return Pointer_To_uRecordset;

   procedure UpdateBatch
     (This          : uRecordset_Type;
      AffectRecords : AffectEnum);

   procedure CancelBatch
     (This          : uRecordset_Type;
      AffectRecords : AffectEnum);

   function Get_CursorLocation
     (This        : uRecordset_Type)
     return CursorLocationEnum;

   procedure Put_CursorLocation
     (This        : uRecordset_Type;
      plCursorLoc : CursorLocationEnum);

   function NextRecordset
     (This            : uRecordset_Type;
      RecordsAffected : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING)
     return Pointer_To_uRecordset;

   function Supports
     (This          : uRecordset_Type;
      CursorOptions : CursorOptionEnum)
     return GNATCOM.Types.VARIANT_BOOL;

   function Get_Collect
     (This  : uRecordset_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure Put_Collect
     (This  : uRecordset_Type;
      Index : GNATCOM.Types.VARIANT;
      pvar  : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);

   function Get_MarshalOptions
     (This      : uRecordset_Type)
     return MarshalOptionsEnum;

   procedure Put_MarshalOptions
     (This      : uRecordset_Type;
      peMarshal : MarshalOptionsEnum);

   procedure Find
     (This            : uRecordset_Type;
      Criteria        : GNATCOM.Types.BSTR;
      SkipRecords     : Interfaces.C.long;
      SearchDirection : SearchDirectionEnum;
      Start           : GNATCOM.Types.VARIANT
         := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True);

   procedure Cancel
     (This : uRecordset_Type);

   function Get_DataSource
     (This            : uRecordset_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;

   procedure PutRef_DataSource
     (This            : uRecordset_Type;
      ppunkDataSource : GNATCOM.Types.Pointer_To_IUnknown);

   procedure Save
     (This          : uRecordset_Type;
      FileName      : GNATCOM.Types.BSTR;
      PersistFormat : PersistFormatEnum;
      Free          : Boolean := True);

   function Get_ActiveCommand
     (This  : uRecordset_Type)
     return GNATCOM.Types.Pointer_To_IDispatch;

   procedure Put_StayInSync
     (This         : uRecordset_Type;
      pbStayInSync : GNATCOM.Types.VARIANT_BOOL);

   function Get_StayInSync
     (This         : uRecordset_Type)
     return GNATCOM.Types.VARIANT_BOOL;

   function GetString
     (This            : uRecordset_Type;
      StringFormat    : StringFormatEnum;
      NumRows         : Interfaces.C.long;
      ColumnDelimeter : GNATCOM.Types.BSTR;
      RowDelimeter    : GNATCOM.Types.BSTR;
      NullExpr        : GNATCOM.Types.BSTR;
      Free            : Boolean := True)
     return GNATCOM.Types.BSTR;

   function Get_DataMember
     (This            : uRecordset_Type)
     return GNATCOM.Types.BSTR;

   procedure Put_DataMember
     (This            : uRecordset_Type;
      pbstrDataMember : GNATCOM.Types.BSTR;
      Free            : Boolean := True);

   function CompareBookmarks
     (This      : uRecordset_Type;
      Bookmark1 : GNATCOM.Types.VARIANT;
      Bookmark2 : GNATCOM.Types.VARIANT;
      Free      : Boolean := True)
     return CompareEnum;

   function Clone
     (This      : uRecordset_Type;
      LockType  : LockTypeEnum)
     return Pointer_To_uRecordset;

   procedure Resync
     (This          : uRecordset_Type;
      AffectRecords : AffectEnum;
      ResyncValues  : ResyncEnum);

end ADO.uRecordset_Interface;
