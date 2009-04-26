with GNATCOM.Dispinterface;

package ADO.uRecordset15_Interface is

   type uRecordset15_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out uRecordset15_Type);

   function Pointer (This : uRecordset15_Type)
     return Pointer_To_uRecordset15;

   procedure Attach (This    : in out uRecordset15_Type;
                     Pointer : in     Pointer_To_uRecordset15);

   function Get_Properties
     (This      : uRecordset15_Type)
     return Pointer_To_Properties;

   function Get_AbsolutePosition
     (This : uRecordset15_Type)
     return PositionEnum;

   procedure Put_AbsolutePosition
     (This : uRecordset15_Type;
      pl   : PositionEnum);

   procedure PutRef_ActiveConnection
     (This : uRecordset15_Type;
      pvar : GNATCOM.Types.Pointer_To_IDispatch);

   procedure Put_ActiveConnection
     (This : uRecordset15_Type;
      pvar : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_ActiveConnection
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   function Get_BOF
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT_BOOL;

   function Get_Bookmark
     (This       : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Bookmark
     (This       : uRecordset15_Type;
      pvBookmark : GNATCOM.Types.VARIANT;
      Free       : Boolean := True);

   function Get_CacheSize
     (This : uRecordset15_Type)
     return Interfaces.C.long;

   procedure Put_CacheSize
     (This : uRecordset15_Type;
      pl   : Interfaces.C.long);

   function Get_CursorType
     (This         : uRecordset15_Type)
     return CursorTypeEnum;

   procedure Put_CursorType
     (This         : uRecordset15_Type;
      plCursorType : CursorTypeEnum);

   function Get_EOF
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT_BOOL;

   function Get_Fields
     (This      : uRecordset15_Type)
     return Pointer_To_Fields;

   function Get_LockType
     (This       : uRecordset15_Type)
     return LockTypeEnum;

   procedure Put_LockType
     (This       : uRecordset15_Type;
      plLockType : LockTypeEnum);

   function Get_MaxRecords
     (This         : uRecordset15_Type)
     return Interfaces.C.long;

   procedure Put_MaxRecords
     (This         : uRecordset15_Type;
      plMaxRecords : Interfaces.C.long);

   function Get_RecordCount
     (This : uRecordset15_Type)
     return Interfaces.C.long;

   procedure PutRef_Source
     (This     : uRecordset15_Type;
      pvSource : GNATCOM.Types.Pointer_To_IDispatch);

   procedure Put_Source
     (This     : uRecordset15_Type;
      pvSource : GNATCOM.Types.BSTR;
      Free     : Boolean := True);

   function Get_Source
     (This     : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   procedure AddNew
     (This      : uRecordset15_Type;
      FieldList : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Values    : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Free      : Boolean := True);

   procedure CancelUpdate
     (This : uRecordset15_Type);

   procedure Close
     (This : uRecordset15_Type);

   procedure Delete
     (This          : uRecordset15_Type;
      AffectRecords : AffectEnum);

   function GetRows
     (This   : uRecordset15_Type;
      Rows   : Interfaces.C.long;
      Start  : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Fields : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure Move
     (This       : uRecordset15_Type;
      NumRecords : Interfaces.C.long;
      Start      : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
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
      Source           : GNATCOM.Types.VARIANT :=
         GNATCOM.Types.VARIANT_MISSING;
      ActiveConnection : GNATCOM.Types.VARIANT :=
         GNATCOM.Types.VARIANT_MISSING;
      CursorType       : CursorTypeEnum;
      LockType         : LockTypeEnum;
      Options          : Interfaces.C.long;
      Free             : Boolean := True);

   procedure Requery
     (This    : uRecordset15_Type;
      Options : Interfaces.C.long);

   procedure uxResync
     (This          : uRecordset15_Type;
      AffectRecords : AffectEnum);

   procedure Update
     (This   : uRecordset15_Type;
      Fields : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Values : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Free   : Boolean := True);

   function Get_AbsolutePage
     (This : uRecordset15_Type)
     return PositionEnum;

   procedure Put_AbsolutePage
     (This : uRecordset15_Type;
      pl   : PositionEnum);

   function Get_EditMode
     (This : uRecordset15_Type)
     return EditModeEnum;

   function Get_Filter
     (This     : uRecordset15_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Filter
     (This     : uRecordset15_Type;
      Criteria : GNATCOM.Types.VARIANT;
      Free     : Boolean := True);

   function Get_PageCount
     (This : uRecordset15_Type)
     return Interfaces.C.long;

   function Get_PageSize
     (This : uRecordset15_Type)
     return Interfaces.C.long;

   procedure Put_PageSize
     (This : uRecordset15_Type;
      pl   : Interfaces.C.long);

   function Get_Sort
     (This     : uRecordset15_Type)
     return GNATCOM.Types.BSTR;

   procedure Put_Sort
     (This     : uRecordset15_Type;
      Criteria : GNATCOM.Types.BSTR;
      Free     : Boolean := True);

   function Get_Status
     (This : uRecordset15_Type)
     return Interfaces.C.long;

   function Get_State
     (This       : uRecordset15_Type)
     return Interfaces.C.long;

   function uxClone
     (This      : uRecordset15_Type)
     return Pointer_To_uRecordset;

   procedure UpdateBatch
     (This          : uRecordset15_Type;
      AffectRecords : AffectEnum);

   procedure CancelBatch
     (This          : uRecordset15_Type;
      AffectRecords : AffectEnum);

   function Get_CursorLocation
     (This        : uRecordset15_Type)
     return CursorLocationEnum;

   procedure Put_CursorLocation
     (This        : uRecordset15_Type;
      plCursorLoc : CursorLocationEnum);

   function NextRecordset
     (This            : uRecordset15_Type;
      RecordsAffected : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING)
     return Pointer_To_uRecordset;

   function Supports
     (This          : uRecordset15_Type;
      CursorOptions : CursorOptionEnum)
     return GNATCOM.Types.VARIANT_BOOL;

   function Get_Collect
     (This  : uRecordset15_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;

   procedure Put_Collect
     (This  : uRecordset15_Type;
      Index : GNATCOM.Types.VARIANT;
      pvar  : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);

   function Get_MarshalOptions
     (This      : uRecordset15_Type)
     return MarshalOptionsEnum;

   procedure Put_MarshalOptions
     (This      : uRecordset15_Type;
      peMarshal : MarshalOptionsEnum);

   procedure Find
     (This            : uRecordset15_Type;
      Criteria        : GNATCOM.Types.BSTR;
      SkipRecords     : Interfaces.C.long;
      SearchDirection : SearchDirectionEnum;
      Start           : GNATCOM.Types.VARIANT := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True);

end ADO.uRecordset15_Interface;
