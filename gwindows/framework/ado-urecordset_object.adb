package body ADO.uRecordset_Object is

   function Get_Properties
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_Properties);
   end Get_Properties;

   function Get_AbsolutePosition
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_AbsolutePosition);
   end Get_AbsolutePosition;

   procedure Put_AbsolutePosition
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset_Put_AbsolutePosition,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_AbsolutePosition;

   procedure PutRef_ActiveConnection
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT)
   is
   begin
      PutRef
        (This,
         uRecordset_PutRef_ActiveConnection,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1));
   end PutRef_ActiveConnection;

   procedure Put_ActiveConnection
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset_Put_ActiveConnection,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_ActiveConnection;

   function Get_ActiveConnection
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_ActiveConnection);
   end Get_ActiveConnection;

   function Get_BOF
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_BOF);
   end Get_BOF;

   function Get_Bookmark
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_Bookmark);
   end Get_Bookmark;

   procedure Put_Bookmark
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset_Put_Bookmark,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Bookmark;

   function Get_CacheSize
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_CacheSize);
   end Get_CacheSize;

   procedure Put_CacheSize
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset_Put_CacheSize,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_CacheSize;

   function Get_CursorType
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_CursorType);
   end Get_CursorType;

   procedure Put_CursorType
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset_Put_CursorType,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_CursorType;

   function Get_EOF
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_EOF);
   end Get_EOF;

   function Get_Fields
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_Fields);
   end Get_Fields;

   function Get_LockType
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_LockType);
   end Get_LockType;

   procedure Put_LockType
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset_Put_LockType,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_LockType;

   function Get_MaxRecords
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_MaxRecords);
   end Get_MaxRecords;

   procedure Put_MaxRecords
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset_Put_MaxRecords,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_MaxRecords;

   function Get_RecordCount
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_RecordCount);
   end Get_RecordCount;

   procedure PutRef_Source
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT)
   is
   begin
      PutRef
        (This,
         uRecordset_PutRef_Source,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1));
   end PutRef_Source;

   procedure Put_Source
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset_Put_Source,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Source;

   function Get_Source
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_Source);
   end Get_Source;

   procedure AddNew
     (This      : uRecordset_Type;
      FieldList : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Values    : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free      : Boolean := True)
   is
   begin
      Invoke
        (This,
         uRecordset_AddNew,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Values,
          2 => FieldList),
         Free);
   end AddNew;

   procedure CancelUpdate
     (This : uRecordset_Type)
   is
   begin
      Invoke (This, uRecordset_CancelUpdate);
   end CancelUpdate;

   procedure Close
     (This : uRecordset_Type)
   is
   begin
      Invoke (This, uRecordset_Close);
   end Close;

   procedure Delete
     (This          : uRecordset_Type;
      AffectRecords : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free          : Boolean := True)
   is
   begin
      Invoke
        (This,
         uRecordset_Delete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => AffectRecords),
         Free);
   end Delete;

   function GetRows
     (This   : uRecordset_Type;
      Rows   : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Start  : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Fields : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         uRecordset_GetRows,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Fields,
          2 => Start,
          3 => Rows),
         Free);
   end GetRows;

   procedure Move
     (This       : uRecordset_Type;
      NumRecords : GNATCOM.Types.VARIANT;
      Start      : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free       : Boolean := True)
   is
   begin
      Invoke
        (This,
         uRecordset_Move,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Start,
          2 => NumRecords),
         Free);
   end Move;

   procedure MoveNext
     (This : uRecordset_Type)
   is
   begin
      Invoke (This, uRecordset_MoveNext);
   end MoveNext;

   procedure MovePrevious
     (This : uRecordset_Type)
   is
   begin
      Invoke (This, uRecordset_MovePrevious);
   end MovePrevious;

   procedure MoveFirst
     (This : uRecordset_Type)
   is
   begin
      Invoke (This, uRecordset_MoveFirst);
   end MoveFirst;

   procedure MoveLast
     (This : uRecordset_Type)
   is
   begin
      Invoke (This, uRecordset_MoveLast);
   end MoveLast;

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
      Free             : Boolean := True)
   is
   begin
      Invoke
        (This,
         uRecordset_Open,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Options,
          2 => LockType,
          3 => CursorType,
          4 => ActiveConnection,
          5 => Source),
         Free);
   end Open;

   procedure Requery
     (This    : uRecordset_Type;
      Options : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free    : Boolean := True)
   is
   begin
      Invoke
        (This,
         uRecordset_Requery,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Options),
         Free);
   end Requery;

   procedure uxResync
     (This          : uRecordset_Type;
      AffectRecords : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free          : Boolean := True)
   is
   begin
      Invoke
        (This,
         uRecordset_uxResync,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => AffectRecords),
         Free);
   end uxResync;

   procedure Update
     (This   : uRecordset_Type;
      Fields : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Values : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free   : Boolean := True)
   is
   begin
      Invoke
        (This,
         uRecordset_Update,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Values,
          2 => Fields),
         Free);
   end Update;

   function Get_AbsolutePage
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_AbsolutePage);
   end Get_AbsolutePage;

   procedure Put_AbsolutePage
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset_Put_AbsolutePage,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_AbsolutePage;

   function Get_EditMode
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_EditMode);
   end Get_EditMode;

   function Get_Filter
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_Filter);
   end Get_Filter;

   procedure Put_Filter
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset_Put_Filter,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Filter;

   function Get_PageCount
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_PageCount);
   end Get_PageCount;

   function Get_PageSize
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_PageSize);
   end Get_PageSize;

   procedure Put_PageSize
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset_Put_PageSize,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_PageSize;

   function Get_Sort
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_Sort);
   end Get_Sort;

   procedure Put_Sort
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset_Put_Sort,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Sort;

   function Get_Status
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_Status);
   end Get_Status;

   function Get_State
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_State);
   end Get_State;

   function uxClone
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, uRecordset_uxClone);
   end uxClone;

   procedure UpdateBatch
     (This          : uRecordset_Type;
      AffectRecords : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free          : Boolean := True)
   is
   begin
      Invoke
        (This,
         uRecordset_UpdateBatch,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => AffectRecords),
         Free);
   end UpdateBatch;

   procedure CancelBatch
     (This          : uRecordset_Type;
      AffectRecords : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free          : Boolean := True)
   is
   begin
      Invoke
        (This,
         uRecordset_CancelBatch,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => AffectRecords),
         Free);
   end CancelBatch;

   function Get_CursorLocation
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_CursorLocation);
   end Get_CursorLocation;

   procedure Put_CursorLocation
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset_Put_CursorLocation,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_CursorLocation;

   function NextRecordset
     (This            : uRecordset_Type;
      RecordsAffected : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         uRecordset_NextRecordset,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => RecordsAffected),
         Free);
   end NextRecordset;

   function Supports
     (This          : uRecordset_Type;
      CursorOptions : GNATCOM.Types.VARIANT;
      Free          : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         uRecordset_Supports,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => CursorOptions),
         Free);
   end Supports;

   function Get_Collect
     (This  : uRecordset_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get
        (This,
         uRecordset_Get_Collect,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Index),
         Free);
   end Get_Collect;

   procedure Put_Collect
     (This  : uRecordset_Type;
      Index : GNATCOM.Types.VARIANT;
      P2    : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset_Put_Collect,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P2,
          2 => Index),
         Free);
   end Put_Collect;

   function Get_MarshalOptions
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_MarshalOptions);
   end Get_MarshalOptions;

   procedure Put_MarshalOptions
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset_Put_MarshalOptions,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_MarshalOptions;

   procedure Find
     (This            : uRecordset_Type;
      Criteria        : GNATCOM.Types.VARIANT;
      SkipRecords     : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      SearchDirection : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Start           : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True)
   is
   begin
      Invoke
        (This,
         uRecordset_Find,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Start,
          2 => SearchDirection,
          3 => SkipRecords,
          4 => Criteria),
         Free);
   end Find;

   procedure Cancel
     (This : uRecordset_Type)
   is
   begin
      Invoke (This, uRecordset_Cancel);
   end Cancel;

   function Get_DataSource
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_DataSource);
   end Get_DataSource;

   procedure PutRef_DataSource
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT)
   is
   begin
      PutRef
        (This,
         uRecordset_PutRef_DataSource,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1));
   end PutRef_DataSource;

   procedure Save
     (This          : uRecordset_Type;
      FileName      : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      PersistFormat : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free          : Boolean := True)
   is
   begin
      Invoke
        (This,
         uRecordset_Save,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => PersistFormat,
          2 => FileName),
         Free);
   end Save;

   function Get_ActiveCommand
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_ActiveCommand);
   end Get_ActiveCommand;

   procedure Put_StayInSync
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset_Put_StayInSync,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_StayInSync;

   function Get_StayInSync
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_StayInSync);
   end Get_StayInSync;

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
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         uRecordset_GetString,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => NullExpr,
          2 => RowDelimeter,
          3 => ColumnDelimeter,
          4 => NumRows,
          5 => StringFormat),
         Free);
   end GetString;

   function Get_DataMember
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset_Get_DataMember);
   end Get_DataMember;

   procedure Put_DataMember
     (This : uRecordset_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset_Put_DataMember,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_DataMember;

   function CompareBookmarks
     (This      : uRecordset_Type;
      Bookmark1 : GNATCOM.Types.VARIANT;
      Bookmark2 : GNATCOM.Types.VARIANT;
      Free      : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         uRecordset_CompareBookmarks,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Bookmark2,
          2 => Bookmark1),
         Free);
   end CompareBookmarks;

   function Clone
     (This     : uRecordset_Type;
      LockType : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free     : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         uRecordset_Clone,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => LockType),
         Free);
   end Clone;

   procedure Resync
     (This          : uRecordset_Type;
      AffectRecords : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      ResyncValues  : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free          : Boolean := True)
   is
   begin
      Invoke
        (This,
         uRecordset_Resync,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => ResyncValues,
          2 => AffectRecords),
         Free);
   end Resync;

end ADO.uRecordset_Object;
