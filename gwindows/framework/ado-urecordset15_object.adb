package body ADO.uRecordset15_Object is

   function Get_Properties
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_Properties);
   end Get_Properties;

   function Get_AbsolutePosition
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_AbsolutePosition);
   end Get_AbsolutePosition;

   procedure Put_AbsolutePosition
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset15_Put_AbsolutePosition,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_AbsolutePosition;

   procedure PutRef_ActiveConnection
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT)
   is
   begin
      PutRef
        (This,
         uRecordset15_PutRef_ActiveConnection,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1));
   end PutRef_ActiveConnection;

   procedure Put_ActiveConnection
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset15_Put_ActiveConnection,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_ActiveConnection;

   function Get_ActiveConnection
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_ActiveConnection);
   end Get_ActiveConnection;

   function Get_BOF
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_BOF);
   end Get_BOF;

   function Get_Bookmark
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_Bookmark);
   end Get_Bookmark;

   procedure Put_Bookmark
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset15_Put_Bookmark,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Bookmark;

   function Get_CacheSize
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_CacheSize);
   end Get_CacheSize;

   procedure Put_CacheSize
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset15_Put_CacheSize,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_CacheSize;

   function Get_CursorType
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_CursorType);
   end Get_CursorType;

   procedure Put_CursorType
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset15_Put_CursorType,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_CursorType;

   function Get_EOF
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_EOF);
   end Get_EOF;

   function Get_Fields
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_Fields);
   end Get_Fields;

   function Get_LockType
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_LockType);
   end Get_LockType;

   procedure Put_LockType
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset15_Put_LockType,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_LockType;

   function Get_MaxRecords
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_MaxRecords);
   end Get_MaxRecords;

   procedure Put_MaxRecords
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset15_Put_MaxRecords,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_MaxRecords;

   function Get_RecordCount
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_RecordCount);
   end Get_RecordCount;

   procedure PutRef_Source
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT)
   is
   begin
      PutRef
        (This,
         uRecordset15_PutRef_Source,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1));
   end PutRef_Source;

   procedure Put_Source
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset15_Put_Source,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Source;

   function Get_Source
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_Source);
   end Get_Source;

   procedure AddNew
     (This      : uRecordset15_Type;
      FieldList : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Values    : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free      : Boolean := True)
   is
   begin
      Invoke
        (This,
         uRecordset15_AddNew,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Values,
          2 => FieldList),
         Free);
   end AddNew;

   procedure CancelUpdate
     (This : uRecordset15_Type)
   is
   begin
      Invoke (This, uRecordset15_CancelUpdate);
   end CancelUpdate;

   procedure Close
     (This : uRecordset15_Type)
   is
   begin
      Invoke (This, uRecordset15_Close);
   end Close;

   procedure Delete
     (This          : uRecordset15_Type;
      AffectRecords : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free          : Boolean := True)
   is
   begin
      Invoke
        (This,
         uRecordset15_Delete,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => AffectRecords),
         Free);
   end Delete;

   function GetRows
     (This   : uRecordset15_Type;
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
         uRecordset15_GetRows,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Fields,
          2 => Start,
          3 => Rows),
         Free);
   end GetRows;

   procedure Move
     (This       : uRecordset15_Type;
      NumRecords : GNATCOM.Types.VARIANT;
      Start      : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free       : Boolean := True)
   is
   begin
      Invoke
        (This,
         uRecordset15_Move,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Start,
          2 => NumRecords),
         Free);
   end Move;

   procedure MoveNext
     (This : uRecordset15_Type)
   is
   begin
      Invoke (This, uRecordset15_MoveNext);
   end MoveNext;

   procedure MovePrevious
     (This : uRecordset15_Type)
   is
   begin
      Invoke (This, uRecordset15_MovePrevious);
   end MovePrevious;

   procedure MoveFirst
     (This : uRecordset15_Type)
   is
   begin
      Invoke (This, uRecordset15_MoveFirst);
   end MoveFirst;

   procedure MoveLast
     (This : uRecordset15_Type)
   is
   begin
      Invoke (This, uRecordset15_MoveLast);
   end MoveLast;

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
      Free             : Boolean := True)
   is
   begin
      Invoke
        (This,
         uRecordset15_Open,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Options,
          2 => LockType,
          3 => CursorType,
          4 => ActiveConnection,
          5 => Source),
         Free);
   end Open;

   procedure Requery
     (This    : uRecordset15_Type;
      Options : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free    : Boolean := True)
   is
   begin
      Invoke
        (This,
         uRecordset15_Requery,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Options),
         Free);
   end Requery;

   procedure uxResync
     (This          : uRecordset15_Type;
      AffectRecords : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free          : Boolean := True)
   is
   begin
      Invoke
        (This,
         uRecordset15_uxResync,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => AffectRecords),
         Free);
   end uxResync;

   procedure Update
     (This   : uRecordset15_Type;
      Fields : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Values : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free   : Boolean := True)
   is
   begin
      Invoke
        (This,
         uRecordset15_Update,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Values,
          2 => Fields),
         Free);
   end Update;

   function Get_AbsolutePage
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_AbsolutePage);
   end Get_AbsolutePage;

   procedure Put_AbsolutePage
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset15_Put_AbsolutePage,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_AbsolutePage;

   function Get_EditMode
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_EditMode);
   end Get_EditMode;

   function Get_Filter
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_Filter);
   end Get_Filter;

   procedure Put_Filter
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset15_Put_Filter,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Filter;

   function Get_PageCount
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_PageCount);
   end Get_PageCount;

   function Get_PageSize
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_PageSize);
   end Get_PageSize;

   procedure Put_PageSize
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset15_Put_PageSize,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_PageSize;

   function Get_Sort
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_Sort);
   end Get_Sort;

   procedure Put_Sort
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset15_Put_Sort,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Sort;

   function Get_Status
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_Status);
   end Get_Status;

   function Get_State
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_State);
   end Get_State;

   function uxClone
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, uRecordset15_uxClone);
   end uxClone;

   procedure UpdateBatch
     (This          : uRecordset15_Type;
      AffectRecords : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free          : Boolean := True)
   is
   begin
      Invoke
        (This,
         uRecordset15_UpdateBatch,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => AffectRecords),
         Free);
   end UpdateBatch;

   procedure CancelBatch
     (This          : uRecordset15_Type;
      AffectRecords : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free          : Boolean := True)
   is
   begin
      Invoke
        (This,
         uRecordset15_CancelBatch,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => AffectRecords),
         Free);
   end CancelBatch;

   function Get_CursorLocation
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_CursorLocation);
   end Get_CursorLocation;

   procedure Put_CursorLocation
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset15_Put_CursorLocation,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_CursorLocation;

   function NextRecordset
     (This            : uRecordset15_Type;
      RecordsAffected : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         uRecordset15_NextRecordset,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => RecordsAffected),
         Free);
   end NextRecordset;

   function Supports
     (This          : uRecordset15_Type;
      CursorOptions : GNATCOM.Types.VARIANT;
      Free          : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         uRecordset15_Supports,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => CursorOptions),
         Free);
   end Supports;

   function Get_Collect
     (This  : uRecordset15_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get
        (This,
         uRecordset15_Get_Collect,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Index),
         Free);
   end Get_Collect;

   procedure Put_Collect
     (This  : uRecordset15_Type;
      Index : GNATCOM.Types.VARIANT;
      P2    : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset15_Put_Collect,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P2,
          2 => Index),
         Free);
   end Put_Collect;

   function Get_MarshalOptions
     (This : uRecordset15_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, uRecordset15_Get_MarshalOptions);
   end Get_MarshalOptions;

   procedure Put_MarshalOptions
     (This : uRecordset15_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         uRecordset15_Put_MarshalOptions,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_MarshalOptions;

   procedure Find
     (This            : uRecordset15_Type;
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
         uRecordset15_Find,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => Start,
          2 => SearchDirection,
          3 => SkipRecords,
          4 => Criteria),
         Free);
   end Find;

end ADO.uRecordset15_Object;
