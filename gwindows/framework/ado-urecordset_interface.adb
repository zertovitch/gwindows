with GNATCOM.Iinterface;

with GNATCOM.Errors;

package body ADO.uRecordset_Interface is

   procedure Initialize (This : in out uRecordset_Type) is
   begin
      Set_IID (This, IID_uRecordset);
   end Initialize;

   function Pointer (This : uRecordset_Type)
     return Pointer_To_uRecordset
   is
   begin
      return To_Pointer_To_uRecordset (Address (This));
   end Pointer;

   procedure Attach (This    : in out uRecordset_Type;
                     Pointer : in     Pointer_To_uRecordset)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_Properties
     (This      : uRecordset_Type)
     return Pointer_To_Properties
   is
       RetVal : aliased Pointer_To_Properties;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Properties
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Properties;

   function Get_AbsolutePosition
     (This : uRecordset_Type)
     return PositionEnum
   is
       RetVal : aliased PositionEnum;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_AbsolutePosition
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_AbsolutePosition;

   procedure Put_AbsolutePosition
     (This : uRecordset_Type;
      pl   : PositionEnum)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_AbsolutePosition
         (Pointer (This),
          pl));

   end Put_AbsolutePosition;

   procedure PutRef_ActiveConnection
     (This : uRecordset_Type;
      pvar : GNATCOM.Types.Pointer_To_IDispatch)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.PutRef_ActiveConnection
         (Pointer (This),
          pvar));

   end PutRef_ActiveConnection;

   procedure Put_ActiveConnection
     (This : uRecordset_Type;
      pvar : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_ActiveConnection
         (Pointer (This),
          pvar));

      if Free then
         GNATCOM.Iinterface.Free (pvar);
      end if;

   end Put_ActiveConnection;

   function Get_ActiveConnection
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
       RetVal : aliased GNATCOM.Types.VARIANT;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_ActiveConnection
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_ActiveConnection;

   function Get_BOF
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT_BOOL
   is
       RetVal : aliased GNATCOM.Types.VARIANT_BOOL;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_BOF
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_BOF;

   function Get_Bookmark
     (This       : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
       RetVal : aliased GNATCOM.Types.VARIANT;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Bookmark
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Bookmark;

   procedure Put_Bookmark
     (This       : uRecordset_Type;
      pvBookmark : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Bookmark
         (Pointer (This),
          pvBookmark));

      if Free then
         GNATCOM.Iinterface.Free (pvBookmark);
      end if;

   end Put_Bookmark;

   function Get_CacheSize
     (This : uRecordset_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_CacheSize
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_CacheSize;

   procedure Put_CacheSize
     (This : uRecordset_Type;
      pl   : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_CacheSize
         (Pointer (This),
          pl));

   end Put_CacheSize;

   function Get_CursorType
     (This         : uRecordset_Type)
     return CursorTypeEnum
   is
       RetVal : aliased CursorTypeEnum;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_CursorType
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_CursorType;

   procedure Put_CursorType
     (This         : uRecordset_Type;
      plCursorType : CursorTypeEnum)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_CursorType
         (Pointer (This),
          plCursorType));

   end Put_CursorType;

   function Get_EOF
     (This : uRecordset_Type)
     return GNATCOM.Types.VARIANT_BOOL
   is
       RetVal : aliased GNATCOM.Types.VARIANT_BOOL;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_EOF
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_EOF;

   function Get_Fields
     (This      : uRecordset_Type)
     return Pointer_To_Fields
   is
       RetVal : aliased Pointer_To_Fields;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Fields
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Fields;

   function Get_LockType
     (This       : uRecordset_Type)
     return LockTypeEnum
   is
       RetVal : aliased LockTypeEnum;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_LockType
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_LockType;

   procedure Put_LockType
     (This       : uRecordset_Type;
      plLockType : LockTypeEnum)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_LockType
         (Pointer (This),
          plLockType));

   end Put_LockType;

   function Get_MaxRecords
     (This         : uRecordset_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_MaxRecords
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_MaxRecords;

   procedure Put_MaxRecords
     (This         : uRecordset_Type;
      plMaxRecords : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_MaxRecords
         (Pointer (This),
          plMaxRecords));

   end Put_MaxRecords;

   function Get_RecordCount
     (This : uRecordset_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_RecordCount
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_RecordCount;

   procedure PutRef_Source
     (This     : uRecordset_Type;
      pvSource : GNATCOM.Types.Pointer_To_IDispatch)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.PutRef_Source
         (Pointer (This),
          pvSource));

   end PutRef_Source;

   procedure Put_Source
     (This     : uRecordset_Type;
      pvSource : GNATCOM.Types.BSTR;
      Free     : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Source
         (Pointer (This),
          pvSource));

      if Free then
         GNATCOM.Iinterface.Free (pvSource);
      end if;

   end Put_Source;

   function Get_Source
     (This     : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
       RetVal : aliased GNATCOM.Types.VARIANT;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Source
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Source;

   procedure AddNew
     (This      : uRecordset_Type;
      FieldList : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Values    : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Free      : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.AddNew
         (Pointer (This),
          FieldList,
          Values));

      if Free then
         GNATCOM.Iinterface.Free (FieldList);
         GNATCOM.Iinterface.Free (Values);
      end if;

   end AddNew;

   procedure CancelUpdate
     (This : uRecordset_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.CancelUpdate
         (Pointer (This)));

   end CancelUpdate;

   procedure Close
     (This : uRecordset_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Close
         (Pointer (This)));

   end Close;

   procedure Delete
     (This          : uRecordset_Type;
      AffectRecords : AffectEnum)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Delete
         (Pointer (This),
          AffectRecords));

   end Delete;

   function GetRows
     (This   : uRecordset_Type;
      Rows   : Interfaces.C.long;
      Start  : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Fields : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Free   : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
       RetVal : aliased GNATCOM.Types.VARIANT;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetRows
         (Pointer (This),
          Rows,
          Start,
          Fields,
          RetVal'Unchecked_Access));

      if Free then
         GNATCOM.Iinterface.Free (Start);
         GNATCOM.Iinterface.Free (Fields);
      end if;

      return RetVal;
   end GetRows;

   procedure Move
     (This       : uRecordset_Type;
      NumRecords : Interfaces.C.long;
      Start      : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Free       : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Move
         (Pointer (This),
          NumRecords,
          Start));

      if Free then
         GNATCOM.Iinterface.Free (Start);
      end if;

   end Move;

   procedure MoveNext
     (This : uRecordset_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveNext
         (Pointer (This)));

   end MoveNext;

   procedure MovePrevious
     (This : uRecordset_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MovePrevious
         (Pointer (This)));

   end MovePrevious;

   procedure MoveFirst
     (This : uRecordset_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveFirst
         (Pointer (This)));

   end MoveFirst;

   procedure MoveLast
     (This : uRecordset_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveLast
         (Pointer (This)));

   end MoveLast;

   procedure Open
     (This             : uRecordset_Type;
      Source           : GNATCOM.Types.VARIANT
         := GNATCOM.Types.VARIANT_MISSING;
      ActiveConnection : GNATCOM.Types.VARIANT
         := GNATCOM.Types.VARIANT_MISSING;
      CursorType       : CursorTypeEnum;
      LockType         : LockTypeEnum;
      Options          : Interfaces.C.long;
      Free             : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Open
         (Pointer (This),
          Source,
          ActiveConnection,
          CursorType,
          LockType,
          Options));

      if Free then
         GNATCOM.Iinterface.Free (Source);
         GNATCOM.Iinterface.Free (ActiveConnection);
      end if;

   end Open;

   procedure Requery
     (This    : uRecordset_Type;
      Options : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Requery
         (Pointer (This),
          Options));

   end Requery;

   procedure uxResync
     (This          : uRecordset_Type;
      AffectRecords : AffectEnum)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.uxResync
         (Pointer (This),
          AffectRecords));

   end uxResync;

   procedure Update
     (This   : uRecordset_Type;
      Fields : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Values : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Free   : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Update
         (Pointer (This),
          Fields,
          Values));

      if Free then
         GNATCOM.Iinterface.Free (Fields);
         GNATCOM.Iinterface.Free (Values);
      end if;

   end Update;

   function Get_AbsolutePage
     (This : uRecordset_Type)
     return PositionEnum
   is
       RetVal : aliased PositionEnum;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_AbsolutePage
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_AbsolutePage;

   procedure Put_AbsolutePage
     (This : uRecordset_Type;
      pl   : PositionEnum)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_AbsolutePage
         (Pointer (This),
          pl));

   end Put_AbsolutePage;

   function Get_EditMode
     (This : uRecordset_Type)
     return EditModeEnum
   is
       RetVal : aliased EditModeEnum;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_EditMode
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_EditMode;

   function Get_Filter
     (This     : uRecordset_Type)
     return GNATCOM.Types.VARIANT
   is
       RetVal : aliased GNATCOM.Types.VARIANT;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Filter
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Filter;

   procedure Put_Filter
     (This     : uRecordset_Type;
      Criteria : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Filter
         (Pointer (This),
          Criteria));

      if Free then
         GNATCOM.Iinterface.Free (Criteria);
      end if;

   end Put_Filter;

   function Get_PageCount
     (This : uRecordset_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_PageCount
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_PageCount;

   function Get_PageSize
     (This : uRecordset_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_PageSize
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_PageSize;

   procedure Put_PageSize
     (This : uRecordset_Type;
      pl   : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_PageSize
         (Pointer (This),
          pl));

   end Put_PageSize;

   function Get_Sort
     (This     : uRecordset_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Sort
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Sort;

   procedure Put_Sort
     (This     : uRecordset_Type;
      Criteria : GNATCOM.Types.BSTR;
      Free     : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Sort
         (Pointer (This),
          Criteria));

      if Free then
         GNATCOM.Iinterface.Free (Criteria);
      end if;

   end Put_Sort;

   function Get_Status
     (This : uRecordset_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Status
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Status;

   function Get_State
     (This       : uRecordset_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_State
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_State;

   function uxClone
     (This      : uRecordset_Type)
     return Pointer_To_uRecordset
   is
       RetVal : aliased Pointer_To_uRecordset;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.uxClone
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end uxClone;

   procedure UpdateBatch
     (This          : uRecordset_Type;
      AffectRecords : AffectEnum)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.UpdateBatch
         (Pointer (This),
          AffectRecords));

   end UpdateBatch;

   procedure CancelBatch
     (This          : uRecordset_Type;
      AffectRecords : AffectEnum)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.CancelBatch
         (Pointer (This),
          AffectRecords));

   end CancelBatch;

   function Get_CursorLocation
     (This        : uRecordset_Type)
     return CursorLocationEnum
   is
       RetVal : aliased CursorLocationEnum;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_CursorLocation
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_CursorLocation;

   procedure Put_CursorLocation
     (This        : uRecordset_Type;
      plCursorLoc : CursorLocationEnum)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_CursorLocation
         (Pointer (This),
          plCursorLoc));

   end Put_CursorLocation;

   function NextRecordset
     (This            : uRecordset_Type;
      RecordsAffected : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING)
     return Pointer_To_uRecordset
   is
       RetVal : aliased Pointer_To_uRecordset;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.NextRecordset
         (Pointer (This),
          RecordsAffected,
          RetVal'Unchecked_Access));

      return RetVal;
   end NextRecordset;

   function Supports
     (This          : uRecordset_Type;
      CursorOptions : CursorOptionEnum)
     return GNATCOM.Types.VARIANT_BOOL
   is
       RetVal : aliased GNATCOM.Types.VARIANT_BOOL;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Supports
         (Pointer (This),
          CursorOptions,
          RetVal'Unchecked_Access));

      return RetVal;
   end Supports;

   function Get_Collect
     (This  : uRecordset_Type;
      Index : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
       RetVal : aliased GNATCOM.Types.VARIANT;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Collect
         (Pointer (This),
          Index,
          RetVal'Unchecked_Access));

      if Free then
         GNATCOM.Iinterface.Free (Index);
      end if;

      return RetVal;
   end Get_Collect;

   procedure Put_Collect
     (This  : uRecordset_Type;
      Index : GNATCOM.Types.VARIANT;
      pvar  : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Collect
         (Pointer (This),
          Index,
          pvar));

      if Free then
         GNATCOM.Iinterface.Free (Index);
         GNATCOM.Iinterface.Free (pvar);
      end if;

   end Put_Collect;

   function Get_MarshalOptions
     (This      : uRecordset_Type)
     return MarshalOptionsEnum
   is
       RetVal : aliased MarshalOptionsEnum;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_MarshalOptions
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_MarshalOptions;

   procedure Put_MarshalOptions
     (This      : uRecordset_Type;
      peMarshal : MarshalOptionsEnum)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_MarshalOptions
         (Pointer (This),
          peMarshal));

   end Put_MarshalOptions;

   procedure Find
     (This            : uRecordset_Type;
      Criteria        : GNATCOM.Types.BSTR;
      SkipRecords     : Interfaces.C.long;
      SearchDirection : SearchDirectionEnum;
      Start           : GNATCOM.Types.VARIANT
         := GNATCOM.Types.VARIANT_MISSING;
      Free            : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Find
         (Pointer (This),
          Criteria,
          SkipRecords,
          SearchDirection,
          Start));

      if Free then
         GNATCOM.Iinterface.Free (Criteria);
         GNATCOM.Iinterface.Free (Start);
      end if;

   end Find;

   procedure Cancel
     (This : uRecordset_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Cancel
         (Pointer (This)));

   end Cancel;

   function Get_DataSource
     (This            : uRecordset_Type)
     return GNATCOM.Types.Pointer_To_IUnknown
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IUnknown;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_DataSource
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_DataSource;

   procedure PutRef_DataSource
     (This            : uRecordset_Type;
      ppunkDataSource : GNATCOM.Types.Pointer_To_IUnknown)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.PutRef_DataSource
         (Pointer (This),
          ppunkDataSource));

   end PutRef_DataSource;

   procedure Save
     (This          : uRecordset_Type;
      FileName      : GNATCOM.Types.BSTR;
      PersistFormat : PersistFormatEnum;
      Free          : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Save
         (Pointer (This),
          FileName,
          PersistFormat));

      if Free then
         GNATCOM.Iinterface.Free (FileName);
      end if;

   end Save;

   function Get_ActiveCommand
     (This  : uRecordset_Type)
     return GNATCOM.Types.Pointer_To_IDispatch
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IDispatch;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_ActiveCommand
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_ActiveCommand;

   procedure Put_StayInSync
     (This         : uRecordset_Type;
      pbStayInSync : GNATCOM.Types.VARIANT_BOOL)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_StayInSync
         (Pointer (This),
          pbStayInSync));

   end Put_StayInSync;

   function Get_StayInSync
     (This         : uRecordset_Type)
     return GNATCOM.Types.VARIANT_BOOL
   is
       RetVal : aliased GNATCOM.Types.VARIANT_BOOL;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_StayInSync
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_StayInSync;

   function GetString
     (This            : uRecordset_Type;
      StringFormat    : StringFormatEnum;
      NumRows         : Interfaces.C.long;
      ColumnDelimeter : GNATCOM.Types.BSTR;
      RowDelimeter    : GNATCOM.Types.BSTR;
      NullExpr        : GNATCOM.Types.BSTR;
      Free            : Boolean := True)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.GetString
         (Pointer (This),
          StringFormat,
          NumRows,
          ColumnDelimeter,
          RowDelimeter,
          NullExpr,
          RetVal'Unchecked_Access));

      if Free then
         GNATCOM.Iinterface.Free (ColumnDelimeter);
         GNATCOM.Iinterface.Free (RowDelimeter);
         GNATCOM.Iinterface.Free (NullExpr);
      end if;

      return RetVal;
   end GetString;

   function Get_DataMember
     (This            : uRecordset_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_DataMember
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_DataMember;

   procedure Put_DataMember
     (This            : uRecordset_Type;
      pbstrDataMember : GNATCOM.Types.BSTR;
      Free            : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_DataMember
         (Pointer (This),
          pbstrDataMember));

      if Free then
         GNATCOM.Iinterface.Free (pbstrDataMember);
      end if;

   end Put_DataMember;

   function CompareBookmarks
     (This      : uRecordset_Type;
      Bookmark1 : GNATCOM.Types.VARIANT;
      Bookmark2 : GNATCOM.Types.VARIANT;
      Free      : Boolean := True)
     return CompareEnum
   is
       RetVal : aliased CompareEnum;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.CompareBookmarks
         (Pointer (This),
          Bookmark1,
          Bookmark2,
          RetVal'Unchecked_Access));

      if Free then
         GNATCOM.Iinterface.Free (Bookmark1);
         GNATCOM.Iinterface.Free (Bookmark2);
      end if;

      return RetVal;
   end CompareBookmarks;

   function Clone
     (This      : uRecordset_Type;
      LockType  : LockTypeEnum)
     return Pointer_To_uRecordset
   is
       RetVal : aliased Pointer_To_uRecordset;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Clone
         (Pointer (This),
          LockType,
          RetVal'Unchecked_Access));

      return RetVal;
   end Clone;

   procedure Resync
     (This          : uRecordset_Type;
      AffectRecords : AffectEnum;
      ResyncValues  : ResyncEnum)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Resync
         (Pointer (This),
          AffectRecords,
          ResyncValues));

   end Resync;

end ADO.uRecordset_Interface;
