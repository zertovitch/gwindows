with GNATCOM.IInterface;

with GNATCOM.Errors;

package body IE.IShellNameSpace_Interface is

   procedure Initialize (This : in out IShellNameSpace_Type) is
   begin
      Set_IID (This, IID_IShellNameSpace);
   end Initialize;

   function Pointer (This : IShellNameSpace_Type)
     return Pointer_To_IShellNameSpace
   is
   begin
      return To_Pointer_To_IShellNameSpace (Address (This));
   end Pointer;

   procedure Attach (This    : in out IShellNameSpace_Type;
                     Pointer : in     Pointer_To_IShellNameSpace)
   is
   begin
      Attach (This, GNATCOM.IInterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure MoveSelectionUp
     (This : IShellNameSpace_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveSelectionUp
         (Pointer (This)));

   end MoveSelectionUp;

   procedure MoveSelectionDown
     (This : IShellNameSpace_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveSelectionDown
         (Pointer (This)));

   end MoveSelectionDown;

   procedure ResetSort
     (This : IShellNameSpace_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ResetSort
         (Pointer (This)));

   end ResetSort;

   procedure NewFolder
     (This : IShellNameSpace_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.NewFolder
         (Pointer (This)));

   end NewFolder;

   procedure Synchronize
     (This : IShellNameSpace_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Synchronize
         (Pointer (This)));

   end Synchronize;

   procedure Import
     (This : IShellNameSpace_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Import
         (Pointer (This)));

   end Import;

   procedure Export
     (This : IShellNameSpace_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Export
         (Pointer (This)));

   end Export;

   procedure InvokeContextMenuCommand
     (This       : IShellNameSpace_Type;
      strCommand : GNATCOM.Types.BSTR;
      Free       : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.InvokeContextMenuCommand
         (Pointer (This),
          strCommand));

      if Free then
               GNATCOM.IInterface.Free (strCommand);
      
      end if;

   end InvokeContextMenuCommand;

   procedure MoveSelectionTo
     (This : IShellNameSpace_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveSelectionTo
         (Pointer (This)));

   end MoveSelectionTo;

   function Get_SubscriptionsEnabled
     (This  : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT_BOOL
   is
       RetVal : aliased GNATCOM.Types.VARIANT_BOOL;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_SubscriptionsEnabled
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_SubscriptionsEnabled;

   function CreateSubscriptionForSelection
     (This  : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT_BOOL
   is
       RetVal : aliased GNATCOM.Types.VARIANT_BOOL;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.CreateSubscriptionForSelection
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end CreateSubscriptionForSelection;

   function DeleteSubscriptionForSelection
     (This  : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT_BOOL
   is
       RetVal : aliased GNATCOM.Types.VARIANT_BOOL;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.DeleteSubscriptionForSelection
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end DeleteSubscriptionForSelection;

   procedure SetRoot
     (This         : IShellNameSpace_Type;
      bstrFullPath : GNATCOM.Types.BSTR;
      Free         : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetRoot
         (Pointer (This),
          bstrFullPath));

      if Free then
               GNATCOM.IInterface.Free (bstrFullPath);
      
      end if;

   end SetRoot;

   function Get_EnumOptions
     (This          : IShellNameSpace_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_EnumOptions
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_EnumOptions;

   procedure Put_EnumOptions
     (This          : IShellNameSpace_Type;
      pgrfEnumFlags : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_EnumOptions
         (Pointer (This),
          pgrfEnumFlags));

   end Put_EnumOptions;

   function Get_SelectedItem
     (This  : IShellNameSpace_Type)
     return GNATCOM.Types.Pointer_To_IDispatch
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IDispatch;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_SelectedItem
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_SelectedItem;

   procedure Put_SelectedItem
     (This  : IShellNameSpace_Type;
      pItem : GNATCOM.Types.Pointer_To_IDispatch)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_SelectedItem
         (Pointer (This),
          pItem));

   end Put_SelectedItem;

   function Get_Root
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT
   is
       RetVal : aliased GNATCOM.Types.VARIANT;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Root
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Root;

   procedure Put_Root
     (This : IShellNameSpace_Type;
      pvar : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Root
         (Pointer (This),
          pvar));

      if Free then
               GNATCOM.IInterface.Free (pvar);
      
      end if;

   end Put_Root;

   function Get_Depth
     (This    : IShellNameSpace_Type)
     return Interfaces.C.int
   is
       RetVal : aliased Interfaces.C.int;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Depth
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Depth;

   procedure Put_Depth
     (This    : IShellNameSpace_Type;
      piDepth : Interfaces.C.int)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Depth
         (Pointer (This),
          piDepth));

   end Put_Depth;

   function Get_Mode
     (This   : IShellNameSpace_Type)
     return Interfaces.C.unsigned
   is
       RetVal : aliased Interfaces.C.unsigned;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Mode
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Mode;

   procedure Put_Mode
     (This   : IShellNameSpace_Type;
      puMode : Interfaces.C.unsigned)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Mode
         (Pointer (This),
          puMode));

   end Put_Mode;

   function Get_Flags
     (This     : IShellNameSpace_Type)
     return Interfaces.C.unsigned_long
   is
       RetVal : aliased Interfaces.C.unsigned_long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Flags
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Flags;

   procedure Put_Flags
     (This     : IShellNameSpace_Type;
      pdwFlags : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Flags
         (Pointer (This),
          pdwFlags));

   end Put_Flags;

   procedure Put_TVFlags
     (This    : IShellNameSpace_Type;
      dwFlags : Interfaces.C.unsigned_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_TVFlags
         (Pointer (This),
          dwFlags));

   end Put_TVFlags;

   function Get_TVFlags
     (This    : IShellNameSpace_Type)
     return Interfaces.C.unsigned_long
   is
       RetVal : aliased Interfaces.C.unsigned_long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_TVFlags
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_TVFlags;

   function Get_Columns
     (This        : IShellNameSpace_Type)
     return GNATCOM.Types.BSTR
   is
       RetVal : aliased GNATCOM.Types.BSTR;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Columns
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Columns;

   procedure Put_Columns
     (This        : IShellNameSpace_Type;
      bstrColumns : GNATCOM.Types.BSTR;
      Free        : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Put_Columns
         (Pointer (This),
          bstrColumns));

      if Free then
               GNATCOM.IInterface.Free (bstrColumns);
      
      end if;

   end Put_Columns;

   function Get_CountViewTypes
     (This    : IShellNameSpace_Type)
     return Interfaces.C.int
   is
       RetVal : aliased Interfaces.C.int;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_CountViewTypes
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_CountViewTypes;

   procedure SetViewType
     (This  : IShellNameSpace_Type;
      iType : Interfaces.C.int)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetViewType
         (Pointer (This),
          iType));

   end SetViewType;

   function SelectedItems
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.Pointer_To_IDispatch
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IDispatch;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SelectedItems
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end SelectedItems;

   procedure Expand
     (This   : IShellNameSpace_Type;
      var    : GNATCOM.Types.VARIANT;
      iDepth : Interfaces.C.int;
      Free   : Boolean := True)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Expand
         (Pointer (This),
          var,
          iDepth));

      if Free then
               GNATCOM.IInterface.Free (var);
      
      end if;

   end Expand;

   procedure UnselectAll
     (This : IShellNameSpace_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.UnselectAll
         (Pointer (This)));

   end UnselectAll;

end IE.IShellNameSpace_Interface;

