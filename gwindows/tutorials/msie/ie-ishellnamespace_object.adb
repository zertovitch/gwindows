package body IE.IShellNameSpace_Object is

   procedure MoveSelectionUp
     (This : IShellNameSpace_Type)
   is
   begin
      Invoke (This, IShellNameSpace_MoveSelectionUp);
   end MoveSelectionUp;

   procedure MoveSelectionDown
     (This : IShellNameSpace_Type)
   is
   begin
      Invoke (This, IShellNameSpace_MoveSelectionDown);
   end MoveSelectionDown;

   procedure ResetSort
     (This : IShellNameSpace_Type)
   is
   begin
      Invoke (This, IShellNameSpace_ResetSort);
   end ResetSort;

   procedure NewFolder
     (This : IShellNameSpace_Type)
   is
   begin
      Invoke (This, IShellNameSpace_NewFolder);
   end NewFolder;

   procedure Synchronize
     (This : IShellNameSpace_Type)
   is
   begin
      Invoke (This, IShellNameSpace_Synchronize);
   end Synchronize;

   procedure Import
     (This : IShellNameSpace_Type)
   is
   begin
      Invoke (This, IShellNameSpace_Import);
   end Import;

   procedure Export
     (This : IShellNameSpace_Type)
   is
   begin
      Invoke (This, IShellNameSpace_Export);
   end Export;

   procedure InvokeContextMenuCommand
     (This       : IShellNameSpace_Type;
      strCommand : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellNameSpace_InvokeContextMenuCommand,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => strCommand),
         Free);
   end InvokeContextMenuCommand;

   procedure MoveSelectionTo
     (This : IShellNameSpace_Type)
   is
   begin
      Invoke (This, IShellNameSpace_MoveSelectionTo);
   end MoveSelectionTo;

   function Get_SubscriptionsEnabled
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IShellNameSpace_Get_SubscriptionsEnabled);
   end Get_SubscriptionsEnabled;

   function CreateSubscriptionForSelection
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellNameSpace_CreateSubscriptionForSelection);
   end CreateSubscriptionForSelection;

   function DeleteSubscriptionForSelection
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellNameSpace_DeleteSubscriptionForSelection);
   end DeleteSubscriptionForSelection;

   procedure SetRoot
     (This         : IShellNameSpace_Type;
      bstrFullPath : GNATCOM.Types.VARIANT;
      Free         : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellNameSpace_SetRoot,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => bstrFullPath),
         Free);
   end SetRoot;

   function Get_EnumOptions
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IShellNameSpace_Get_EnumOptions);
   end Get_EnumOptions;

   procedure Put_EnumOptions
     (This : IShellNameSpace_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IShellNameSpace_Put_EnumOptions,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_EnumOptions;

   function Get_SelectedItem
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IShellNameSpace_Get_SelectedItem);
   end Get_SelectedItem;

   procedure Put_SelectedItem
     (This : IShellNameSpace_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IShellNameSpace_Put_SelectedItem,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_SelectedItem;

   function Get_Root
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IShellNameSpace_Get_Root);
   end Get_Root;

   procedure Put_Root
     (This : IShellNameSpace_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IShellNameSpace_Put_Root,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Root;

   function Get_Depth
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IShellNameSpace_Get_Depth);
   end Get_Depth;

   procedure Put_Depth
     (This : IShellNameSpace_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IShellNameSpace_Put_Depth,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Depth;

   function Get_Mode
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IShellNameSpace_Get_Mode);
   end Get_Mode;

   procedure Put_Mode
     (This : IShellNameSpace_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IShellNameSpace_Put_Mode,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Mode;

   function Get_Flags
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IShellNameSpace_Get_Flags);
   end Get_Flags;

   procedure Put_Flags
     (This : IShellNameSpace_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IShellNameSpace_Put_Flags,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Flags;

   procedure Put_TVFlags
     (This : IShellNameSpace_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IShellNameSpace_Put_TVFlags,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_TVFlags;

   function Get_TVFlags
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IShellNameSpace_Get_TVFlags);
   end Get_TVFlags;

   function Get_Columns
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IShellNameSpace_Get_Columns);
   end Get_Columns;

   procedure Put_Columns
     (This : IShellNameSpace_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True)
   is
   begin
      Put
        (This,
         IShellNameSpace_Put_Columns,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => P1),
         Free);
   end Put_Columns;

   function Get_CountViewTypes
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IShellNameSpace_Get_CountViewTypes);
   end Get_CountViewTypes;

   procedure SetViewType
     (This  : IShellNameSpace_Type;
      iType : GNATCOM.Types.VARIANT;
      Free  : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellNameSpace_SetViewType,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => iType),
         Free);
   end SetViewType;

   function SelectedItems
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellNameSpace_SelectedItems);
   end SelectedItems;

   procedure Expand
     (This   : IShellNameSpace_Type;
      var    : GNATCOM.Types.VARIANT;
      iDepth : GNATCOM.Types.VARIANT;
      Free   : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellNameSpace_Expand,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => iDepth,
          2 => var),
         Free);
   end Expand;

   procedure UnselectAll
     (This : IShellNameSpace_Type)
   is
   begin
      Invoke (This, IShellNameSpace_UnselectAll);
   end UnselectAll;

end IE.IShellNameSpace_Object;

