package body IE.IShellFavoritesNameSpace_Object is

   procedure MoveSelectionUp
     (This : IShellFavoritesNameSpace_Type)
   is
   begin
      Invoke (This, IShellFavoritesNameSpace_MoveSelectionUp);
   end MoveSelectionUp;

   procedure MoveSelectionDown
     (This : IShellFavoritesNameSpace_Type)
   is
   begin
      Invoke (This, IShellFavoritesNameSpace_MoveSelectionDown);
   end MoveSelectionDown;

   procedure ResetSort
     (This : IShellFavoritesNameSpace_Type)
   is
   begin
      Invoke (This, IShellFavoritesNameSpace_ResetSort);
   end ResetSort;

   procedure NewFolder
     (This : IShellFavoritesNameSpace_Type)
   is
   begin
      Invoke (This, IShellFavoritesNameSpace_NewFolder);
   end NewFolder;

   procedure Synchronize
     (This : IShellFavoritesNameSpace_Type)
   is
   begin
      Invoke (This, IShellFavoritesNameSpace_Synchronize);
   end Synchronize;

   procedure Import
     (This : IShellFavoritesNameSpace_Type)
   is
   begin
      Invoke (This, IShellFavoritesNameSpace_Import);
   end Import;

   procedure Export
     (This : IShellFavoritesNameSpace_Type)
   is
   begin
      Invoke (This, IShellFavoritesNameSpace_Export);
   end Export;

   procedure InvokeContextMenuCommand
     (This       : IShellFavoritesNameSpace_Type;
      strCommand : GNATCOM.Types.VARIANT;
      Free       : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellFavoritesNameSpace_InvokeContextMenuCommand,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => strCommand),
         Free);
   end InvokeContextMenuCommand;

   procedure MoveSelectionTo
     (This : IShellFavoritesNameSpace_Type)
   is
   begin
      Invoke (This, IShellFavoritesNameSpace_MoveSelectionTo);
   end MoveSelectionTo;

   function Get_SubscriptionsEnabled
     (This : IShellFavoritesNameSpace_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IShellFavoritesNameSpace_Get_SubscriptionsEnabled);
   end Get_SubscriptionsEnabled;

   function CreateSubscriptionForSelection
     (This : IShellFavoritesNameSpace_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellFavoritesNameSpace_CreateSubscriptionForSelection);
   end CreateSubscriptionForSelection;

   function DeleteSubscriptionForSelection
     (This : IShellFavoritesNameSpace_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellFavoritesNameSpace_DeleteSubscriptionForSelection);
   end DeleteSubscriptionForSelection;

   procedure SetRoot
     (This         : IShellFavoritesNameSpace_Type;
      bstrFullPath : GNATCOM.Types.VARIANT;
      Free         : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellFavoritesNameSpace_SetRoot,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => bstrFullPath),
         Free);
   end SetRoot;

end IE.IShellFavoritesNameSpace_Object;

