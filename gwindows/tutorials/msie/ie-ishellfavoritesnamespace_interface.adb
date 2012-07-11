with GNATCOM.IInterface;

with GNATCOM.Errors;

package body IE.IShellFavoritesNameSpace_Interface is

   procedure Initialize (This : in out IShellFavoritesNameSpace_Type) is
   begin
      Set_IID (This, IID_IShellFavoritesNameSpace);
   end Initialize;

   function Pointer (This : IShellFavoritesNameSpace_Type)
     return Pointer_To_IShellFavoritesNameSpace
   is
   begin
      return To_Pointer_To_IShellFavoritesNameSpace (Address (This));
   end Pointer;

   procedure Attach (This    : in out IShellFavoritesNameSpace_Type;
                     Pointer : in     Pointer_To_IShellFavoritesNameSpace)
   is
   begin
      Attach (This, GNATCOM.IInterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure MoveSelectionUp
     (This : IShellFavoritesNameSpace_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveSelectionUp
         (Pointer (This)));

   end MoveSelectionUp;

   procedure MoveSelectionDown
     (This : IShellFavoritesNameSpace_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveSelectionDown
         (Pointer (This)));

   end MoveSelectionDown;

   procedure ResetSort
     (This : IShellFavoritesNameSpace_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ResetSort
         (Pointer (This)));

   end ResetSort;

   procedure NewFolder
     (This : IShellFavoritesNameSpace_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.NewFolder
         (Pointer (This)));

   end NewFolder;

   procedure Synchronize
     (This : IShellFavoritesNameSpace_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Synchronize
         (Pointer (This)));

   end Synchronize;

   procedure Import
     (This : IShellFavoritesNameSpace_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Import
         (Pointer (This)));

   end Import;

   procedure Export
     (This : IShellFavoritesNameSpace_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Export
         (Pointer (This)));

   end Export;

   procedure InvokeContextMenuCommand
     (This       : IShellFavoritesNameSpace_Type;
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
     (This : IShellFavoritesNameSpace_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MoveSelectionTo
         (Pointer (This)));

   end MoveSelectionTo;

   function Get_SubscriptionsEnabled
     (This  : IShellFavoritesNameSpace_Type)
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
     (This  : IShellFavoritesNameSpace_Type)
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
     (This  : IShellFavoritesNameSpace_Type)
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
     (This         : IShellFavoritesNameSpace_Type;
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

end IE.IShellFavoritesNameSpace_Interface;

