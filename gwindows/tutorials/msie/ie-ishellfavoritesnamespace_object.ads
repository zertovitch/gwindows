with GNATCOM.Dispinterface;

package IE.IShellFavoritesNameSpace_Object is

   type IShellFavoritesNameSpace_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure MoveSelectionUp
     (This : IShellFavoritesNameSpace_Type);
   --  method MoveSelectionUp

   procedure MoveSelectionDown
     (This : IShellFavoritesNameSpace_Type);
   --  method MoveSelectionDown

   procedure ResetSort
     (This : IShellFavoritesNameSpace_Type);
   --  method ResetSort

   procedure NewFolder
     (This : IShellFavoritesNameSpace_Type);
   --  method NewFolder

   procedure Synchronize
     (This : IShellFavoritesNameSpace_Type);
   --  method Synchronize

   procedure Import
     (This : IShellFavoritesNameSpace_Type);
   --  method Import

   procedure Export
     (This : IShellFavoritesNameSpace_Type);
   --  method Export

   procedure InvokeContextMenuCommand
     (This       : IShellFavoritesNameSpace_Type;
      strCommand : GNATCOM.Types.VARIANT;
      Free       : Boolean := True);
   --  method InvokeContextMenuCommand

   procedure MoveSelectionTo
     (This : IShellFavoritesNameSpace_Type);
   --  method MoveSelectionTo

   function Get_SubscriptionsEnabled
     (This : IShellFavoritesNameSpace_Type)
     return GNATCOM.Types.VARIANT;
   --  Query to see if subscriptions are enabled

   function CreateSubscriptionForSelection
     (This : IShellFavoritesNameSpace_Type)
     return GNATCOM.Types.VARIANT;
   --  method CreateSubscriptionForSelection

   function DeleteSubscriptionForSelection
     (This : IShellFavoritesNameSpace_Type)
     return GNATCOM.Types.VARIANT;
   --  method DeleteSubscriptionForSelection

   procedure SetRoot
     (This         : IShellFavoritesNameSpace_Type;
      bstrFullPath : GNATCOM.Types.VARIANT;
      Free         : Boolean := True);
   --  old, use put_Root() instead

end IE.IShellFavoritesNameSpace_Object;

