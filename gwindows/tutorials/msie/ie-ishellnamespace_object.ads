with GNATCOM.Dispinterface;

package IE.IShellNameSpace_Object is

   type IShellNameSpace_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure MoveSelectionUp
     (This : IShellNameSpace_Type);
   --  method MoveSelectionUp

   procedure MoveSelectionDown
     (This : IShellNameSpace_Type);
   --  method MoveSelectionDown

   procedure ResetSort
     (This : IShellNameSpace_Type);
   --  method ResetSort

   procedure NewFolder
     (This : IShellNameSpace_Type);
   --  method NewFolder

   procedure Synchronize
     (This : IShellNameSpace_Type);
   --  method Synchronize

   procedure Import
     (This : IShellNameSpace_Type);
   --  method Import

   procedure Export
     (This : IShellNameSpace_Type);
   --  method Export

   procedure InvokeContextMenuCommand
     (This       : IShellNameSpace_Type;
      strCommand : GNATCOM.Types.VARIANT;
      Free       : Boolean := True);
   --  method InvokeContextMenuCommand

   procedure MoveSelectionTo
     (This : IShellNameSpace_Type);
   --  method MoveSelectionTo

   function Get_SubscriptionsEnabled
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT;
   --  Query to see if subscriptions are enabled

   function CreateSubscriptionForSelection
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT;
   --  method CreateSubscriptionForSelection

   function DeleteSubscriptionForSelection
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT;
   --  method DeleteSubscriptionForSelection

   procedure SetRoot
     (This         : IShellNameSpace_Type;
      bstrFullPath : GNATCOM.Types.VARIANT;
      Free         : Boolean := True);
   --  old, use put_Root() instead

   function Get_EnumOptions
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT;
   --  options 

   procedure Put_EnumOptions
     (This : IShellNameSpace_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  options 

   function Get_SelectedItem
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT;
   --  get the selected item

   procedure Put_SelectedItem
     (This : IShellNameSpace_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  get the selected item

   function Get_Root
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT;
   --  get the root item

   procedure Put_Root
     (This : IShellNameSpace_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  get the root item

   function Get_Depth
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Depth
     (This : IShellNameSpace_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Mode
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Mode
     (This : IShellNameSpace_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_Flags
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Flags
     (This : IShellNameSpace_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   procedure Put_TVFlags
     (This : IShellNameSpace_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_TVFlags
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT;

   function Get_Columns
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT;

   procedure Put_Columns
     (This : IShellNameSpace_Type;
      P1   : GNATCOM.Types.VARIANT;
      Free : Boolean := True);

   function Get_CountViewTypes
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT;
   --  number of view types

   procedure SetViewType
     (This  : IShellNameSpace_Type;
      iType : GNATCOM.Types.VARIANT;
      Free  : Boolean := True);
   --  set view type

   function SelectedItems
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT;
   --  collection of selected items

   procedure Expand
     (This   : IShellNameSpace_Type;
      var    : GNATCOM.Types.VARIANT;
      iDepth : GNATCOM.Types.VARIANT;
      Free   : Boolean := True);
   --  expands item specified depth

   procedure UnselectAll
     (This : IShellNameSpace_Type);
   --  unselects all items

end IE.IShellNameSpace_Object;

