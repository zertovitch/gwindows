with GNATCOM.Dispinterface;

package IE.IShellNameSpace_Interface is

   type IShellNameSpace_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out IShellNameSpace_Type);

   function Pointer (This : IShellNameSpace_Type)
     return Pointer_To_IShellNameSpace;

   procedure Attach (This    : in out IShellNameSpace_Type;
                     Pointer : in     Pointer_To_IShellNameSpace);

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
      strCommand : GNATCOM.Types.BSTR;
      Free       : Boolean := True);
   --  method InvokeContextMenuCommand

   procedure MoveSelectionTo
     (This : IShellNameSpace_Type);
   --  method MoveSelectionTo

   function Get_SubscriptionsEnabled
     (This  : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT_BOOL;
   --  Query to see if subscriptions are enabled

   function CreateSubscriptionForSelection
     (This  : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT_BOOL;
   --  method CreateSubscriptionForSelection

   function DeleteSubscriptionForSelection
     (This  : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT_BOOL;
   --  method DeleteSubscriptionForSelection

   procedure SetRoot
     (This         : IShellNameSpace_Type;
      bstrFullPath : GNATCOM.Types.BSTR;
      Free         : Boolean := True);
   --  old, use put_Root() instead

   function Get_EnumOptions
     (This          : IShellNameSpace_Type)
     return Interfaces.C.long;
   --  options 

   procedure Put_EnumOptions
     (This          : IShellNameSpace_Type;
      pgrfEnumFlags : Interfaces.C.long);
   --  options 

   function Get_SelectedItem
     (This  : IShellNameSpace_Type)
     return GNATCOM.Types.Pointer_To_IDispatch;
   --  get the selected item

   procedure Put_SelectedItem
     (This  : IShellNameSpace_Type;
      pItem : GNATCOM.Types.Pointer_To_IDispatch);
   --  get the selected item

   function Get_Root
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.VARIANT;
   --  get the root item

   procedure Put_Root
     (This : IShellNameSpace_Type;
      pvar : GNATCOM.Types.VARIANT;
      Free : Boolean := True);
   --  get the root item

   function Get_Depth
     (This    : IShellNameSpace_Type)
     return Interfaces.C.int;

   procedure Put_Depth
     (This    : IShellNameSpace_Type;
      piDepth : Interfaces.C.int);

   function Get_Mode
     (This   : IShellNameSpace_Type)
     return Interfaces.C.unsigned;

   procedure Put_Mode
     (This   : IShellNameSpace_Type;
      puMode : Interfaces.C.unsigned);

   function Get_Flags
     (This     : IShellNameSpace_Type)
     return Interfaces.C.unsigned_long;

   procedure Put_Flags
     (This     : IShellNameSpace_Type;
      pdwFlags : Interfaces.C.unsigned_long);

   procedure Put_TVFlags
     (This    : IShellNameSpace_Type;
      dwFlags : Interfaces.C.unsigned_long);

   function Get_TVFlags
     (This    : IShellNameSpace_Type)
     return Interfaces.C.unsigned_long;

   function Get_Columns
     (This        : IShellNameSpace_Type)
     return GNATCOM.Types.BSTR;

   procedure Put_Columns
     (This        : IShellNameSpace_Type;
      bstrColumns : GNATCOM.Types.BSTR;
      Free        : Boolean := True);

   function Get_CountViewTypes
     (This    : IShellNameSpace_Type)
     return Interfaces.C.int;
   --  number of view types

   procedure SetViewType
     (This  : IShellNameSpace_Type;
      iType : Interfaces.C.int);
   --  set view type

   function SelectedItems
     (This : IShellNameSpace_Type)
     return GNATCOM.Types.Pointer_To_IDispatch;
   --  collection of selected items

   procedure Expand
     (This   : IShellNameSpace_Type;
      var    : GNATCOM.Types.VARIANT;
      iDepth : Interfaces.C.int;
      Free   : Boolean := True);
   --  expands item specified depth

   procedure UnselectAll
     (This : IShellNameSpace_Type);
   --  unselects all items

end IE.IShellNameSpace_Interface;

