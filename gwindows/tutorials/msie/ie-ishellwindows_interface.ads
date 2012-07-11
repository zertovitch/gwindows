with GNATCOM.Dispinterface;

package IE.IShellWindows_Interface is

   type IShellWindows_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   procedure Initialize (This : in out IShellWindows_Type);

   function Pointer (This : IShellWindows_Type)
     return Pointer_To_IShellWindows;

   procedure Attach (This    : in out IShellWindows_Type;
                     Pointer : in     Pointer_To_IShellWindows);

   function Get_Count
     (This  : IShellWindows_Type)
     return Interfaces.C.long;
   --  Get count of open Shell windows

   function Item
     (This   : IShellWindows_Type;
      index  : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Free   : Boolean := True)
     return GNATCOM.Types.Pointer_To_IDispatch;
   --  Return the shell window for the given index

   function uNewEnum
     (This  : IShellWindows_Type)
     return GNATCOM.Types.Pointer_To_IUnknown;
   --  Enumerates the figures

   procedure Register
     (This     : IShellWindows_Type;
      pid      : GNATCOM.Types.Pointer_To_IDispatch;
      HWND     : Interfaces.C.long;
      swClass  : Interfaces.C.int;
      plCookie : GNATCOM.Types.Pointer_To_long);
   --  Register a window with the list

   procedure RegisterPending
     (This        : IShellWindows_Type;
      lThreadId   : Interfaces.C.long;
      pvarloc     : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      pvarlocRoot : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      swClass     : Interfaces.C.int;
      plCookie    : GNATCOM.Types.Pointer_To_long);
   --  Register a pending open with the list

   procedure Revoke
     (This    : IShellWindows_Type;
      lCookie : Interfaces.C.long);
   --  Remove a window from the list

   procedure OnNavigate
     (This    : IShellWindows_Type;
      lCookie : Interfaces.C.long;
      pvarloc : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING);
   --  Notifies the new location

   procedure OnActivated
     (This    : IShellWindows_Type;
      lCookie : Interfaces.C.long;
      fActive : GNATCOM.Types.VARIANT_BOOL);
   --  Notifies the activation

   function FindWindowSW
     (This        : IShellWindows_Type;
      pvarloc     : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      pvarlocRoot : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      swClass     : Interfaces.C.int;
      pHWND       : GNATCOM.Types.Pointer_To_long;
      swfwOptions : Interfaces.C.int)
     return GNATCOM.Types.Pointer_To_IDispatch;
   --  Find the window based on the location

   procedure OnCreated
     (This    : IShellWindows_Type;
      lCookie : Interfaces.C.long;
      punk    : GNATCOM.Types.Pointer_To_IUnknown);
   --  Notifies on creation and frame name set

   procedure ProcessAttachDetach
     (This    : IShellWindows_Type;
      fAttach : GNATCOM.Types.VARIANT_BOOL);
   --  Used by IExplore to register different processes

end IE.IShellWindows_Interface;

