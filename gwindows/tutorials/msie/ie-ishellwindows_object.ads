with GNATCOM.Dispinterface;

package IE.IShellWindows_Object is

   type IShellWindows_Type is
     new GNATCOM.Dispinterface.Dispinterface_Type with null record;

   function Get_Count
     (This : IShellWindows_Type)
     return GNATCOM.Types.VARIANT;
   --  Get count of open Shell windows

   function Item
     (This  : IShellWindows_Type;
      index : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT;
   --  Return the shell window for the given index

   function uNewEnum
     (This : IShellWindows_Type)
     return GNATCOM.Types.VARIANT;
   --  Enumerates the figures

   procedure Register
     (This     : IShellWindows_Type;
      pid      : GNATCOM.Types.VARIANT;
      HWND     : GNATCOM.Types.VARIANT;
      swClass  : GNATCOM.Types.VARIANT;
      plCookie : GNATCOM.Types.VARIANT;
      Free     : Boolean := True);
   --  Register a window with the list

   procedure RegisterPending
     (This        : IShellWindows_Type;
      lThreadId   : GNATCOM.Types.VARIANT;
      pvarloc     : GNATCOM.Types.VARIANT;
      pvarlocRoot : GNATCOM.Types.VARIANT;
      swClass     : GNATCOM.Types.VARIANT;
      plCookie    : GNATCOM.Types.VARIANT;
      Free        : Boolean := True);
   --  Register a pending open with the list

   procedure Revoke
     (This    : IShellWindows_Type;
      lCookie : GNATCOM.Types.VARIANT;
      Free    : Boolean := True);
   --  Remove a window from the list

   procedure OnNavigate
     (This    : IShellWindows_Type;
      lCookie : GNATCOM.Types.VARIANT;
      pvarloc : GNATCOM.Types.VARIANT;
      Free    : Boolean := True);
   --  Notifies the new location

   procedure OnActivated
     (This    : IShellWindows_Type;
      lCookie : GNATCOM.Types.VARIANT;
      fActive : GNATCOM.Types.VARIANT;
      Free    : Boolean := True);
   --  Notifies the activation

   function FindWindowSW
     (This        : IShellWindows_Type;
      pvarloc     : GNATCOM.Types.VARIANT;
      pvarlocRoot : GNATCOM.Types.VARIANT;
      swClass     : GNATCOM.Types.VARIANT;
      pHWND       : GNATCOM.Types.VARIANT;
      swfwOptions : GNATCOM.Types.VARIANT;
      Free        : Boolean := True)
     return GNATCOM.Types.VARIANT;
   --  Find the window based on the location

   procedure OnCreated
     (This    : IShellWindows_Type;
      lCookie : GNATCOM.Types.VARIANT;
      punk    : GNATCOM.Types.VARIANT;
      Free    : Boolean := True);
   --  Notifies on creation and frame name set

   procedure ProcessAttachDetach
     (This    : IShellWindows_Type;
      fAttach : GNATCOM.Types.VARIANT;
      Free    : Boolean := True);
   --  Used by IExplore to register different processes

end IE.IShellWindows_Object;

