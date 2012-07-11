package body IE.IShellWindows_Object is

   function Get_Count
     (This : IShellWindows_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Get (This, IShellWindows_Get_Count);
   end Get_Count;

   function Item
     (This  : IShellWindows_Type;
      index : GNATCOM.Types.VARIANT
        := GNATCOM.Types.VARIANT_MISSING;
      Free  : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         IShellWindows_Item,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => index),
         Free);
   end Item;

   function uNewEnum
     (This : IShellWindows_Type)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke (This, IShellWindows_uNewEnum);
   end uNewEnum;

   procedure Register
     (This     : IShellWindows_Type;
      pid      : GNATCOM.Types.VARIANT;
      HWND     : GNATCOM.Types.VARIANT;
      swClass  : GNATCOM.Types.VARIANT;
      plCookie : GNATCOM.Types.VARIANT;
      Free     : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellWindows_Register,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => plCookie,
          2 => swClass,
          3 => HWND,
          4 => pid),
         Free);
   end Register;

   procedure RegisterPending
     (This        : IShellWindows_Type;
      lThreadId   : GNATCOM.Types.VARIANT;
      pvarloc     : GNATCOM.Types.VARIANT;
      pvarlocRoot : GNATCOM.Types.VARIANT;
      swClass     : GNATCOM.Types.VARIANT;
      plCookie    : GNATCOM.Types.VARIANT;
      Free        : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellWindows_RegisterPending,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => plCookie,
          2 => swClass,
          3 => pvarlocRoot,
          4 => pvarloc,
          5 => lThreadId),
         Free);
   end RegisterPending;

   procedure Revoke
     (This    : IShellWindows_Type;
      lCookie : GNATCOM.Types.VARIANT;
      Free    : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellWindows_Revoke,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => lCookie),
         Free);
   end Revoke;

   procedure OnNavigate
     (This    : IShellWindows_Type;
      lCookie : GNATCOM.Types.VARIANT;
      pvarloc : GNATCOM.Types.VARIANT;
      Free    : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellWindows_OnNavigate,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => pvarloc,
          2 => lCookie),
         Free);
   end OnNavigate;

   procedure OnActivated
     (This    : IShellWindows_Type;
      lCookie : GNATCOM.Types.VARIANT;
      fActive : GNATCOM.Types.VARIANT;
      Free    : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellWindows_OnActivated,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => fActive,
          2 => lCookie),
         Free);
   end OnActivated;

   function FindWindowSW
     (This        : IShellWindows_Type;
      pvarloc     : GNATCOM.Types.VARIANT;
      pvarlocRoot : GNATCOM.Types.VARIANT;
      swClass     : GNATCOM.Types.VARIANT;
      pHWND       : GNATCOM.Types.VARIANT;
      swfwOptions : GNATCOM.Types.VARIANT;
      Free        : Boolean := True)
     return GNATCOM.Types.VARIANT
   is
   begin
      return Invoke
        (This,
         IShellWindows_FindWindowSW,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => swfwOptions,
          2 => pHWND,
          3 => swClass,
          4 => pvarlocRoot,
          5 => pvarloc),
         Free);
   end FindWindowSW;

   procedure OnCreated
     (This    : IShellWindows_Type;
      lCookie : GNATCOM.Types.VARIANT;
      punk    : GNATCOM.Types.VARIANT;
      Free    : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellWindows_OnCreated,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => punk,
          2 => lCookie),
         Free);
   end OnCreated;

   procedure ProcessAttachDetach
     (This    : IShellWindows_Type;
      fAttach : GNATCOM.Types.VARIANT;
      Free    : Boolean := True)
   is
   begin
      Invoke
        (This,
         IShellWindows_ProcessAttachDetach,
         GNATCOM.Dispinterface.Parameter_Array'
         (1 => fAttach),
         Free);
   end ProcessAttachDetach;

end IE.IShellWindows_Object;

