with GNATCOM.IInterface;

with GNATCOM.Errors;

package body IE.IShellWindows_Interface is

   procedure Initialize (This : in out IShellWindows_Type) is
   begin
      Set_IID (This, IID_IShellWindows);
   end Initialize;

   function Pointer (This : IShellWindows_Type)
     return Pointer_To_IShellWindows
   is
   begin
      return To_Pointer_To_IShellWindows (Address (This));
   end Pointer;

   procedure Attach (This    : in out IShellWindows_Type;
                     Pointer : in     Pointer_To_IShellWindows)
   is
   begin
      Attach (This, GNATCOM.IInterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   function Get_Count
     (This  : IShellWindows_Type)
     return Interfaces.C.long
   is
       RetVal : aliased Interfaces.C.long;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Get_Count
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end Get_Count;

   function Item
     (This   : IShellWindows_Type;
      index  : GNATCOM.Types.VARIANT  := GNATCOM.Types.VARIANT_MISSING;
      Free   : Boolean := True)
     return GNATCOM.Types.Pointer_To_IDispatch
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IDispatch;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Item
         (Pointer (This),
          index,
          RetVal'Unchecked_Access));

      if Free then
               GNATCOM.IInterface.Free (index);
      
      end if;

      return RetVal;
   end Item;

   function uNewEnum
     (This  : IShellWindows_Type)
     return GNATCOM.Types.Pointer_To_IUnknown
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IUnknown;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.uNewEnum
         (Pointer (This),
          RetVal'Unchecked_Access));

      return RetVal;
   end uNewEnum;

   procedure Register
     (This     : IShellWindows_Type;
      pid      : GNATCOM.Types.Pointer_To_IDispatch;
      HWND     : Interfaces.C.long;
      swClass  : Interfaces.C.int;
      plCookie : GNATCOM.Types.Pointer_To_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Register
         (Pointer (This),
          pid,
          HWND,
          swClass,
          plCookie));

   end Register;

   procedure RegisterPending
     (This        : IShellWindows_Type;
      lThreadId   : Interfaces.C.long;
      pvarloc     : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      pvarlocRoot : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      swClass     : Interfaces.C.int;
      plCookie    : GNATCOM.Types.Pointer_To_long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RegisterPending
         (Pointer (This),
          lThreadId,
          pvarloc,
          pvarlocRoot,
          swClass,
          plCookie));

   end RegisterPending;

   procedure Revoke
     (This    : IShellWindows_Type;
      lCookie : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.Revoke
         (Pointer (This),
          lCookie));

   end Revoke;

   procedure OnNavigate
     (This    : IShellWindows_Type;
      lCookie : Interfaces.C.long;
      pvarloc : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.OnNavigate
         (Pointer (This),
          lCookie,
          pvarloc));

   end OnNavigate;

   procedure OnActivated
     (This    : IShellWindows_Type;
      lCookie : Interfaces.C.long;
      fActive : GNATCOM.Types.VARIANT_BOOL)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.OnActivated
         (Pointer (This),
          lCookie,
          fActive));

   end OnActivated;

   function FindWindowSW
     (This        : IShellWindows_Type;
      pvarloc     : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      pvarlocRoot : access GNATCOM.Types.VARIANT :=
        GNATCOM.Types.PVARIANT_MISSING;
      swClass     : Interfaces.C.int;
      pHWND       : GNATCOM.Types.Pointer_To_long;
      swfwOptions : Interfaces.C.int)
     return GNATCOM.Types.Pointer_To_IDispatch
   is
       RetVal : aliased GNATCOM.Types.Pointer_To_IDispatch;
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.FindWindowSW
         (Pointer (This),
          pvarloc,
          pvarlocRoot,
          swClass,
          pHWND,
          swfwOptions,
          RetVal'Unchecked_Access));

      return RetVal;
   end FindWindowSW;

   procedure OnCreated
     (This    : IShellWindows_Type;
      lCookie : Interfaces.C.long;
      punk    : GNATCOM.Types.Pointer_To_IUnknown)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.OnCreated
         (Pointer (This),
          lCookie,
          punk));

   end OnCreated;

   procedure ProcessAttachDetach
     (This    : IShellWindows_Type;
      fAttach : GNATCOM.Types.VARIANT_BOOL)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ProcessAttachDetach
         (Pointer (This),
          fAttach));

   end ProcessAttachDetach;

end IE.IShellWindows_Interface;

