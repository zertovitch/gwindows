with GNATCOM.Errors;

package body TaskbarLib.ITaskbarList3_Interface is

   procedure Initialize (This : in out ITaskbarList3_Type) is
   begin
      Set_IID (This, IID_ITaskbarList3);
   end Initialize;

   function Pointer (This : ITaskbarList3_Type)
     return Pointer_To_ITaskbarList3
   is
   begin
      return To_Pointer_To_ITaskbarList3 (Address (This));
   end Pointer;

   procedure Attach (This    : in out ITaskbarList3_Type;
                     Pointer : in     Pointer_To_ITaskbarList3)
   is
   begin
      Attach (This, GNATCOM.Iinterface.To_Pointer_To_IUnknown
              (Pointer.all'Address));
   end Attach;

   procedure HrInit
     (This : ITaskbarList3_Type)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.HrInit
         (Pointer (This)));

   end HrInit;

   procedure AddTab
     (This : ITaskbarList3_Type;
      hwnd : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.AddTab
         (Pointer (This),
          hwnd));

   end AddTab;

   procedure DeleteTab
     (This : ITaskbarList3_Type;
      hwnd : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.DeleteTab
         (Pointer (This),
          hwnd));

   end DeleteTab;

   procedure ActivateTab
     (This : ITaskbarList3_Type;
      hwnd : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ActivateTab
         (Pointer (This),
          hwnd));

   end ActivateTab;

   procedure SetActivateAlt
     (This : ITaskbarList3_Type;
      hwnd : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetActivateAlt
         (Pointer (This),
          hwnd));

   end SetActivateAlt;

   procedure MarkFullscreenWindow
     (This        : ITaskbarList3_Type;
      hwnd        : Interfaces.C.long;
      fFullscreen : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.MarkFullscreenWindow
         (Pointer (This),
          hwnd,
          fFullscreen));

   end MarkFullscreenWindow;

   procedure SetProgressValue
     (This         : ITaskbarList3_Type;
      hwnd         : Interfaces.C.long;
      ullCompleted : GNATCOM.Types.DWORDLONG;
      ullTotal     : GNATCOM.Types.DWORDLONG)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetProgressValue
         (Pointer (This),
          hwnd,
          ullCompleted,
          ullTotal));

   end SetProgressValue;

   procedure SetProgressState
     (This     : ITaskbarList3_Type;
      hwnd     : Interfaces.C.long;
      tbpFlags : TBPFLAG)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetProgressState
         (Pointer (This),
          hwnd,
          tbpFlags));

   end SetProgressState;

   procedure RegisterTab
     (This    : ITaskbarList3_Type;
      hwndTab : Interfaces.C.long;
      hwndMDI : wireHWND)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.RegisterTab
         (Pointer (This),
          hwndTab,
          hwndMDI));

   end RegisterTab;

   procedure UnregisterTab
     (This    : ITaskbarList3_Type;
      hwndTab : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.UnregisterTab
         (Pointer (This),
          hwndTab));

   end UnregisterTab;

   procedure SetTabOrder
     (This             : ITaskbarList3_Type;
      hwndTab          : Interfaces.C.long;
      hwndInsertBefore : Interfaces.C.long)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetTabOrder
         (Pointer (This),
          hwndTab,
          hwndInsertBefore));

   end SetTabOrder;

   procedure SetTabActive
     (This      : ITaskbarList3_Type;
      hwndTab   : Interfaces.C.long;
      hwndMDI   : Interfaces.C.long;
      tbatFlags : TBATFLAG)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetTabActive
         (Pointer (This),
          hwndTab,
          hwndMDI,
          tbatFlags));

   end SetTabActive;

   procedure ThumbBarAddButtons
     (This     : ITaskbarList3_Type;
      hwnd     : Interfaces.C.long;
      cButtons : Interfaces.C.unsigned;
      pButton  : Pointer_To_tagTHUMBBUTTON)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ThumbBarAddButtons
         (Pointer (This),
          hwnd,
          cButtons,
          pButton));

   end ThumbBarAddButtons;

   procedure ThumbBarUpdateButtons
     (This     : ITaskbarList3_Type;
      hwnd     : Interfaces.C.long;
      cButtons : Interfaces.C.unsigned;
      pButton  : Pointer_To_tagTHUMBBUTTON)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ThumbBarUpdateButtons
         (Pointer (This),
          hwnd,
          cButtons,
          pButton));

   end ThumbBarUpdateButtons;

   procedure ThumbBarSetImageList
     (This : ITaskbarList3_Type;
      hwnd : Interfaces.C.long;
      himl : GNATCOM.Types.Pointer_To_IUnknown)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.ThumbBarSetImageList
         (Pointer (This),
          hwnd,
          himl));

   end ThumbBarSetImageList;

   procedure SetOverlayIcon
     (This           : ITaskbarList3_Type;
      hwnd           : Interfaces.C.long;
      hIcon          : GNATCOM.Types.Pointer_To_IUnknown;
      pszDescription : GNATCOM.Types.LPWSTR)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetOverlayIcon
         (Pointer (This),
          hwnd,
          hIcon,
          pszDescription));

   end SetOverlayIcon;

   procedure SetThumbnailTooltip
     (This   : ITaskbarList3_Type;
      hwnd   : Interfaces.C.long;
      pszTip : GNATCOM.Types.LPWSTR)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetThumbnailTooltip
         (Pointer (This),
          hwnd,
          pszTip));

   end SetThumbnailTooltip;

   procedure SetThumbnailClip
     (This    : ITaskbarList3_Type;
      hwnd    : Interfaces.C.long;
      prcClip : Pointer_To_tagRECT)
   is
   begin
      GNATCOM.Errors.Error_Check
        (Pointer (This).Vtbl.SetThumbnailClip
         (Pointer (This),
          hwnd,
          prcClip));

   end SetThumbnailClip;

end TaskbarLib.ITaskbarList3_Interface;
