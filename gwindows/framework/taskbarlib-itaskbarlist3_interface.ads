with GNATCOM.Iinterface;

package TaskbarLib.ITaskbarList3_Interface is

   type ITaskbarList3_Type is
     new GNATCOM.Iinterface.Interface_Type with null record;

   procedure Initialize (This : in out ITaskbarList3_Type);

   function Pointer (This : ITaskbarList3_Type)
     return Pointer_To_ITaskbarList3;

   procedure Attach (This    : in out ITaskbarList3_Type;
                     Pointer : in     Pointer_To_ITaskbarList3);

   procedure HrInit
     (This : ITaskbarList3_Type);

   procedure AddTab
     (This : ITaskbarList3_Type;
      hwnd : Interfaces.C.long);

   procedure DeleteTab
     (This : ITaskbarList3_Type;
      hwnd : Interfaces.C.long);

   procedure ActivateTab
     (This : ITaskbarList3_Type;
      hwnd : Interfaces.C.long);

   procedure SetActivateAlt
     (This : ITaskbarList3_Type;
      hwnd : Interfaces.C.long);

   procedure MarkFullscreenWindow
     (This        : ITaskbarList3_Type;
      hwnd        : Interfaces.C.long;
      fFullscreen : Interfaces.C.long);

   procedure SetProgressValue
     (This         : ITaskbarList3_Type;
      hwnd         : Interfaces.C.long;
      ullCompleted : GNATCOM.Types.DWORDLONG;
      ullTotal     : GNATCOM.Types.DWORDLONG);

   procedure SetProgressState
     (This     : ITaskbarList3_Type;
      hwnd     : Interfaces.C.long;
      tbpFlags : TBPFLAG);

   procedure RegisterTab
     (This    : ITaskbarList3_Type;
      hwndTab : Interfaces.C.long;
      hwndMDI : wireHWND);

   procedure UnregisterTab
     (This    : ITaskbarList3_Type;
      hwndTab : Interfaces.C.long);

   procedure SetTabOrder
     (This             : ITaskbarList3_Type;
      hwndTab          : Interfaces.C.long;
      hwndInsertBefore : Interfaces.C.long);

   procedure SetTabActive
     (This      : ITaskbarList3_Type;
      hwndTab   : Interfaces.C.long;
      hwndMDI   : Interfaces.C.long;
      tbatFlags : TBATFLAG);

   procedure ThumbBarAddButtons
     (This     : ITaskbarList3_Type;
      hwnd     : Interfaces.C.long;
      cButtons : Interfaces.C.unsigned;
      pButton  : Pointer_To_tagTHUMBBUTTON);

   procedure ThumbBarUpdateButtons
     (This     : ITaskbarList3_Type;
      hwnd     : Interfaces.C.long;
      cButtons : Interfaces.C.unsigned;
      pButton  : Pointer_To_tagTHUMBBUTTON);

   procedure ThumbBarSetImageList
     (This : ITaskbarList3_Type;
      hwnd : Interfaces.C.long;
      himl : GNATCOM.Types.Pointer_To_IUnknown);

   procedure SetOverlayIcon
     (This           : ITaskbarList3_Type;
      hwnd           : Interfaces.C.long;
      hIcon          : GNATCOM.Types.Pointer_To_IUnknown;
      pszDescription : GNATCOM.Types.LPWSTR);

   procedure SetThumbnailTooltip
     (This   : ITaskbarList3_Type;
      hwnd   : Interfaces.C.long;
      pszTip : GNATCOM.Types.LPWSTR);

   procedure SetThumbnailClip
     (This    : ITaskbarList3_Type;
      hwnd    : Interfaces.C.long;
      prcClip : Pointer_To_tagRECT);

end TaskbarLib.ITaskbarList3_Interface;
