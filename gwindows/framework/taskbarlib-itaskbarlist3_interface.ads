with GNATCOM.Iinterface;
with Win32_Types;

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
      hwnd : Win32_Types.Long);

   procedure DeleteTab
     (This : ITaskbarList3_Type;
      hwnd : Win32_Types.Long);

   procedure ActivateTab
     (This : ITaskbarList3_Type;
      hwnd : Win32_Types.Long);

   procedure SetActivateAlt
     (This : ITaskbarList3_Type;
      hwnd : Win32_Types.Long);

   procedure MarkFullscreenWindow
     (This        : ITaskbarList3_Type;
      hwnd        : Win32_Types.Long;
      fFullscreen : Win32_Types.Long);

   procedure SetProgressValue
     (This         : ITaskbarList3_Type;
      hwnd         : Win32_Types.Long;
      ullCompleted : GNATCOM.Types.DWORDLONG;
      ullTotal     : GNATCOM.Types.DWORDLONG);

   procedure SetProgressState
     (This     : ITaskbarList3_Type;
      hwnd     : Win32_Types.Long;
      tbpFlags : TBPFLAG);

   procedure RegisterTab
     (This    : ITaskbarList3_Type;
      hwndTab : Win32_Types.Long;
      hwndMDI : wireHWND);

   procedure UnregisterTab
     (This    : ITaskbarList3_Type;
      hwndTab : Win32_Types.Long);

   procedure SetTabOrder
     (This             : ITaskbarList3_Type;
      hwndTab          : Win32_Types.Long;
      hwndInsertBefore : Win32_Types.Long);

   procedure SetTabActive
     (This      : ITaskbarList3_Type;
      hwndTab   : Win32_Types.Long;
      hwndMDI   : Win32_Types.Long;
      tbatFlags : TBATFLAG);

   procedure ThumbBarAddButtons
     (This     : ITaskbarList3_Type;
      hwnd     : Win32_Types.Long;
      cButtons : Interfaces.C.unsigned;
      pButton  : Pointer_To_tagTHUMBBUTTON);

   procedure ThumbBarUpdateButtons
     (This     : ITaskbarList3_Type;
      hwnd     : Win32_Types.Long;
      cButtons : Interfaces.C.unsigned;
      pButton  : Pointer_To_tagTHUMBBUTTON);

   procedure ThumbBarSetImageList
     (This : ITaskbarList3_Type;
      hwnd : Win32_Types.Long;
      himl : GNATCOM.Types.Pointer_To_IUnknown);

   procedure SetOverlayIcon
     (This           : ITaskbarList3_Type;
      hwnd           : Win32_Types.Long;
      hIcon          : GNATCOM.Types.Pointer_To_IUnknown;
      pszDescription : GNATCOM.Types.LPWSTR);

   procedure SetThumbnailTooltip
     (This   : ITaskbarList3_Type;
      hwnd   : Win32_Types.Long;
      pszTip : GNATCOM.Types.LPWSTR);

   procedure SetThumbnailClip
     (This    : ITaskbarList3_Type;
      hwnd    : Win32_Types.Long;
      prcClip : Pointer_To_tagRECT);

end TaskbarLib.ITaskbarList3_Interface;
