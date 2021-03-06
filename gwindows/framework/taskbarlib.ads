--  Generated by             : BindCOM
--  Generated on             : 19.03.2014 03:41:05
--  Library Name             : TaskbarLib
--  Library Documentation    :
--  Library Version          : 1.0
--  Library LIBID            : {683BF642-E9CA-4124-BE43-67065B2FA653}
--  Elements in Type Library : 14

with Ada.Unchecked_Conversion;
with Interfaces.C;

with GNATCOM.Types;
with GNATCOM.GUID;

package TaskbarLib is

   type ITaskbarList;
   type IUnknown;
   type GUID;
   type T2;
   type ITaskbarList2;
   type tagTHUMBBUTTON;
   type ITaskbarList3;
   subtype TBPFLAG is Interfaces.C.long;
   type wireHWND;
   type uRemotableHandle;
   type u_MIDL_IWinTypes_0009;
   subtype TBATFLAG is Interfaces.C.long;
   type tagRECT;

   type Pointer_To_ITaskbarList is access all ITaskbarList;
   type Pointer_To_IUnknown is access all IUnknown;
   type Pointer_To_ITaskbarList2 is access all ITaskbarList2;
   type Pointer_To_ITaskbarList3 is access all ITaskbarList3;
   type Pointer_To_uRemotableHandle is access all uRemotableHandle;
   type Pointer_To_tagTHUMBBUTTON is access all tagTHUMBBUTTON;
   type Pointer_To_tagRECT is access all tagRECT;

   LIBID_TaskbarLib : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{683BF642-E9CA-4124-BE43-67065B2FA653}");

   --  Element Index         : 0
   --  Element Name          : ITaskbarList
   --  Element Type          : Interface

   IID_ITaskbarList : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{56FDF342-FD6D-11D0-958A-006097C9A090}");

   type af_ITaskbarList_QueryInterface is access
     function (This      : access ITaskbarList;
               riid      : GNATCOM.Types.Pointer_To_GUID;
               ppvObject : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList_QueryInterface);

   type af_ITaskbarList_AddRef is access
     function (This : access ITaskbarList)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_ITaskbarList_AddRef);

   type af_ITaskbarList_Release is access
     function (This : access ITaskbarList)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_ITaskbarList_Release);

   type af_ITaskbarList_HrInit is access
     function (This : access ITaskbarList)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList_HrInit);

   type af_ITaskbarList_AddTab is access
     function (This : access ITaskbarList;
               hwnd : Interfaces.C.long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList_AddTab);

   type af_ITaskbarList_DeleteTab is access
     function (This : access ITaskbarList;
               hwnd : Interfaces.C.long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList_DeleteTab);

   type af_ITaskbarList_ActivateTab is access
     function (This : access ITaskbarList;
               hwnd : Interfaces.C.long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList_ActivateTab);

   type af_ITaskbarList_SetActivateAlt is access
     function (This : access ITaskbarList;
               hwnd : Interfaces.C.long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList_SetActivateAlt);

   type ITaskbarListVtbl;
   type Pointer_To_ITaskbarListVtbl is access all ITaskbarListVtbl;

   type ITaskbarList is
      record
         Vtbl : Pointer_To_ITaskbarListVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, ITaskbarList);

   type ITaskbarListVtbl is
      record
         QueryInterface : af_ITaskbarList_QueryInterface;
         AddRef         : af_ITaskbarList_AddRef;
         Release        : af_ITaskbarList_Release;
         HrInit         : af_ITaskbarList_HrInit;
         AddTab         : af_ITaskbarList_AddTab;
         DeleteTab      : af_ITaskbarList_DeleteTab;
         ActivateTab    : af_ITaskbarList_ActivateTab;
         SetActivateAlt : af_ITaskbarList_SetActivateAlt;
      end record;
   pragma Convention (C_Pass_By_Copy, ITaskbarListVtbl);

   function To_Pointer_To_ITaskbarList is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_ITaskbarList);

   --  Element Index         : 1
   --  Element Name          : IUnknown
   --  Element Type          : Interface

   IID_IUnknown : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{00000000-0000-0000-C000-000000000046}");

   type af_IUnknown_QueryInterface is access
     function (This      : access IUnknown;
               riid      : GNATCOM.Types.Pointer_To_GUID;
               ppvObject : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_IUnknown_QueryInterface);

   type af_IUnknown_AddRef is access
     function (This : access IUnknown)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IUnknown_AddRef);

   type af_IUnknown_Release is access
     function (This : access IUnknown)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_IUnknown_Release);

   type IUnknownVtbl;
   type Pointer_To_IUnknownVtbl is access all IUnknownVtbl;

   type IUnknown is
      record
         Vtbl : Pointer_To_IUnknownVtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, IUnknown);

   type IUnknownVtbl is
      record
         QueryInterface : af_IUnknown_QueryInterface;
         AddRef         : af_IUnknown_AddRef;
         Release        : af_IUnknown_Release;
      end record;
   pragma Convention (C_Pass_By_Copy, IUnknownVtbl);

   function To_Pointer_To_IUnknown is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_IUnknown);

   type T1 is
     array (Integer range 0 .. 7) of Interfaces.C.unsigned_char;
   Size_Of_T1 : constant := Interfaces.C.unsigned_char'Size * 8;

   Size_Of_T2 : constant := 128;

   --  Element Index         : 3
   --  Element Name          : __MIDL___MIDL_itf_TaskbarLib_0006_0001_0001
   --  Element Type          : Record

   type T2 is
      record
         Data1 : Interfaces.C.unsigned_long;
         Data2 : Interfaces.C.unsigned_short;
         Data3 : Interfaces.C.unsigned_short;
         Data4 : T1;
      end record;
   pragma Convention (C_Pass_By_Copy, T2);
   for T2 use
      record
         Data1 at 0 range 0 .. 0 + Interfaces.C.unsigned_long'Size - 1;
         Data2 at 0 range 32 .. 32 + Interfaces.C.unsigned_short'Size - 1;
         Data3 at 0 range 48 .. 48 + Interfaces.C.unsigned_short'Size - 1;
         Data4 at 0 range 64 .. 64 + Size_Of_T1 - 1;
      end record;
   for T2'Size use Size_Of_T2;
   for T2'Alignment use 4;

   type GUID is
     new T2;

   --  Element Index         : 4
   --  Element Name          : ITaskbarList2
   --  Element Type          : Interface

   IID_ITaskbarList2 : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{602D4995-B13A-429B-A66E-1935E44F4317}");

   type af_ITaskbarList2_QueryInterface is access
     function (This      : access ITaskbarList2;
               riid      : GNATCOM.Types.Pointer_To_GUID;
               ppvObject : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList2_QueryInterface);

   type af_ITaskbarList2_AddRef is access
     function (This : access ITaskbarList2)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_ITaskbarList2_AddRef);

   type af_ITaskbarList2_Release is access
     function (This : access ITaskbarList2)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_ITaskbarList2_Release);

   type af_ITaskbarList2_HrInit is access
     function (This : access ITaskbarList2)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList2_HrInit);

   type af_ITaskbarList2_AddTab is access
     function (This : access ITaskbarList2;
               hwnd : Interfaces.C.long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList2_AddTab);

   type af_ITaskbarList2_DeleteTab is access
     function (This : access ITaskbarList2;
               hwnd : Interfaces.C.long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList2_DeleteTab);

   type af_ITaskbarList2_ActivateTab is access
     function (This : access ITaskbarList2;
               hwnd : Interfaces.C.long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList2_ActivateTab);

   type af_ITaskbarList2_SetActivateAlt is access
     function (This : access ITaskbarList2;
               hwnd : Interfaces.C.long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList2_SetActivateAlt);

   type af_ITaskbarList2_MarkFullscreenWindow is access
     function (This        : access ITaskbarList2;
               hwnd        : Interfaces.C.long;
               fFullscreen : Interfaces.C.long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList2_MarkFullscreenWindow);

   type ITaskbarList2Vtbl;
   type Pointer_To_ITaskbarList2Vtbl is access all ITaskbarList2Vtbl;

   type ITaskbarList2 is
      record
         Vtbl : Pointer_To_ITaskbarList2Vtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, ITaskbarList2);

   type ITaskbarList2Vtbl is
      record
         QueryInterface       : af_ITaskbarList2_QueryInterface;
         AddRef               : af_ITaskbarList2_AddRef;
         Release              : af_ITaskbarList2_Release;
         HrInit               : af_ITaskbarList2_HrInit;
         AddTab               : af_ITaskbarList2_AddTab;
         DeleteTab            : af_ITaskbarList2_DeleteTab;
         ActivateTab          : af_ITaskbarList2_ActivateTab;
         SetActivateAlt       : af_ITaskbarList2_SetActivateAlt;
         MarkFullscreenWindow : af_ITaskbarList2_MarkFullscreenWindow;
      end record;
   pragma Convention (C_Pass_By_Copy, ITaskbarList2Vtbl);

   function To_Pointer_To_ITaskbarList2 is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_ITaskbarList2);

   type T3 is
     array (Integer range 0 .. 259) of Interfaces.C.unsigned_short;
   Size_Of_T3 : constant := Interfaces.C.unsigned_short'Size * 260;

   --  **** Manual edit :
   Adj_64_Bits : constant := GNATCOM.Types.Size_Of_Pointers - 32;

   Size_Of_tagTHUMBBUTTON : constant := 4320 + Adj_64_Bits;

   --  Element Index         : 5
   --  Element Name          : tagTHUMBBUTTON
   --  Element Type          : Record

   type tagTHUMBBUTTON is
      record
         dwMask  : Interfaces.C.unsigned_long;
         iId     : Interfaces.C.unsigned;
         iBitmap : Interfaces.C.unsigned;
         hIcon   : GNATCOM.Types.Pointer_To_IUnknown;
         szTip   : T3;
         dwFlags : Interfaces.C.unsigned_long;
      end record;
   pragma Convention (C_Pass_By_Copy, tagTHUMBBUTTON);
   for tagTHUMBBUTTON use
      record
         dwMask  at 0 range 0 .. 0 + Interfaces.C.unsigned_long'Size - 1;
         iId     at 0 range 32 .. 32 + Interfaces.C.unsigned'Size - 1;
         iBitmap at 0 range 64 .. 64 + Interfaces.C.unsigned'Size - 1;
         hIcon   at 0 range 96 .. 96 + GNATCOM.Types.Size_Of_Pointers - 1;
         szTip   at 0 range 128 + Adj_64_Bits ..
                            128 + Adj_64_Bits + Size_Of_T3 - 1;
         dwFlags at 0 range 4288 + Adj_64_Bits ..
                            4288 + Adj_64_Bits +
                            Interfaces.C.unsigned_long'Size - 1;
      end record;
   for tagTHUMBBUTTON'Size use Size_Of_tagTHUMBBUTTON;
   for tagTHUMBBUTTON'Alignment use 4;

   --  Element Index         : 7
   --  Element Name          : TBPFLAG
   --  Element Type          : Enumeration

   TBPF_NOPROGRESS    : constant := 0;
   TBPF_INDETERMINATE : constant := 1;
   TBPF_NORMAL        : constant := 2;
   TBPF_ERROR         : constant := 4;
   TBPF_PAUSED        : constant := 8;

   Size_Of_u_MIDL_IWinTypes_0009 : constant := 32;

   --  Element Index         : 10
   --  Element Name          : __MIDL_IWinTypes_0009
   --  Element Type          : Union

   subtype u_MIDL_IWinTypes_0009_Range is Positive range 1 .. 2;
   type u_MIDL_IWinTypes_0009 (Which : u_MIDL_IWinTypes_0009_Range := 1) is
      record
         case Which is
            when 1 =>
               hInproc : Interfaces.C.long;
            when 2 =>
               hRemote : Interfaces.C.long;
         end case;
      end record;
   pragma Convention (C_Pass_By_Copy, u_MIDL_IWinTypes_0009);
   pragma Unchecked_Union (u_MIDL_IWinTypes_0009);
   for u_MIDL_IWinTypes_0009'Size use Size_Of_u_MIDL_IWinTypes_0009;

   Size_Of_uRemotableHandle : constant := 64;

   --  Element Index         : 9
   --  Element Name          : _RemotableHandle
   --  Element Type          : Record

   type uRemotableHandle is
      record
         fContext : Interfaces.C.long;
         u        : u_MIDL_IWinTypes_0009;
      end record;
   pragma Convention (C_Pass_By_Copy, uRemotableHandle);
   for uRemotableHandle use
      record
         fContext at 0 range 0 .. 0 + Interfaces.C.long'Size - 1;
         u        at 0 range 32 .. 32 + Size_Of_u_MIDL_IWinTypes_0009 - 1;
      end record;
   for uRemotableHandle'Size use Size_Of_uRemotableHandle;
   for uRemotableHandle'Alignment use 4;

   type wireHWND is
     new Pointer_To_uRemotableHandle;

   --  Element Index         : 11
   --  Element Name          : TBATFLAG
   --  Element Type          : Enumeration

   TBATF_USEMDITHUMBNAIL   : constant := 1;
   TBATF_USEMDILIVEPREVIEW : constant := 2;

   Size_Of_tagRECT : constant := 128;

   --  Element Index         : 12
   --  Element Name          : tagRECT
   --  Element Type          : Record

   type tagRECT is
      record
         left   : Interfaces.C.long;
         top    : Interfaces.C.long;
         right  : Interfaces.C.long;
         bottom : Interfaces.C.long;
      end record;
   pragma Convention (C_Pass_By_Copy, tagRECT);
   for tagRECT use
      record
         left   at 0 range 0 .. 0 + Interfaces.C.long'Size - 1;
         top    at 0 range 32 .. 32 + Interfaces.C.long'Size - 1;
         right  at 0 range 64 .. 64 + Interfaces.C.long'Size - 1;
         bottom at 0 range 96 .. 96 + Interfaces.C.long'Size - 1;
      end record;
   for tagRECT'Size use Size_Of_tagRECT;
   for tagRECT'Alignment use 4;

   --  Element Index         : 6
   --  Element Name          : ITaskbarList3
   --  Element Type          : Interface

   IID_ITaskbarList3 : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{EA1AFB91-9E28-4B86-90E9-9E9F8A5EEFAF}");

   type af_ITaskbarList3_QueryInterface is access
     function (This      : access ITaskbarList3;
               riid      : GNATCOM.Types.Pointer_To_GUID;
               ppvObject : GNATCOM.Types.Pointer_To_Pointer_To_Void)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList3_QueryInterface);

   type af_ITaskbarList3_AddRef is access
     function (This : access ITaskbarList3)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_ITaskbarList3_AddRef);

   type af_ITaskbarList3_Release is access
     function (This : access ITaskbarList3)
     return Interfaces.C.unsigned_long;
   pragma Convention (StdCall, af_ITaskbarList3_Release);

   type af_ITaskbarList3_HrInit is access
     function (This : access ITaskbarList3)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList3_HrInit);

   type af_ITaskbarList3_AddTab is access
     function (This : access ITaskbarList3;
               hwnd : Interfaces.C.long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList3_AddTab);

   type af_ITaskbarList3_DeleteTab is access
     function (This : access ITaskbarList3;
               hwnd : Interfaces.C.long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList3_DeleteTab);

   type af_ITaskbarList3_ActivateTab is access
     function (This : access ITaskbarList3;
               hwnd : Interfaces.C.long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList3_ActivateTab);

   type af_ITaskbarList3_SetActivateAlt is access
     function (This : access ITaskbarList3;
               hwnd : Interfaces.C.long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList3_SetActivateAlt);

   type af_ITaskbarList3_MarkFullscreenWindow is access
     function (This        : access ITaskbarList3;
               hwnd        : Interfaces.C.long;
               fFullscreen : Interfaces.C.long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList3_MarkFullscreenWindow);

   type af_ITaskbarList3_SetProgressValue is access
     function (This         : access ITaskbarList3;
               hwnd         : Interfaces.C.long;
               ullCompleted : GNATCOM.Types.DWORDLONG;
               ullTotal     : GNATCOM.Types.DWORDLONG)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList3_SetProgressValue);

   type af_ITaskbarList3_SetProgressState is access
     function (This     : access ITaskbarList3;
               hwnd     : Interfaces.C.long;
               tbpFlags : TBPFLAG)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList3_SetProgressState);

   type af_ITaskbarList3_RegisterTab is access
     function (This    : access ITaskbarList3;
               hwndTab : Interfaces.C.long;
               hwndMDI : wireHWND)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList3_RegisterTab);

   type af_ITaskbarList3_UnregisterTab is access
     function (This    : access ITaskbarList3;
               hwndTab : Interfaces.C.long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList3_UnregisterTab);

   type af_ITaskbarList3_SetTabOrder is access
     function (This             : access ITaskbarList3;
               hwndTab          : Interfaces.C.long;
               hwndInsertBefore : Interfaces.C.long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList3_SetTabOrder);

   type af_ITaskbarList3_SetTabActive is access
     function (This      : access ITaskbarList3;
               hwndTab   : Interfaces.C.long;
               hwndMDI   : Interfaces.C.long;
               tbatFlags : TBATFLAG)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList3_SetTabActive);

   type af_ITaskbarList3_ThumbBarAddButtons is access
     function (This     : access ITaskbarList3;
               hwnd     : Interfaces.C.long;
               cButtons : Interfaces.C.unsigned;
               pButton  : Pointer_To_tagTHUMBBUTTON)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList3_ThumbBarAddButtons);

   type af_ITaskbarList3_ThumbBarUpdateButtons is access
     function (This     : access ITaskbarList3;
               hwnd     : Interfaces.C.long;
               cButtons : Interfaces.C.unsigned;
               pButton  : Pointer_To_tagTHUMBBUTTON)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList3_ThumbBarUpdateButtons);

   type af_ITaskbarList3_ThumbBarSetImageList is access
     function (This : access ITaskbarList3;
               hwnd : Interfaces.C.long;
               himl : GNATCOM.Types.Pointer_To_IUnknown)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList3_ThumbBarSetImageList);

   type af_ITaskbarList3_SetOverlayIcon is access
     function (This           : access ITaskbarList3;
               hwnd           : Interfaces.C.long;
               hIcon          : GNATCOM.Types.Pointer_To_IUnknown;
               pszDescription : GNATCOM.Types.LPWSTR)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList3_SetOverlayIcon);

   type af_ITaskbarList3_SetThumbnailTooltip is access
     function (This   : access ITaskbarList3;
               hwnd   : Interfaces.C.long;
               pszTip : GNATCOM.Types.LPWSTR)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList3_SetThumbnailTooltip);

   type af_ITaskbarList3_SetThumbnailClip is access
     function (This    : access ITaskbarList3;
               hwnd    : Interfaces.C.long;
               prcClip : Pointer_To_tagRECT)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, af_ITaskbarList3_SetThumbnailClip);

   type ITaskbarList3Vtbl;
   type Pointer_To_ITaskbarList3Vtbl is access all ITaskbarList3Vtbl;

   type ITaskbarList3 is
      record
         Vtbl : Pointer_To_ITaskbarList3Vtbl;
      end record;
   pragma Convention (C_Pass_By_Copy, ITaskbarList3);

   type ITaskbarList3Vtbl is
      record
         QueryInterface        : af_ITaskbarList3_QueryInterface;
         AddRef                : af_ITaskbarList3_AddRef;
         Release               : af_ITaskbarList3_Release;
         HrInit                : af_ITaskbarList3_HrInit;
         AddTab                : af_ITaskbarList3_AddTab;
         DeleteTab             : af_ITaskbarList3_DeleteTab;
         ActivateTab           : af_ITaskbarList3_ActivateTab;
         SetActivateAlt        : af_ITaskbarList3_SetActivateAlt;
         MarkFullscreenWindow  : af_ITaskbarList3_MarkFullscreenWindow;
         SetProgressValue      : af_ITaskbarList3_SetProgressValue;
         SetProgressState      : af_ITaskbarList3_SetProgressState;
         RegisterTab           : af_ITaskbarList3_RegisterTab;
         UnregisterTab         : af_ITaskbarList3_UnregisterTab;
         SetTabOrder           : af_ITaskbarList3_SetTabOrder;
         SetTabActive          : af_ITaskbarList3_SetTabActive;
         ThumbBarAddButtons    : af_ITaskbarList3_ThumbBarAddButtons;
         ThumbBarUpdateButtons : af_ITaskbarList3_ThumbBarUpdateButtons;
         ThumbBarSetImageList  : af_ITaskbarList3_ThumbBarSetImageList;
         SetOverlayIcon        : af_ITaskbarList3_SetOverlayIcon;
         SetThumbnailTooltip   : af_ITaskbarList3_SetThumbnailTooltip;
         SetThumbnailClip      : af_ITaskbarList3_SetThumbnailClip;
      end record;
   pragma Convention (C_Pass_By_Copy, ITaskbarList3Vtbl);

   function To_Pointer_To_ITaskbarList3 is
     new Ada.Unchecked_Conversion
     (GNATCOM.Types.Pointer_To_Void, Pointer_To_ITaskbarList3);

   --  Element Index         : 13
   --  Element Name          : TaskbarList
   --  Element Type          : CoClass

   CLSID_TaskbarList : aliased GNATCOM.Types.GUID :=
     GNATCOM.GUID.To_GUID ("{56FDF344-FD6D-11D0-958A-006097C9A090}");

end TaskbarLib;
