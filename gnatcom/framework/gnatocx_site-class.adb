------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                  G N A T O C X _ S I T E . C L A S S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2005 David Botton                   --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. It is distributed in the hope that it will be useful,  but WITHOUT --
-- ANY WARRANTY;  without  even the  implied warranty of MERCHANTABILITY or --
-- FITNESS FOR A PARTICULAR PURPOSE.    See the GNU General  Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- More information about GNATCOM and the most current version can          --
-- be located on the web at http://www.gnavi.org/gnatcom                    --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

package body GNATOCX_Site.Class is

   function IStorage_CreateStream
     (This      : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pwcsName  : GNATCOM.Types.LPWSTR;
      grfMode   : Interfaces.C.unsigned_long;
      reserved1 : Interfaces.C.unsigned_long;
      reserved2 : Interfaces.C.unsigned_long;
      ppstm     : Pointer_To_Pointer_To_IStream)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, pwcsName, grfMode,
                           reserved1, reserved2, ppstm);
   begin
      return GNATCOM.E_NOTIMPL;
   end IStorage_CreateStream;

   function IStorage_RemoteOpenStream
     (This        : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pwcsName    : GNATCOM.Types.LPWSTR;
      cbReserved1 : Interfaces.C.unsigned_long;
      reserved1   : Pointer_To_unsigned_char;
      grfMode     : Interfaces.C.unsigned_long;
      reserved2   : Interfaces.C.unsigned_long;
      ppstm       : Pointer_To_Pointer_To_IStream)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, pwcsName, cbReserved1, reserved1,
                           grfMode, reserved2, ppstm);
   begin
      return GNATCOM.E_NOTIMPL;
   end IStorage_RemoteOpenStream;

   function IStorage_CreateStorage
     (This      : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pwcsName  : GNATCOM.Types.LPWSTR;
      grfMode   : Interfaces.C.unsigned_long;
      reserved1 : Interfaces.C.unsigned_long;
      reserved2 : Interfaces.C.unsigned_long;
      ppstg     : Pointer_To_Pointer_To_IStorage)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, pwcsName, grfMode, reserved1,
                           reserved2, ppstg);
   begin
      return GNATCOM.E_NOTIMPL;
   end IStorage_CreateStorage;

   function IStorage_OpenStorage
     (This         : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pwcsName     : GNATCOM.Types.LPWSTR;
      pstgPriority : Pointer_To_IStorage;
      grfMode      : Interfaces.C.unsigned_long;
      snbExclude   : SNB;
      reserved     : Interfaces.C.unsigned_long;
      ppstg        : Pointer_To_Pointer_To_IStorage)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, pwcsName, pstgPriority, grfMode,
                           snbExclude, reserved, ppstg);
   begin
      return GNATCOM.E_NOTIMPL;
   end IStorage_OpenStorage;

   function IStorage_CopyTo
     (This         : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      ciidExclude  : Interfaces.C.unsigned_long;
      rgiidExclude : GNATCOM.Types.Pointer_To_GUID;
      snbExclude   : SNB;
      pstgDest     : Pointer_To_IStorage)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, ciidExclude, rgiidExclude,
                           snbExclude, pstgDest);
   begin
      return GNATCOM.E_NOTIMPL;
   end IStorage_CopyTo;

   function IStorage_MoveElementTo
     (This        : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pwcsName    : GNATCOM.Types.LPWSTR;
      pstgDest    : Pointer_To_IStorage;
      pwcsNewName : GNATCOM.Types.LPWSTR;
      grfFlags    : Interfaces.C.unsigned_long)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, pwcsName, pstgDest, pwcsNewName, grfFlags);
   begin
      return GNATCOM.E_NOTIMPL;
   end IStorage_MoveElementTo;

   function IStorage_Commit
     (This           : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      grfCommitFlags : Interfaces.C.unsigned_long)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, grfCommitFlags);
   begin
      return GNATCOM.E_NOTIMPL;
   end IStorage_Commit;

   function IStorage_Revert
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This);
   begin
      return GNATCOM.E_NOTIMPL;
   end IStorage_Revert;

   function IStorage_RemoteEnumElements
     (This        : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      reserved1   : Interfaces.C.unsigned_long;
      cbReserved2 : Interfaces.C.unsigned_long;
      reserved2   : Pointer_To_unsigned_char;
      reserved3   : Interfaces.C.unsigned_long;
      ppenum      : Pointer_To_Pointer_To_IEnumSTATSTG)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, reserved1, cbReserved2,
                           reserved2, reserved3, ppenum);
   begin
      return GNATCOM.E_NOTIMPL;
   end IStorage_RemoteEnumElements;

   function IStorage_DestroyElement
     (This     : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pwcsName : GNATCOM.Types.LPWSTR)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, pwcsName);
   begin
      return GNATCOM.E_NOTIMPL;
   end IStorage_DestroyElement;

   function IStorage_RenameElement
     (This        : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pwcsOldName : GNATCOM.Types.LPWSTR;
      pwcsNewName : GNATCOM.Types.LPWSTR)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, pwcsOldName, pwcsNewName);
   begin
      return GNATCOM.E_NOTIMPL;
   end IStorage_RenameElement;

   function IStorage_SetElementTimes
     (This     : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pwcsName : GNATCOM.Types.LPWSTR;
      pctime   : Pointer_To_uFILETIME;
      patime   : Pointer_To_uFILETIME;
      pmtime   : Pointer_To_uFILETIME)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, pwcsName, pctime, patime, pmtime);
   begin
      return GNATCOM.E_NOTIMPL;
   end IStorage_SetElementTimes;

   function IStorage_SetClass
     (This  : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      clsid : GNATCOM.Types.Pointer_To_GUID)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, clsid);
   begin
      return GNATCOM.S_OK;
   end IStorage_SetClass;

   function IStorage_SetStateBits
     (This         : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      grfStateBits : Interfaces.C.unsigned_long;
      grfMask      : Interfaces.C.unsigned_long)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, grfStateBits, grfMask);
   begin
      return GNATCOM.E_NOTIMPL;
   end IStorage_SetStateBits;

   function IStorage_Stat
     (This        : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pstatstg    : Pointer_To_STATSTG;
      grfStatFlag : Interfaces.C.unsigned_long)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, pstatstg, grfStatFlag);
   begin
      return GNATCOM.E_NOTIMPL;
   end IStorage_Stat;

   function IOleInPlaceFrame_GetWindow
     (This  : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      phwnd : Pointer_To_HWND)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, phwnd);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleInPlaceFrame_GetWindow;

   function IOleInPlaceFrame_ContextSensitiveHelp
     (This       : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      fEnterMode : Interfaces.C.long)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, fEnterMode);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleInPlaceFrame_ContextSensitiveHelp;

   function IOleInPlaceFrame_GetBorder
     (This         : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      lprectBorder : Pointer_To_RECT)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, lprectBorder);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleInPlaceFrame_GetBorder;

   function IOleInPlaceFrame_RequestBorderSpace
     (This          : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pborderwidths : Pointer_To_RECT)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, pborderwidths);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleInPlaceFrame_RequestBorderSpace;

   function IOleInPlaceFrame_SetBorderSpace
     (This          : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pborderwidths : Pointer_To_RECT)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, pborderwidths);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleInPlaceFrame_SetBorderSpace;

   function IOleInPlaceFrame_SetActiveObject
     (This          : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pActiveObject : Pointer_To_IOleInPlaceActiveObject;
      pszObjName    : GNATCOM.Types.LPWSTR)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, pActiveObject, pszObjName);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleInPlaceFrame_SetActiveObject;

   function IOleInPlaceFrame_InsertMenus
     (This         : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      hmenuShared  : HMENU;
      lpMenuWidths : Pointer_To_OleMenuGroupWidths)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, hmenuShared, lpMenuWidths);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleInPlaceFrame_InsertMenus;

   function IOleInPlaceFrame_SetMenu
     (This             : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      hmenuShared      : HMENU;
      holemenu         : HGLOBAL;
      hwndActiveObject : HWND)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, hmenuShared, holemenu, hwndActiveObject);
   begin
      return GNATCOM.S_OK;
   end IOleInPlaceFrame_SetMenu;

   function IOleInPlaceFrame_RemoveMenus
     (This        : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      hmenuShared : HMENU)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, hmenuShared);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleInPlaceFrame_RemoveMenus;

   function IOleInPlaceFrame_SetStatusText
     (This          : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pszStatusText : GNATCOM.Types.LPWSTR)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, pszStatusText);
   begin
      return GNATCOM.S_OK;
   end IOleInPlaceFrame_SetStatusText;

   function IOleInPlaceFrame_EnableModeless
     (This    : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      fEnable : Interfaces.C.long)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, fEnable);
   begin
      return GNATCOM.S_OK;
   end IOleInPlaceFrame_EnableModeless;

   function IOleInPlaceFrame_TranslateAccelerator
     (This  : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      lpmsg : Pointer_To_MSG;
      wID   : Interfaces.C.unsigned_short)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, lpmsg, wID);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleInPlaceFrame_TranslateAccelerator;

   function IOleClientSite_SaveObject
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleClientSite_SaveObject;

   function IOleClientSite_GetMoniker
     (This           : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      dwAssign       : Interfaces.C.unsigned_long;
      dwWhichMoniker : Interfaces.C.unsigned_long;
      ppmk           : Pointer_To_Pointer_To_IMoniker)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, dwAssign, dwWhichMoniker, ppmk);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleClientSite_GetMoniker;

   function IOleClientSite_GetContainer
     (This        : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      ppContainer : Pointer_To_Pointer_To_IOleContainer)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This);
   begin
      ppContainer.all := null;
      return GNATCOM.E_NOINTERFACE;
   end IOleClientSite_GetContainer;

   function IOleClientSite_ShowObject
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This);
   begin
      return GNATCOM.S_OK;
   end IOleClientSite_ShowObject;

   function IOleClientSite_OnShowWindow
     (This  : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      fShow : Interfaces.C.long)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (fShow, This);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleClientSite_OnShowWindow;

   function IOleClientSite_RequestNewObjectLayout
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleClientSite_RequestNewObjectLayout;

   function IOleInPlaceSite_GetWindow
     (This  : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      phwnd : Pointer_To_HWND)
     return GNATCOM.Types.HRESULT
   is
      Object : Pointer_To_GNATOCXClass_Type :=
        Pointer_To_GNATOCXClass_Type (This.CoClass);
   begin
      phwnd.all := Object.HWND;
      return GNATCOM.S_OK;
   end IOleInPlaceSite_GetWindow;

   function IOleInPlaceSite_ContextSensitiveHelp
     (This       : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      fEnterMode : Interfaces.C.long)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, fEnterMode);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleInPlaceSite_ContextSensitiveHelp;

   function IOleInPlaceSite_CanInPlaceActivate
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This);
   begin
      return GNATCOM.S_OK;
   end IOleInPlaceSite_CanInPlaceActivate;

   function IOleInPlaceSite_OnInPlaceActivate
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This);
   begin
      return GNATCOM.S_OK;
   end IOleInPlaceSite_OnInPlaceActivate;

   function IOleInPlaceSite_OnUIActivate
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This);
   begin
      return GNATCOM.S_OK;
   end IOleInPlaceSite_OnUIActivate;

   function IOleInPlaceSite_GetWindowContext
     (This         : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      ppFrame      : Pointer_To_Pointer_To_IOleInPlaceFrame;
      ppDoc        : Pointer_To_Pointer_To_IOleInPlaceUIWindow;
      lprcPosRect  : Pointer_To_RECT;
      lprcClipRect : Pointer_To_RECT;
      lpFrameInfo  : Pointer_To_OIFI)
     return GNATCOM.Types.HRESULT
   is
      function To_Pointer_To_IOleInPlaceFrame is
         new Ada.Unchecked_Conversion
        (GNATCOM.Types.Pointer_To_Void, Pointer_To_IOleInPlaceFrame);

      Object : Pointer_To_GNATOCXClass_Type :=
        Pointer_To_GNATOCXClass_Type (This.CoClass);

      Frame_Interface : aliased GNATCOM.Types.Pointer_To_Void;

      procedure GetClientRect
        (hwnd            : System.Address := Object.HWND;
         Rect            : out GNATOCX_Site.RECT);
      pragma Import (StdCall, GetClientRect, "GetClientRect");

      RIID   : aliased GNATCOM.Types.GUID := IID_IOleInPlaceFrame;
      Result : GNATCOM.Types.HRESULT;
   begin
      Result := GNATCOM.Create.COM_Interface.QueryInterface
        (This,
         RIID'Unchecked_Access,
         Frame_Interface'Access);

      ppFrame.all := To_Pointer_To_IOleInPlaceFrame (Frame_Interface);
      ppDoc.all := null;

      GetClientRect (Rect => lprcPosRect.all);
      GetClientRect (Rect => lprcClipRect.all);

      lpFrameInfo.fMDIApp := 0;
      lpFrameInfo.hwndFrame := Object.HWND;
      lpFrameInfo.haccel := 0;
      lpFrameInfo.cAccelEntries := 0;

      return GNATCOM.S_OK;
   end IOleInPlaceSite_GetWindowContext;

   function IOleInPlaceSite_Scroll
     (This         : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      scrollExtant : SIZE)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, scrollExtant);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleInPlaceSite_Scroll;

   function IOleInPlaceSite_OnUIDeactivate
     (This      : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      fUndoable : Interfaces.C.long)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, fUndoable);
   begin
      return GNATCOM.S_OK;
   end IOleInPlaceSite_OnUIDeactivate;

   function IOleInPlaceSite_OnInPlaceDeactivate
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This);
   begin
      return GNATCOM.S_OK;
   end IOleInPlaceSite_OnInPlaceDeactivate;

   function IOleInPlaceSite_DiscardUndoState
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleInPlaceSite_DiscardUndoState;

   function IOleInPlaceSite_DeactivateAndUndo
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleInPlaceSite_DeactivateAndUndo;

   function IOleInPlaceSite_OnPosRectChange
     (This        : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      lprcPosRect : Pointer_To_RECT)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, lprcPosRect);
   begin
      return GNATCOM.S_OK;
   end IOleInPlaceSite_OnPosRectChange;

   function IOleContainer_ParseDisplayName
     (This           : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pbc            : Pointer_To_IBindCtx;
      pszDisplayName : GNATCOM.Types.LPWSTR;
      pchEaten       : GNATCOM.Types.Pointer_To_unsigned_long;
      ppmkOut        : Pointer_To_Pointer_To_IMoniker)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, pbc, pszDisplayName, pchEaten, ppmkOut);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleContainer_ParseDisplayName;

   function IOleContainer_EnumObjects
     (This     : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      grfFlags : Interfaces.C.unsigned_long;
      ppenum   : Pointer_To_Pointer_To_IEnumUnknown)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, grfFlags, ppenum);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleContainer_EnumObjects;

   function IOleContainer_LockContainer
     (This  : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      fLock : Interfaces.C.long)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, fLock);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleContainer_LockContainer;

   function IOleWindow_GetWindow
     (This  : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      phwnd : Pointer_To_HWND)
     return GNATCOM.Types.HRESULT
   is
      Object : Pointer_To_GNATOCXClass_Type :=
        Pointer_To_GNATOCXClass_Type (This.CoClass);
   begin
      phwnd.all := Object.HWND;
      return GNATCOM.S_OK;
   end IOleWindow_GetWindow;

   function IOleWindow_ContextSensitiveHelp
     (This       : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      fEnterMode : Interfaces.C.long)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, fEnterMode);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleWindow_ContextSensitiveHelp;

   function IOleInPlaceUIWindow_GetWindow
     (This  : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      phwnd : Pointer_To_HWND)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, phwnd);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleInPlaceUIWindow_GetWindow;

   function IOleInPlaceUIWindow_ContextSensitiveHelp
     (This       : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      fEnterMode : Interfaces.C.long)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, fEnterMode);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleInPlaceUIWindow_ContextSensitiveHelp;

   function IOleInPlaceUIWindow_GetBorder
     (This         : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      lprectBorder : Pointer_To_RECT)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, lprectBorder);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleInPlaceUIWindow_GetBorder;

   function IOleInPlaceUIWindow_RequestBorderSpace
     (This          : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pborderwidths : Pointer_To_RECT)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, pborderwidths);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleInPlaceUIWindow_RequestBorderSpace;

   function IOleInPlaceUIWindow_SetBorderSpace
     (This          : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pborderwidths : Pointer_To_RECT)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, pborderwidths);
   begin
      return GNATCOM.E_NOTIMPL;
   end IOleInPlaceUIWindow_SetBorderSpace;

   function IOleInPlaceUIWindow_SetActiveObject
     (This          : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pActiveObject : Pointer_To_IOleInPlaceActiveObject;
      pszObjName    : GNATCOM.Types.LPWSTR)
     return GNATCOM.Types.HRESULT
   is
      pragma Unreferenced (This, pActiveObject, pszObjName);
   begin
      return GNATCOM.S_OK;
   end IOleInPlaceUIWindow_SetActiveObject;

   function Create
     return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type
   is
   begin
      return GNATCOM.Create.COM_Interface.Create_Object
        (new GNATOCXClass_Type);
   end Create;

end GNATOCX_Site.Class;
