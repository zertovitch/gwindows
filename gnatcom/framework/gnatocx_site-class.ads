------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                  G N A T O C X _ S I T E . C L A S S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2005 David Botton                   --
--                                                                          --
-- MIT License                                                              --
--                                                                          --
-- Permission is hereby granted, free of charge, to any person obtaining    --
-- a copy of this software and associated documentation files (the          --
-- "Software"), to deal in the Software without restriction, including      --
-- without limitation the rights to use, copy, modify, merge, publish,      --
-- distribute, sublicense, and/or sell copies of the Software, and to       --
-- permit persons to whom the Software is furnished to do so, subject to    --
-- the following conditions:                                                --
--                                                                          --
-- The above copyright notice and this permission notice shall be included  --
-- in all copies or substantial portions of the Software.                   --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,          --
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF       --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   --
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY     --
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,     --
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE        --
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                   --
--                                                                          --
-- More information about GNATCOM and the most current version can          --
-- be located on the web at http://www.gnavi.org/gnatcom                    --
--                                                                          --
------------------------------------------------------------------------------

with GNATOCX.IOleObject_Interface;
with System;
with Win32_Types;

package GNATOCX_Site.Class is

   function IStorage_CreateStream
     (This      : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pwcsName  : GNATCOM.Types.LPWSTR;
      grfMode   : Win32_Types.Unsigned_Long;
      reserved1 : Win32_Types.Unsigned_Long;
      reserved2 : Win32_Types.Unsigned_Long;
      ppstm     : Pointer_To_Pointer_To_IStream)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IStorage_CreateStream);
   pragma Machine_Attribute (IStorage_CreateStream, "ms_abi");

   function IStorage_RemoteOpenStream
     (This        : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pwcsName    : GNATCOM.Types.LPWSTR;
      cbReserved1 : Win32_Types.Unsigned_Long;
      reserved1   : Pointer_To_unsigned_char;
      grfMode     : Win32_Types.Unsigned_Long;
      reserved2   : Win32_Types.Unsigned_Long;
      ppstm       : Pointer_To_Pointer_To_IStream)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IStorage_RemoteOpenStream);
   pragma Machine_Attribute (IStorage_RemoteOpenStream, "ms_abi");

   function IStorage_CreateStorage
     (This      : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pwcsName  : GNATCOM.Types.LPWSTR;
      grfMode   : Win32_Types.Unsigned_Long;
      reserved1 : Win32_Types.Unsigned_Long;
      reserved2 : Win32_Types.Unsigned_Long;
      ppstg     : Pointer_To_Pointer_To_IStorage)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IStorage_CreateStorage);
   pragma Machine_Attribute (IStorage_CreateStorage, "ms_abi");

   function IStorage_OpenStorage
     (This         : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pwcsName     : GNATCOM.Types.LPWSTR;
      pstgPriority : Pointer_To_IStorage;
      grfMode      : Win32_Types.Unsigned_Long;
      snbExclude   : SNB;
      reserved     : Win32_Types.Unsigned_Long;
      ppstg        : Pointer_To_Pointer_To_IStorage)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IStorage_OpenStorage);
   pragma Machine_Attribute (IStorage_OpenStorage, "ms_abi");

   function IStorage_CopyTo
     (This         : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      ciidExclude  : Win32_Types.Unsigned_Long;
      rgiidExclude : GNATCOM.Types.Pointer_To_GUID;
      snbExclude   : SNB;
      pstgDest     : Pointer_To_IStorage)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IStorage_CopyTo);
   pragma Machine_Attribute (IStorage_CopyTo, "ms_abi");

   function IStorage_MoveElementTo
     (This        : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pwcsName    : GNATCOM.Types.LPWSTR;
      pstgDest    : Pointer_To_IStorage;
      pwcsNewName : GNATCOM.Types.LPWSTR;
      grfFlags    : Win32_Types.Unsigned_Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IStorage_MoveElementTo);
   pragma Machine_Attribute (IStorage_MoveElementTo, "ms_abi");

   function IStorage_Commit
     (This           : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      grfCommitFlags : Win32_Types.Unsigned_Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IStorage_Commit);
   pragma Machine_Attribute (IStorage_Commit, "ms_abi");

   function IStorage_Revert
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IStorage_Revert);
   pragma Machine_Attribute (IStorage_Revert, "ms_abi");

   function IStorage_RemoteEnumElements
     (This        : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      reserved1   : Win32_Types.Unsigned_Long;
      cbReserved2 : Win32_Types.Unsigned_Long;
      reserved2   : Pointer_To_unsigned_char;
      reserved3   : Win32_Types.Unsigned_Long;
      ppenum      : Pointer_To_Pointer_To_IEnumSTATSTG)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IStorage_RemoteEnumElements);
   pragma Machine_Attribute (IStorage_RemoteEnumElements, "ms_abi");

   function IStorage_DestroyElement
     (This     : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pwcsName : GNATCOM.Types.LPWSTR)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IStorage_DestroyElement);
   pragma Machine_Attribute (IStorage_DestroyElement, "ms_abi");

   function IStorage_RenameElement
     (This        : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pwcsOldName : GNATCOM.Types.LPWSTR;
      pwcsNewName : GNATCOM.Types.LPWSTR)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IStorage_RenameElement);
   pragma Machine_Attribute (IStorage_RenameElement, "ms_abi");

   function IStorage_SetElementTimes
     (This     : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pwcsName : GNATCOM.Types.LPWSTR;
      pctime   : Pointer_To_uFILETIME;
      patime   : Pointer_To_uFILETIME;
      pmtime   : Pointer_To_uFILETIME)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IStorage_SetElementTimes);
   pragma Machine_Attribute (IStorage_SetElementTimes, "ms_abi");

   function IStorage_SetClass
     (This  : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      clsid : GNATCOM.Types.Pointer_To_GUID)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IStorage_SetClass);
   pragma Machine_Attribute (IStorage_SetClass, "ms_abi");

   function IStorage_SetStateBits
     (This         : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      grfStateBits : Win32_Types.Unsigned_Long;
      grfMask      : Win32_Types.Unsigned_Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IStorage_SetStateBits);
   pragma Machine_Attribute (IStorage_SetStateBits, "ms_abi");

   function IStorage_Stat
     (This        : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pstatstg    : Pointer_To_STATSTG;
      grfStatFlag : Win32_Types.Unsigned_Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IStorage_Stat);
   pragma Machine_Attribute (IStorage_Stat, "ms_abi");

   type IStorage_Vtbl_Record is
      record
         IUnknown           :
           GNATCOM.Create.COM_Interface.IUnknown_Vtbl_Record;
         CreateStream       : af_IStorage_CreateStream :=
           IStorage_CreateStream'Access;
         RemoteOpenStream   : af_IStorage_RemoteOpenStream :=
           IStorage_RemoteOpenStream'Access;
         CreateStorage      : af_IStorage_CreateStorage :=
           IStorage_CreateStorage'Access;
         OpenStorage        : af_IStorage_OpenStorage :=
           IStorage_OpenStorage'Access;
         CopyTo             : af_IStorage_CopyTo :=
           IStorage_CopyTo'Access;
         MoveElementTo      : af_IStorage_MoveElementTo :=
           IStorage_MoveElementTo'Access;
         Commit             : af_IStorage_Commit :=
           IStorage_Commit'Access;
         Revert             : af_IStorage_Revert :=
           IStorage_Revert'Access;
         RemoteEnumElements : af_IStorage_RemoteEnumElements :=
           IStorage_RemoteEnumElements'Access;
         DestroyElement     : af_IStorage_DestroyElement :=
           IStorage_DestroyElement'Access;
         RenameElement      : af_IStorage_RenameElement :=
           IStorage_RenameElement'Access;
         SetElementTimes    : af_IStorage_SetElementTimes :=
           IStorage_SetElementTimes'Access;
         SetClass           : af_IStorage_SetClass :=
           IStorage_SetClass'Access;
         SetStateBits       : af_IStorage_SetStateBits :=
           IStorage_SetStateBits'Access;
         Stat               : af_IStorage_Stat :=
           IStorage_Stat'Access;
      end record;
   pragma Convention (C_Pass_By_Copy, IStorage_Vtbl_Record);

   type Pointer_To_IStorage_Vtbl_Record is
     access all IStorage_Vtbl_Record;

   IStorage_Vtbl : aliased IStorage_Vtbl_Record;

   function IOleInPlaceFrame_GetWindow
     (This  : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      phwnd : Pointer_To_HWND)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceFrame_GetWindow);
   pragma Machine_Attribute (IOleInPlaceFrame_GetWindow, "ms_abi");

   function IOleInPlaceFrame_ContextSensitiveHelp
     (This       : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      fEnterMode : Win32_Types.Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceFrame_ContextSensitiveHelp);
   pragma Machine_Attribute (IOleInPlaceFrame_ContextSensitiveHelp, "ms_abi");

   function IOleInPlaceFrame_GetBorder
     (This         : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      lprectBorder : Pointer_To_RECT)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceFrame_GetBorder);
   pragma Machine_Attribute (IOleInPlaceFrame_GetBorder, "ms_abi");

   function IOleInPlaceFrame_RequestBorderSpace
     (This          : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pborderwidths : Pointer_To_RECT)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceFrame_RequestBorderSpace);
   pragma Machine_Attribute (IOleInPlaceFrame_RequestBorderSpace, "ms_abi");

   function IOleInPlaceFrame_SetBorderSpace
     (This          : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pborderwidths : Pointer_To_RECT)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceFrame_SetBorderSpace);
   pragma Machine_Attribute (IOleInPlaceFrame_SetBorderSpace, "ms_abi");

   function IOleInPlaceFrame_SetActiveObject
     (This          : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pActiveObject : Pointer_To_IOleInPlaceActiveObject;
      pszObjName    : GNATCOM.Types.LPWSTR)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceFrame_SetActiveObject);
   pragma Machine_Attribute (IOleInPlaceFrame_SetActiveObject, "ms_abi");

   function IOleInPlaceFrame_InsertMenus
     (This         : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      hmenuShared  : HMENU;
      lpMenuWidths : Pointer_To_OleMenuGroupWidths)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceFrame_InsertMenus);
   pragma Machine_Attribute (IOleInPlaceFrame_InsertMenus, "ms_abi");

   function IOleInPlaceFrame_SetMenu
     (This             : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      hmenuShared      : HMENU;
      holemenu         : HGLOBAL;
      hwndActiveObject : HWND)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceFrame_SetMenu);
   pragma Machine_Attribute (IOleInPlaceFrame_SetMenu, "ms_abi");

   function IOleInPlaceFrame_RemoveMenus
     (This        : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      hmenuShared : HMENU)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceFrame_RemoveMenus);
   pragma Machine_Attribute (IOleInPlaceFrame_RemoveMenus, "ms_abi");

   function IOleInPlaceFrame_SetStatusText
     (This          : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pszStatusText : GNATCOM.Types.LPWSTR)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceFrame_SetStatusText);
   pragma Machine_Attribute (IOleInPlaceFrame_SetStatusText, "ms_abi");

   function IOleInPlaceFrame_EnableModeless
     (This    : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      fEnable : Win32_Types.Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceFrame_EnableModeless);
   pragma Machine_Attribute (IOleInPlaceFrame_EnableModeless, "ms_abi");

   function IOleInPlaceFrame_TranslateAccelerator
     (This  : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      lpmsg : Pointer_To_MSG;
      wID   : Interfaces.C.unsigned_short)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceFrame_TranslateAccelerator);
   pragma Machine_Attribute (IOleInPlaceFrame_TranslateAccelerator, "ms_abi");

   type IOleInPlaceFrame_Vtbl_Record is
      record
         IUnknown             :
           GNATCOM.Create.COM_Interface.IUnknown_Vtbl_Record;
         GetWindow            : af_IOleInPlaceFrame_GetWindow :=
           IOleInPlaceFrame_GetWindow'Access;
         ContextSensitiveHelp : af_IOleInPlaceFrame_ContextSensitiveHelp :=
           IOleInPlaceFrame_ContextSensitiveHelp'Access;
         GetBorder            : af_IOleInPlaceFrame_GetBorder :=
           IOleInPlaceFrame_GetBorder'Access;
         RequestBorderSpace   : af_IOleInPlaceFrame_RequestBorderSpace :=
           IOleInPlaceFrame_RequestBorderSpace'Access;
         SetBorderSpace       : af_IOleInPlaceFrame_SetBorderSpace :=
           IOleInPlaceFrame_SetBorderSpace'Access;
         SetActiveObject      : af_IOleInPlaceFrame_SetActiveObject :=
           IOleInPlaceFrame_SetActiveObject'Access;
         InsertMenus          : af_IOleInPlaceFrame_InsertMenus :=
           IOleInPlaceFrame_InsertMenus'Access;
         SetMenu              : af_IOleInPlaceFrame_SetMenu :=
           IOleInPlaceFrame_SetMenu'Access;
         RemoveMenus          : af_IOleInPlaceFrame_RemoveMenus :=
           IOleInPlaceFrame_RemoveMenus'Access;
         SetStatusText        : af_IOleInPlaceFrame_SetStatusText :=
           IOleInPlaceFrame_SetStatusText'Access;
         EnableModeless       : af_IOleInPlaceFrame_EnableModeless :=
           IOleInPlaceFrame_EnableModeless'Access;
         TranslateAccelerator : af_IOleInPlaceFrame_TranslateAccelerator :=
           IOleInPlaceFrame_TranslateAccelerator'Access;
      end record;
   pragma Convention (C_Pass_By_Copy, IOleInPlaceFrame_Vtbl_Record);

   type Pointer_To_IOleInPlaceFrame_Vtbl_Record is
     access all IOleInPlaceFrame_Vtbl_Record;

   IOleInPlaceFrame_Vtbl : aliased IOleInPlaceFrame_Vtbl_Record;

   function IOleClientSite_SaveObject
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleClientSite_SaveObject);
   pragma Machine_Attribute (IOleClientSite_SaveObject, "ms_abi");

   function IOleClientSite_GetMoniker
     (This           : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      dwAssign       : Win32_Types.Unsigned_Long;
      dwWhichMoniker : Win32_Types.Unsigned_Long;
      ppmk           : Pointer_To_Pointer_To_IMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleClientSite_GetMoniker);
   pragma Machine_Attribute (IOleClientSite_GetMoniker, "ms_abi");

   function IOleClientSite_GetContainer
     (This        : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      ppContainer : Pointer_To_Pointer_To_IOleContainer)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleClientSite_GetContainer);
   pragma Machine_Attribute (IOleClientSite_GetContainer, "ms_abi");

   function IOleClientSite_ShowObject
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleClientSite_ShowObject);
   pragma Machine_Attribute (IOleClientSite_ShowObject, "ms_abi");

   function IOleClientSite_OnShowWindow
     (This  : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      fShow : Win32_Types.Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleClientSite_OnShowWindow);
   pragma Machine_Attribute (IOleClientSite_OnShowWindow, "ms_abi");

   function IOleClientSite_RequestNewObjectLayout
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleClientSite_RequestNewObjectLayout);
   pragma Machine_Attribute (IOleClientSite_RequestNewObjectLayout, "ms_abi");

   type IOleClientSite_Vtbl_Record is
      record
         IUnknown               :
           GNATCOM.Create.COM_Interface.IUnknown_Vtbl_Record;
         SaveObject             : af_IOleClientSite_SaveObject :=
           IOleClientSite_SaveObject'Access;
         GetMoniker             : af_IOleClientSite_GetMoniker :=
           IOleClientSite_GetMoniker'Access;
         GetContainer           : af_IOleClientSite_GetContainer :=
           IOleClientSite_GetContainer'Access;
         ShowObject             : af_IOleClientSite_ShowObject :=
           IOleClientSite_ShowObject'Access;
         OnShowWindow           : af_IOleClientSite_OnShowWindow :=
           IOleClientSite_OnShowWindow'Access;
         RequestNewObjectLayout : af_IOleClientSite_RequestNewObjectLayout :=
           IOleClientSite_RequestNewObjectLayout'Access;
      end record;
   pragma Convention (C_Pass_By_Copy, IOleClientSite_Vtbl_Record);

   type Pointer_To_IOleClientSite_Vtbl_Record is
     access all IOleClientSite_Vtbl_Record;

   IOleClientSite_Vtbl : aliased IOleClientSite_Vtbl_Record;

   function IOleInPlaceSite_GetWindow
     (This  : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      phwnd : Pointer_To_HWND)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceSite_GetWindow);
   pragma Machine_Attribute (IOleInPlaceSite_GetWindow, "ms_abi");

   function IOleInPlaceSite_ContextSensitiveHelp
     (This       : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      fEnterMode : Win32_Types.Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceSite_ContextSensitiveHelp);
   pragma Machine_Attribute (IOleInPlaceSite_ContextSensitiveHelp, "ms_abi");

   function IOleInPlaceSite_CanInPlaceActivate
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceSite_CanInPlaceActivate);
   pragma Machine_Attribute (IOleInPlaceSite_CanInPlaceActivate, "ms_abi");

   function IOleInPlaceSite_OnInPlaceActivate
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceSite_OnInPlaceActivate);
   pragma Machine_Attribute (IOleInPlaceSite_OnInPlaceActivate, "ms_abi");

   function IOleInPlaceSite_OnUIActivate
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceSite_OnUIActivate);
   pragma Machine_Attribute (IOleInPlaceSite_OnUIActivate, "ms_abi");

   function IOleInPlaceSite_GetWindowContext
     (This         : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      ppFrame      : Pointer_To_Pointer_To_IOleInPlaceFrame;
      ppDoc        : Pointer_To_Pointer_To_IOleInPlaceUIWindow;
      lprcPosRect  : Pointer_To_RECT;
      lprcClipRect : Pointer_To_RECT;
      lpFrameInfo  : Pointer_To_OIFI)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceSite_GetWindowContext);
   pragma Machine_Attribute (IOleInPlaceSite_GetWindowContext, "ms_abi");

   function IOleInPlaceSite_Scroll
     (This         : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      scrollExtant : SIZE)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceSite_Scroll);
   pragma Machine_Attribute (IOleInPlaceSite_Scroll, "ms_abi");

   function IOleInPlaceSite_OnUIDeactivate
     (This      : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      fUndoable : Win32_Types.Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceSite_OnUIDeactivate);
   pragma Machine_Attribute (IOleInPlaceSite_OnUIDeactivate, "ms_abi");

   function IOleInPlaceSite_OnInPlaceDeactivate
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceSite_OnInPlaceDeactivate);
   pragma Machine_Attribute (IOleInPlaceSite_OnInPlaceDeactivate, "ms_abi");

   function IOleInPlaceSite_DiscardUndoState
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceSite_DiscardUndoState);
   pragma Machine_Attribute (IOleInPlaceSite_DiscardUndoState, "ms_abi");

   function IOleInPlaceSite_DeactivateAndUndo
     (This : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceSite_DeactivateAndUndo);
   pragma Machine_Attribute (IOleInPlaceSite_DeactivateAndUndo, "ms_abi");

   function IOleInPlaceSite_OnPosRectChange
     (This        : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      lprcPosRect : Pointer_To_RECT)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceSite_OnPosRectChange);
   pragma Machine_Attribute (IOleInPlaceSite_OnPosRectChange, "ms_abi");

   type IOleInPlaceSite_Vtbl_Record is
      record
         IUnknown             :
           GNATCOM.Create.COM_Interface.IUnknown_Vtbl_Record;
         GetWindow            : af_IOleInPlaceSite_GetWindow :=
           IOleInPlaceSite_GetWindow'Access;
         ContextSensitiveHelp : af_IOleInPlaceSite_ContextSensitiveHelp :=
           IOleInPlaceSite_ContextSensitiveHelp'Access;
         CanInPlaceActivate   : af_IOleInPlaceSite_CanInPlaceActivate :=
           IOleInPlaceSite_CanInPlaceActivate'Access;
         OnInPlaceActivate    : af_IOleInPlaceSite_OnInPlaceActivate :=
           IOleInPlaceSite_OnInPlaceActivate'Access;
         OnUIActivate         : af_IOleInPlaceSite_OnUIActivate :=
           IOleInPlaceSite_OnUIActivate'Access;
         GetWindowContext     : af_IOleInPlaceSite_GetWindowContext :=
           IOleInPlaceSite_GetWindowContext'Access;
         Scroll               : af_IOleInPlaceSite_Scroll :=
           IOleInPlaceSite_Scroll'Access;
         OnUIDeactivate       : af_IOleInPlaceSite_OnUIDeactivate :=
           IOleInPlaceSite_OnUIDeactivate'Access;
         OnInPlaceDeactivate  : af_IOleInPlaceSite_OnInPlaceDeactivate :=
           IOleInPlaceSite_OnInPlaceDeactivate'Access;
         DiscardUndoState     : af_IOleInPlaceSite_DiscardUndoState :=
           IOleInPlaceSite_DiscardUndoState'Access;
         DeactivateAndUndo    : af_IOleInPlaceSite_DeactivateAndUndo :=
           IOleInPlaceSite_DeactivateAndUndo'Access;
         OnPosRectChange      : af_IOleInPlaceSite_OnPosRectChange :=
           IOleInPlaceSite_OnPosRectChange'Access;
      end record;
   pragma Convention (C_Pass_By_Copy, IOleInPlaceSite_Vtbl_Record);

   type Pointer_To_IOleInPlaceSite_Vtbl_Record is
     access all IOleInPlaceSite_Vtbl_Record;

   IOleInPlaceSite_Vtbl : aliased IOleInPlaceSite_Vtbl_Record;

   function IOleContainer_ParseDisplayName
     (This           : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pbc            : Pointer_To_IBindCtx;
      pszDisplayName : GNATCOM.Types.LPWSTR;
      pchEaten       : GNATCOM.Types.Pointer_To_unsigned_long;
      ppmkOut        : Pointer_To_Pointer_To_IMoniker)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleContainer_ParseDisplayName);
   pragma Machine_Attribute (IOleContainer_ParseDisplayName, "ms_abi");

   function IOleContainer_EnumObjects
     (This     : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      grfFlags : Win32_Types.Unsigned_Long;
      ppenum   : Pointer_To_Pointer_To_IEnumUnknown)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleContainer_EnumObjects);
   pragma Machine_Attribute (IOleContainer_EnumObjects, "ms_abi");

   function IOleContainer_LockContainer
     (This  : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      fLock : Win32_Types.Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleContainer_LockContainer);
   pragma Machine_Attribute (IOleContainer_LockContainer, "ms_abi");

   type IOleContainer_Vtbl_Record is
      record
         IUnknown         : GNATCOM.Create.COM_Interface.IUnknown_Vtbl_Record;
         ParseDisplayName : af_IOleContainer_ParseDisplayName :=
           IOleContainer_ParseDisplayName'Access;
         EnumObjects      : af_IOleContainer_EnumObjects :=
           IOleContainer_EnumObjects'Access;
         LockContainer    : af_IOleContainer_LockContainer :=
           IOleContainer_LockContainer'Access;
      end record;
   pragma Convention (C_Pass_By_Copy, IOleContainer_Vtbl_Record);

   type Pointer_To_IOleContainer_Vtbl_Record is
     access all IOleContainer_Vtbl_Record;

   IOleContainer_Vtbl : aliased IOleContainer_Vtbl_Record;

   function IOleWindow_GetWindow
     (This  : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      phwnd : Pointer_To_HWND)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleWindow_GetWindow);
   pragma Machine_Attribute (IOleWindow_GetWindow, "ms_abi");

   function IOleWindow_ContextSensitiveHelp
     (This       : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      fEnterMode : Win32_Types.Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleWindow_ContextSensitiveHelp);
   pragma Machine_Attribute (IOleWindow_ContextSensitiveHelp, "ms_abi");

   type IOleWindow_Vtbl_Record is
      record
         IUnknown             :
           GNATCOM.Create.COM_Interface.IUnknown_Vtbl_Record;
         GetWindow            : af_IOleWindow_GetWindow :=
           IOleWindow_GetWindow'Access;
         ContextSensitiveHelp : af_IOleWindow_ContextSensitiveHelp :=
           IOleWindow_ContextSensitiveHelp'Access;
      end record;
   pragma Convention (C_Pass_By_Copy, IOleWindow_Vtbl_Record);

   type Pointer_To_IOleWindow_Vtbl_Record is
     access all IOleWindow_Vtbl_Record;

   IOleWindow_Vtbl : aliased IOleWindow_Vtbl_Record;

   function IOleInPlaceUIWindow_GetWindow
     (This  : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      phwnd : Pointer_To_HWND)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceUIWindow_GetWindow);
   pragma Machine_Attribute (IOleInPlaceUIWindow_GetWindow, "ms_abi");

   function IOleInPlaceUIWindow_ContextSensitiveHelp
     (This       : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      fEnterMode : Win32_Types.Long)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceUIWindow_ContextSensitiveHelp);
   pragma Machine_Attribute (IOleInPlaceUIWindow_ContextSensitiveHelp, "ms_abi");

   function IOleInPlaceUIWindow_GetBorder
     (This         : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      lprectBorder : Pointer_To_RECT)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceUIWindow_GetBorder);
   pragma Machine_Attribute (IOleInPlaceUIWindow_GetBorder, "ms_abi");

   function IOleInPlaceUIWindow_RequestBorderSpace
     (This          : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pborderwidths : Pointer_To_RECT)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceUIWindow_RequestBorderSpace);
   pragma Machine_Attribute (IOleInPlaceUIWindow_RequestBorderSpace, "ms_abi");

   function IOleInPlaceUIWindow_SetBorderSpace
     (This          : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pborderwidths : Pointer_To_RECT)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceUIWindow_SetBorderSpace);
   pragma Machine_Attribute (IOleInPlaceUIWindow_SetBorderSpace, "ms_abi");

   function IOleInPlaceUIWindow_SetActiveObject
     (This          : access
        GNATCOM.Create.COM_Interface.COM_Interface_Type;
      pActiveObject : Pointer_To_IOleInPlaceActiveObject;
      pszObjName    : GNATCOM.Types.LPWSTR)
     return GNATCOM.Types.HRESULT;
   pragma Convention (StdCall, IOleInPlaceUIWindow_SetActiveObject);
   pragma Machine_Attribute (IOleInPlaceUIWindow_SetActiveObject, "ms_abi");

   type IOleInPlaceUIWindow_Vtbl_Record is
      record
         IUnknown             :
           GNATCOM.Create.COM_Interface.IUnknown_Vtbl_Record;
         GetWindow            : af_IOleInPlaceUIWindow_GetWindow :=
           IOleInPlaceUIWindow_GetWindow'Access;
         ContextSensitiveHelp : af_IOleInPlaceUIWindow_ContextSensitiveHelp :=
           IOleInPlaceUIWindow_ContextSensitiveHelp'Access;
         GetBorder            : af_IOleInPlaceUIWindow_GetBorder :=
           IOleInPlaceUIWindow_GetBorder'Access;
         RequestBorderSpace   : af_IOleInPlaceUIWindow_RequestBorderSpace :=
           IOleInPlaceUIWindow_RequestBorderSpace'Access;
         SetBorderSpace       : af_IOleInPlaceUIWindow_SetBorderSpace :=
           IOleInPlaceUIWindow_SetBorderSpace'Access;
         SetActiveObject      : af_IOleInPlaceUIWindow_SetActiveObject :=
           IOleInPlaceUIWindow_SetActiveObject'Access;
      end record;
   pragma Convention (C_Pass_By_Copy, IOleInPlaceUIWindow_Vtbl_Record);

   type Pointer_To_IOleInPlaceUIWindow_Vtbl_Record is
     access all IOleInPlaceUIWindow_Vtbl_Record;

   IOleInPlaceUIWindow_Vtbl : aliased IOleInPlaceUIWindow_Vtbl_Record;

      GUID_Map : aliased GNATCOM.Create.COM_Interface.GUID_Record_Array :=
        (1 => (IID_IStorage, IStorage_Vtbl'Address),
         2 => (IID_IOleInPlaceFrame, IOleInPlaceFrame_Vtbl'Address),
         3 => (IID_IOleClientSite, IOleClientSite_Vtbl'Address),
         4 => (IID_IOleInPlaceSite, IOleInPlaceSite_Vtbl'Address),
         5 => (IID_IOleContainer, IOleContainer_Vtbl'Address),
         6 => (IID_IOleWindow, IOleWindow_Vtbl'Address),
         7 => (IID_IOleInPlaceUIWindow, IOleInPlaceUIWindow_Vtbl'Address));

   type GNATOCXClass_Type is
     new GNATCOM.Create.COM_Interface.CoClass_Type (GUID_Map'Access) with
      record
         OleObject : GNATOCX.IOleObject_Interface.IOleObject_Type;
         HWND      : System.Address;
      end record;

   type Pointer_To_GNATOCXClass_Type is
     access all GNATOCXClass_Type;

   function Create
     return GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type;

end GNATOCX_Site.Class;
