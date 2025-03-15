------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                    G W I N D O W S . A C T I V E X                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2022 David Botton                   --
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
-- More information about GWindows and the latest current release can       --
-- be located on the web at http://www.gnavi.org/gwindows                   --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
with Interfaces.C;
with System;

with GWindows.GStrings;

with GNATCOM.GUID;
with GNATCOM.Create.COM_Interface;

with GNATOCX.IOleObject_Interface;
with GNATOCX.IOleInPlaceObject_Interface;
with GNATOCX_Site.Class;
with GWindows.Types;

package body GWindows.ActiveX is
   OLEIVERB_PRIMARY            : constant := 0;
   --     OLEIVERB_SHOW               : constant := -1;
   --     OLEIVERB_OPEN               : constant := -2;
   --     OLEIVERB_HIDE               : constant := -3;
   OLEIVERB_UIACTIVATE         : constant := -4;
   --     OLEIVERB_INPLACEACTIVATE    : constant := -5;
   --     OLEIVERB_DISCARDUNDOSTATE   : constant := -6;

   --   OLECLOSE_SAVEIFDIRTY       : constant :=  0;
   OLECLOSE_NOSAVE      : constant :=  1;
   --   OLECLOSE_PROMPTSAVE        : constant :=  2;

   -------------------------------------------------------------------------
   --  Package Body
   -------------------------------------------------------------------------

   ------------
   -- Create --
   ------------

   procedure Create (Control : in out ActiveX_Type;
                     Parent  : in out GWindows.Base.Base_Window_Type'Class;
                     CLSID   : in     GNATCOM.Types.GUID;
                     Left    : in     Integer;
                     Top     : in     Integer;
                     Width   : in     Integer;
                     Height  : in     Integer;
                     Key     : in     GNATCOM.Types.BSTR := null)
   is
   begin
      Control.CLSID := CLSID;
      Control.Key := Key;

      Create_As_Control (Control,
                         Parent,
                         "",
                         Left, Top, Width, Height);
      Visible (Control);
   end Create;

   procedure Create (Control : in out ActiveX_Type;
                     Parent  : in out GWindows.Base.Base_Window_Type'Class;
                     ProgID  : in     GString;
                     Left    : in     Integer;
                     Top     : in     Integer;
                     Width   : in     Integer;
                     Height  : in     Integer;
                     Key     : in     GNATCOM.Types.BSTR := null)
   is
      Class_ID : constant GNATCOM.Types.GUID :=
        GNATCOM.GUID.To_GUID (GWindows.GStrings.To_String (ProgID));
   begin
      Create (Control, Parent, Class_ID, Left, Top, Width, Height, Key);
   end Create;

   ---------------
   -- Interface --
   ---------------

   function Interfac (Control : in ActiveX_Type)
                      return GNATCOM.Iinterface.Interface_Type
   is
      IUnknown : GNATCOM.Iinterface.Interface_Type;
   begin
      GNATCOM.Iinterface.Attach_From_GIT (IUnknown, Control.Cookie);
      return IUnknown;
   end Interfac;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Window : in out ActiveX_Type)
   is
   begin
      declare
         use GNATOCX.IOleObject_Interface;

         OleObject : GNATOCX.IOleObject_Interface.IOleObject_Type;
      begin
         Attach_From_GIT (OleObject, Window.Cookie);
         Close (OleObject, OLECLOSE_NOSAVE);
      end;

      GNATCOM.Iinterface.Remove_From_GIT (Window.Cookie);

      GWindows.Base.On_Destroy (GWindows.Base.Base_Window_Type (Window));
   end On_Destroy;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Window : in out ActiveX_Type)
   is
      use type Interfaces.C.long;
      use type GNATCOM.Types.BSTR;

      use GNATOCX.IOleObject_Interface;
      IUnknown : GNATCOM.Iinterface.Interface_Type;

      function To_Pointer_To_IOleClientSite is
         new Ada.Unchecked_Conversion
          (GNATCOM.Types.Pointer_To_Void, GNATOCX.Pointer_To_IOleClientSite);
      function To_Addr is
         new Ada.Unchecked_Conversion (GWindows.Types.Handle, System.Address);

      OleObject : GNATOCX.IOleObject_Interface.IOleObject_Type;

      SiteObject_Interface : constant
        GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type :=
        GNATOCX_Site.Class.Create;
      SiteObject_CoClass   : constant
        GNATOCX_Site.Class.Pointer_To_GNATOCXClass_Type :=
        GNATOCX_Site.Class.Pointer_To_GNATOCXClass_Type
        (SiteObject_Interface.CoClass);

      Site_Interface      : aliased GNATCOM.Types.Pointer_To_Void;

      RIID    : aliased GNATCOM.Types.GUID;
      Result  : GNATCOM.Types.HRESULT;
      pragma Unreferenced (Result);
      WHandle : aliased GWindows.Types.Handle;
      WRect   : aliased GNATOCX.RECT;
   begin
      SiteObject_CoClass.HWND := To_Addr (Handle (Window));

      WHandle := Handle (Window);

      WRect.left := 0;
      WRect.top := 0;
      WRect.right := Interfaces.C.int (GWindows.ActiveX.Width (Window));
      WRect.bottom := Interfaces.C.int (GWindows.ActiveX.Height (Window));

      RIID := GNATOCX.IID_IOleClientSite;
      Result := GNATCOM.Create.COM_Interface.QueryInterface
        (SiteObject_Interface,
         RIID'Unchecked_Access,
         Site_Interface'Access);

      if Window.Key /= null then
         Create (OleObject, Window.CLSID, Window.Key);
      else
         Create (OleObject, Window.CLSID);
      end if;
      SetClientSite (OleObject,
                     To_Pointer_To_IOleClientSite (Site_Interface));

      DoVerb (OleObject, OLEIVERB_UIACTIVATE, null,
              To_Pointer_To_IOleClientSite (Site_Interface),
              -1, To_Addr (WHandle), WRect'Unchecked_Access);

      DoVerb (OleObject, OLEIVERB_PRIMARY, null,
              To_Pointer_To_IOleClientSite (Site_Interface),
              -1, To_Addr (WHandle), WRect'Unchecked_Access);

      GNATCOM.Iinterface.Query (IUnknown, OleObject);
      Window.Cookie := GNATCOM.Iinterface.Put_In_GIT (IUnknown);
   end On_Create;

   -------------
   -- On_Size --
   -------------

   procedure On_Size (Window : in out ActiveX_Type;
                      Width  : in     Integer;
                      Height : in     Integer)
   is
      pragma Warnings (Off, Window);
      use GNATOCX.IOleInPlaceObject_Interface;

      OleObject : GNATOCX.IOleInPlaceObject_Interface.IOleInPlaceObject_Type;
      WRect   : aliased GNATOCX.RECT;
   begin
      Attach_From_GIT (OleObject, Window.Cookie);

      WRect.left := 0;
      WRect.top := 0;
      WRect.right := Interfaces.C.int (Width);
      WRect.bottom := Interfaces.C.int (Height);

      SetObjectRects (OleObject,
                      WRect'Unchecked_Access,
                      WRect'Unchecked_Access);
   end On_Size;

end GWindows.ActiveX;
