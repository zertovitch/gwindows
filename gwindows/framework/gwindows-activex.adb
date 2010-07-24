------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                    G W I N D O W S . A C T I V E X                       --
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
      Class_ID : GNATCOM.Types.GUID :=
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

      SiteObject_Interface :
        GNATCOM.Create.COM_Interface.Pointer_To_COM_Interface_Type :=
        GNATOCX_Site.Class.Create;
      SiteObject_CoClass   : GNATOCX_Site.Class.Pointer_To_GNATOCXClass_Type :=
        GNATOCX_Site.Class.Pointer_To_GNATOCXClass_Type
        (SiteObject_Interface.CoClass);

      Site_Interface      : aliased GNATCOM.Types.Pointer_To_Void;

      RIID    : aliased GNATCOM.Types.GUID;
      Result  : GNATCOM.Types.HRESULT;
      WHandle : aliased GWindows.Types.Handle;
      WRect   : aliased GNATOCX.RECT;
   begin
      SiteObject_CoClass.HWND := To_Addr (Handle (Window));

      WHandle := Handle (Window);

      WRect.left := 0;
      WRect.top := 0;
      WRect.right := Interfaces.C.long (GWindows.ActiveX.Width (Window));
      WRect.bottom := Interfaces.C.long (GWindows.ActiveX.Height (Window));

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
      WRect.right := Interfaces.C.long (Width);
      WRect.bottom := Interfaces.C.long (Height);

      SetObjectRects (OleObject,
                      WRect'Unchecked_Access,
                      WRect'Unchecked_Access);
   end On_Size;

end GWindows.ActiveX;
