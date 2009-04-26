------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                    G W I N D O W S . A C T I V E X                       --
--                                                                          --
--                                 S p e c                                  --
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

with GWindows.Base;
with GWindows.Windows;

with GNATCOM.Iinterface;
with GNATCOM.Types;

package GWindows.ActiveX is

   -------------------------------------------------------------------------
   --  ActiveX_Type
   -------------------------------------------------------------------------

   type ActiveX_Type is new GWindows.Base.Base_Window_Type with private;
   type ActiveX_Access is access all ActiveX_Type;
   type Pointer_To_ActiveX_Class is access all ActiveX_Type'Class;

   -------------------------------------------------------------------------
   --  ActiveX_Type - Creation Methods
   -------------------------------------------------------------------------
   --  ActiveX controls can only be created in the main Single Threaded
   --  Apartment (STA) of an application (ie. the main thread of execution
   --  after GNATCOM.Initialize.Initialize_COM has been called). Once it
   --  is created though, the Interface function can be called in any thread
   --  that has been initialized for COM (be it an MTA or alternate STA) to
   --  return an interface pointer usable in that thread / apartment.

   procedure Create (Control : in out ActiveX_Type;
                     Parent  : in out GWindows.Base.Base_Window_Type'Class;
                     CLSID   : in     GNATCOM.Types.GUID;
                     Left    : in     Integer;
                     Top     : in     Integer;
                     Width   : in     Integer;
                     Height  : in     Integer;
                     Key     : in     GNATCOM.Types.BSTR := null);

   procedure Create (Control : in out ActiveX_Type;
                     Parent  : in out GWindows.Base.Base_Window_Type'Class;
                     ProgID  : in     GString;
                     Left    : in     Integer;
                     Top     : in     Integer;
                     Width   : in     Integer;
                     Height  : in     Integer;
                     Key     : in     GNATCOM.Types.BSTR := null);

   --  Creates and ActiveX control
   --  In order to be able to use an ActiveX control, you have to have
   --  rights to do so. If you have an ActiveX control with no licensing
   --  or have a development version license on your machine than leaving
   --  Key = null will work. Most operating system related ActiveX controls
   --  such as Internet Explorer or Internet based ActiveX controls
   --  Adobe Acrobat Reader do not have any Keys. The way to obtain a key
   --  to an ActiveX control is on a machine where you have installed the
   --  development license, create a simple app that calls
   --  GNATCOM.Interface.Get_Key with the CLSID of the object. They Key that
   --  is returned you can then use in the above procedures. This will allow
   --  the ActiveX control to work on any machine, ie. you can now distribute
   --  your OCX and EXE together and they will work even if there is no
   --  development license on the target machine.

   -------------------------------------------------------------------------
   --  ActiveX_Type - Properties
   -------------------------------------------------------------------------

   function Interfac (Control : in ActiveX_Type)
                      return GNATCOM.Iinterface.Interface_Type;

   -------------------------------------------------------------------------
   --  ActiveX_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  When overiding events, to insure that the event handlers will still
   --  be executed when set by users, call the base class or fire the event
   --  handler directly.

   procedure On_Create (Window : in out ActiveX_Type);
   --  Handles creation once window is ready

   procedure On_Destroy (Window : in out ActiveX_Type);
   --  Handles clean up of control

   procedure On_Size (Window : in out ActiveX_Type;
                      Width  : in     Integer;
                      Height : in     Integer);
   --  Handles changes in size to control
private

   type ActiveX_Type is new GWindows.Windows.Window_Type with
      record
         Cookie : GNATCOM.Iinterface.GIT_Cookie := 0;
         CLSID  : GNATCOM.Types.GUID;
         Key    : GNATCOM.Types.BSTR := null;
      end record;

end GWindows.ActiveX;
