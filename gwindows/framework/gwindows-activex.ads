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
