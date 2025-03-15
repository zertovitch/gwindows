------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                G W I N D O W S . S C R O L L _ P A N E L S               --
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
with GWindows.Panels;

package GWindows.Scroll_Panels is

   -------------------------------------------------------------------------
   --  Inside_Panel_Type
   -------------------------------------------------------------------------
   --  This is the type of the panel contained in the Scroll Panel

   type Inside_Panel_Type is
     new GWindows.Panels.Panel_Type with null record;

   -------------------------------------------------------------------------
   --  Scroll_Panel_Type
   -------------------------------------------------------------------------
   --  Scroll_Panel_Type can also be used to create a regular window that
   --  scrolls its contents which are placed on the inside panel

   type Scroll_Panel_Type is
     new GWindows.Windows.Window_Type with
      record
         Panel : Inside_Panel_Type;
      end record;
   type Scroll_Panel_Access is access all Scroll_Panel_Type;
   type Pointer_To_Scroll_Panel_Class is access all Scroll_Panel_Type'Class;

   -------------------------------------------------------------------------
   --  Scroll_Panel_Type - Creation Methods
   -------------------------------------------------------------------------

   procedure Create
     (Window     : in out Scroll_Panel_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False);
   --  Create scroll panel control

   -------------------------------------------------------------------------
   --  Scroll_Panel_Type - Methods
   -------------------------------------------------------------------------

   procedure Panel_Size (Window : in out Scroll_Panel_Type;
                         Width  : in     Integer;
                         Height : in     Integer);
   --  Using this method to set the panel size will insure that scroll bars
   --  are updated if needed on the Scroll Panel. You can also set the
   --  size of the inside panel directly, but the scroll bars will not
   --  be adjusted for the Scroll Panel until the next On_Size event is
   --  received.

   -------------------------------------------------------------------------
   --  Scroll_Panel_Type - Event Framework Methods
   -------------------------------------------------------------------------
   --  When overiding events, to insure that the event handlers will still
   --  be executed when set by users, call the base class or fire the event
   --  handler directly.

   procedure On_Create (Window : in out Scroll_Panel_Type);
   --  Handles creation of internal panel

   procedure On_Vertical_Scroll
     (Window  : in out Scroll_Panel_Type;
      Request : in     GWindows.Base.Scroll_Request_Type;
      Control : in     GWindows.Base.Pointer_To_Base_Window_Class);
   --  Handles vertical scroll

   procedure On_Horizontal_Scroll
     (Window  : in out Scroll_Panel_Type;
      Request : in     GWindows.Base.Scroll_Request_Type;
      Control : in     GWindows.Base.Pointer_To_Base_Window_Class);
   --  Handles horizontal scroll

   procedure On_Size (Window : in out Scroll_Panel_Type;
                      Width  : in     Integer;
                      Height : in     Integer);
   --  Handles size changes

   procedure On_Destroy (Window : in out Inside_Panel_Type);
   --  Handles being destroyed by contained Cancel_Button_Type's

end GWindows.Scroll_Panels;
