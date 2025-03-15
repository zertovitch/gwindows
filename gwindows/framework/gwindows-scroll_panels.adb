------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                G W I N D O W S . S C R O L L _ P A N E L S               --
--                                                                          --
--                                 B o d y                                  --
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

package body GWindows.Scroll_Panels is

   -------------------------------------------------------------------------
   --  Package Body
   -------------------------------------------------------------------------

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Window : in out Inside_Panel_Type) is
      use GWindows.Base;

      Parent_Win : constant Pointer_To_Base_Window_Class := Parent (Window);
   begin
      if Parent_Win /= null then
         Close (Parent_Win.all);
      end if;
   end On_Destroy;

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Window : in out Scroll_Panel_Type) is
   begin
      Create_As_Control (Window.Panel, Window, "Panel",
                         0, 0,
                         Client_Area_Width (Window),
                         Client_Area_Height (Window));
      Show (Window.Panel);
   end On_Create;

   --------------------------
   -- On_Horizontal_Scroll --
   --------------------------

   procedure On_Horizontal_Scroll
     (Window  : in out Scroll_Panel_Type;
      Request : in     GWindows.Base.Scroll_Request_Type;
      Control : in     GWindows.Base.Pointer_To_Base_Window_Class)
   is
      use GWindows.Windows;
      use GWindows.Base;
   begin
      if Request = Thumb_Drag then
         Move (Window.Panel,
               0 - Scroll_Drag_Position (Window, Horizontal),
               0 - Scroll_Drag_Position (Window, Vertical));
      else
         Move (Window.Panel,
               0 - Scroll_Position (Window, Horizontal),
               0 - Scroll_Position (Window, Vertical));

         On_Horizontal_Scroll
           (GWindows.Windows.Window_Type (Window),
            Request,
            Control);
      end if;
   end On_Horizontal_Scroll;

   -------------
   -- On_Size --
   -------------

   procedure On_Size
     (Window : in out Scroll_Panel_Type;
      Width  : in     Integer;
      Height : in     Integer)
   is
      use GWindows.Windows;
      pragma Warnings (Off, Width);
      pragma Warnings (Off, Height);
   begin
      if
        Client_Area_Width (Window) < Client_Area_Width (Window.Panel)
      then
         Horizontal_Scroll_Bar (Window);
         Scroll_Range
           (Window, Horizontal, 0,
            Client_Area_Width (Window.Panel) -
            Client_Area_Width (Window) + 30);
         Scroll_Page_Size (Window, Horizontal, 30);
      else
         Left (Window.Panel, 0);
         Scroll_Position (Window, Horizontal, 0);
         Horizontal_Scroll_Bar (Window, False);
      end if;

      if
        Client_Area_Height (Window) < Client_Area_Height (Window.Panel)
      then
         Vertical_Scroll_Bar (Window);
         Scroll_Range
           (Window, Vertical, 0,
            Client_Area_Height (Window.Panel) -
            Client_Area_Height (Window) + 30);
         Scroll_Page_Size (Window, Vertical, 30);
      else
         Top (Window.Panel, 0);
         Scroll_Position (Window, Vertical, 0);
         Vertical_Scroll_Bar (Window, False);
      end if;

   end On_Size;

   ------------------------
   -- On_Vertical_Scroll --
   ------------------------

   procedure On_Vertical_Scroll
     (Window  : in out Scroll_Panel_Type;
      Request : in     GWindows.Base.Scroll_Request_Type;
      Control : in     GWindows.Base.Pointer_To_Base_Window_Class)

   is
      use GWindows.Windows;
      use GWindows.Base;
   begin
      if Request = Thumb_Drag then
         Move (Window.Panel,
               0 - Scroll_Drag_Position (Window, Horizontal),
               0 - Scroll_Drag_Position (Window, Vertical));
      else
         Move (Window.Panel,
               0 - Scroll_Drag_Position (Window, Horizontal),
               0 - Scroll_Position (Window, Vertical));

         On_Vertical_Scroll (GWindows.Windows.Window_Type (Window),
                             Request,
                             Control);
      end if;
   end On_Vertical_Scroll;

   ----------------
   -- Panel_Size --
   ----------------

   procedure Panel_Size (Window : in out Scroll_Panel_Type;
                         Width  : in     Integer;
                         Height : in     Integer)
   is
   begin
      Size (Window.Panel, Width, Height);
      On_Size (Window,
               GWindows.Scroll_Panels.Width (Window),
               GWindows.Scroll_Panels.Height (Window));
   end Panel_Size;

   ------------
   -- Create --
   ------------

   procedure Create
     (Window     : in out Scroll_Panel_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
   begin
      Create_As_Control (Window, Parent, "", Left, Top, Width, Height,
                         Show => Show,
                         Is_Dynamic => Is_Dynamic);
   end Create;

end GWindows.Scroll_Panels;
