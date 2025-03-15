------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                      G W I N D O W S . P A N E L S                       --
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

package body GWindows.Drawing_Panels is

   ---------------
   -- On_Create --
   ---------------

   procedure On_Create (Window : in out Drawing_Panel_Type)
   is
      use GWindows.Windows;
   begin
      On_Create (Window_Type (Window));
      Resize_Canvas (Window, False);
   end On_Create;

   ----------------
   -- Get_Canvas --
   ----------------

   procedure Get_Canvas
     (Window : in Drawing_Panel_Type;
      Canvas : in out Drawing_Canvas_Type'Class)
   is
      use GWindows.Drawing;
   begin
      Handle (Canvas, Handle (Window.Memory_Canvas));
      Canvas.Parent := Handle (Window);
   end Get_Canvas;

   --------------
   -- On_Paint --
   --------------

   procedure On_Paint
     (Window : in out Drawing_Panel_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type)
   is
      use GWindows.Drawing;
      pragma Warnings (Off, Area);
   begin
      BitBlt (Canvas, 0, 0, Window.Width, Window.Height,
              Window.Memory_Canvas, 0, 0);
   end On_Paint;

   -------------
   -- On_Size --
   -------------

   procedure On_Size
     (Window : in out Drawing_Panel_Type;
      Width  : in     Integer;
      Height : in     Integer)
   is
      use GWindows.Windows;
   begin
      if Window.Auto_Resize then
         Resize_Canvas (Window);
      end if;

      On_Size (Window_Type (Window), Width, Height);
   end On_Size;

   -------------------
   -- Resize_Canvas --
   -------------------

   procedure Resize_Canvas (Window : in out Drawing_Panel_Type;
                            Width  : in     Integer;
                            Height : in     Integer;
                            Copy   : in     Boolean            := True)
   is
      use GWindows.Drawing;
      use GWindows.Drawing_Objects;

      Drawing_Area : Canvas_Type;
      New_Canvas   : Memory_Canvas_Type;
      New_Bitmap   : Bitmap_Type;
   begin
      if not (Width = Window.Width and Height = Window.Height) then
         Get_Canvas (Window, Drawing_Area);

         Create_Memory_Canvas (New_Canvas, Drawing_Area);

         Create_Compatible_Bitmap (Drawing_Area, New_Bitmap,
                                   Width, Height);

         Select_Object (New_Canvas, New_Bitmap);

         if Copy then
            BitBlt (New_Canvas, 0, 0, Window.Width, Window.Height,
                    Window.Memory_Canvas, 0, 0);
         end if;

         Release (Window.Memory_Canvas);
         Delete (Window.Bitmap);

         Handle (Window.Memory_Canvas, Handle (New_Canvas));
         Handle (Window.Bitmap, Handle (New_Bitmap));

         Handle (New_Canvas, GWindows.Types.Null_Handle);
         Handle (New_Bitmap, GWindows.Types.Null_Handle);

         Window.Width := Width;
         Window.Height := Height;
      end if;
   end Resize_Canvas;

   procedure Resize_Canvas
     (Window : in out Drawing_Panel_Type;
      Copy   : in     Boolean            := True)
   is
      New_Width    : constant Integer := Client_Area_Width (Window);
      New_Height   : constant Integer := Client_Area_Height (Window);
   begin
      Resize_Canvas (Window, New_Width, New_Height, Copy);
   end Resize_Canvas;

   -------------------------
   -- On_Erase_Background --
   -------------------------

   procedure On_Erase_Background
     (Window : in out Drawing_Panel_Type;
      Canvas : in out GWindows.Drawing.Canvas_Type;
      Area   : in     GWindows.Types.Rectangle_Type)
   is
      pragma Warnings (Off, Window);
      pragma Warnings (Off, Canvas);
      pragma Warnings (Off, Area);
   begin
      --  Prevent redraw of background
      null;
   end On_Erase_Background;

   -----------------
   -- Auto_Resize --
   -----------------

   procedure Auto_Resize (Window : in out Drawing_Panel_Type;
                          Value  : in     Boolean            := True)
   is
   begin
      Window.Auto_Resize := Value;
   end Auto_Resize;

   function Auto_Resize (Window : in Drawing_Panel_Type) return Boolean
   is
   begin
      return Window.Auto_Resize;
   end Auto_Resize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Canvas : in out Drawing_Canvas_Type)
   is
      --  Since it is possible to have a global Canvas that is
      --  finalized _after_ the parent window, we avoid using
      --  an access to the parent window and instead use a
      --  work around using windows handles that even if the
      --  window is destroyed will not result in errors.

      procedure InvalidateRect
        (Hwnd   : GWindows.Types.Handle := Canvas.Parent;
         lpRect : Integer               := 0;
         bErase : Integer               := 0);
      pragma Import (StdCall, InvalidateRect, "InvalidateRect");

   begin
      InvalidateRect;
   end Finalize;

   ------------
   -- Create --
   ------------

   procedure Create
     (Window     : in out Drawing_Panel_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
   begin
      GWindows.Windows.Create_As_Control
        (GWindows.Windows.Window_Type (Window),
         Parent,
         Left       => Left,
         Top        => Top,
         Width      => Width,
         Height     => Height,
         Show       => Show,
         Is_Dynamic => Is_Dynamic);
   end Create;

end GWindows.Drawing_Panels;
