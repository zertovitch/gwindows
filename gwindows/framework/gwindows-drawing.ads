------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                     G W I N D O W S . D R A W I N G                      --
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

with Ada.Finalization;
with Interfaces.C;

with GWindows.Types;
with GWindows.Colors;
with GWindows.Drawing_Objects;

package GWindows.Drawing is

   -------------------------------------------------------------------------
   --  Information_Canvas_Type
   -------------------------------------------------------------------------
   --  The information Canvas is used when there is no need to do drawing
   --  just to rerieve properties of the canvas

   type Information_Canvas_Type is
     new Ada.Finalization.Limited_Controlled with private;
   --  A canvas that can not be drawn upon, but can be used to query
   --  capabilities using package GWindows.Drawing.Capabilities

   type Canvas_State_Type is new Integer;

   function Save_State (Canvas : in Information_Canvas_Type)
                       return Canvas_State_Type;
   procedure Load_State (Canvas : in out Information_Canvas_Type;
                        State  : in     Canvas_State_Type);
   --  Store state of canvas

   procedure Capture (Canvas : in out Information_Canvas_Type;
                      HWND   : in     GWindows.Types.Handle;
                      HDC    : in     GWindows.Types.Handle);
   --  Capture's device context and releases it when
   --  canvas is finalized

   procedure Release (Canvas : in out Information_Canvas_Type);
   --  Release the device context

   procedure Handle (Canvas : in out Information_Canvas_Type;
                     HDC    : in     GWindows.Types.Handle);
   --  Setting the handle should only be done with full understanding
   --  of the GWindows framework. If a handle is set on a new Canvas
   --  object, the previous state will not be stored or restored, nor
   --  will the device context be released unless a previous caputre
   --  was performed.
   function Handle (Canvas : in Information_Canvas_Type)
                   return GWindows.Types.Handle;
   --  Win32 Handle to device context

   procedure Initialize (Object : in out Information_Canvas_Type);
   procedure Finalize (Object : in out Information_Canvas_Type);
   --  Insures that internal context is restored to original state

   -------------------------------------------------------------------------
   --  Canvas_Type
   -------------------------------------------------------------------------
   --  A drawable canvas on a Window
   --  Drawing on canvas of a window will be erased under many conditions
   --  code should be added to redraw the canvas in the On_Paint event or
   --  use a Memory_Canvas and bitmap to store the image and restore
   --  from it later in the On_Paint

   type Canvas_Type is new Information_Canvas_Type with private;

   procedure Fill_Rectangle
     (Canvas      : in out Canvas_Type;
      Rectangle   : in     GWindows.Types.Rectangle_Type;
      Color_Const : in     Integer);
   --  Fill Rectangle using a System Color Constant
   --  Excludes bottom and right edge

   procedure Fill_Rectangle
     (Canvas    : in out Canvas_Type;
      Rectangle : in     GWindows.Types.Rectangle_Type;
      Brush     : in     GWindows.Drawing_Objects.Brush_Type);
   --  Excludes bottom and right edge

   procedure Frame_Rectangle
     (Canvas    : in out Canvas_Type;
      Rectangle : in     GWindows.Types.Rectangle_Type;
      Brush     : in     GWindows.Drawing_Objects.Brush_Type);
   --  Excludes bottom and right edge

   procedure Invert_Rectangle
     (Canvas    : in out Canvas_Type;
      Rectangle : in     GWindows.Types.Rectangle_Type);
   --  Excludes bottom and right edge

   procedure Rectangle (Canvas                   : in out Canvas_Type;
                        Left, Top, Right, Bottom : in     Integer);
   --  Draw a rectangle with current pen and fill with current brush
   --  Excludes bottom and right edge

   type ThreeD_Type is (Raised, Sunken);

   procedure Rectangle_3D
     (Canvas                   : in out Canvas_Type;
      Left, Top, Right, Bottom : in     Integer;
      Direction                : in     ThreeD_Type := Raised;
      Thickness                : in     Integer     := 1);
   --  Draw a 3D rectangle using COLOR_BTNHILIGHT for top left side
   --  and COLOR_BTNSHADOW for bottom right side

   procedure Rectangle_3D
     (Canvas               : in out Canvas_Type;
      Left, Top,
        Right, Bottom      : in     Integer;
      Top_Left_Color,
        Bottom_Right_Color : in     GWindows.Colors.Color_Type;
      Thickness            : in     Integer                   := 1);
   --  Draw a 3D rectangle

   procedure Rounded_Rectangle (Canvas                    : in out Canvas_Type;
                                Left, Top, Right, Bottom,
                                  Ellipse_Width,
                                  Ellipse_Height          : in     Integer);
   --  Draw a rounded rectangle with current pen and fill with current brush

   procedure Chord (Canvas                     : in out Canvas_Type;
                    Left, Top, Right, Bottom,
                      XRadial1, YRadial1,
                      XRadial2, YRadial2       : in Integer);
   --  Draw a chord with current pen and fill with current brush

   procedure Ellipse (Canvas                   : in out Canvas_Type;
                      Left, Top, Right, Bottom : in     Integer);
   --  Draw an ellipse with current pen and fill with current brush

   procedure Pie (Canvas                    : in out Canvas_Type;
                  Left, Top, Right, Bottom,
                    XRadial1, YRadial1,
                    XRadial2, YRadial2      : in     Integer);
   --  Draw a pie with current pen and fill with current brush

   procedure Polygon (Canvas   : in out Canvas_Type;
                      Vertices : in     GWindows.Types.Point_Array_Type);
   --  Draw a pie with current pen and fill with current brush
   --  Polygon is autmoticly closed by drawing a line between first and last
   --  vertices

   procedure Put (Canvas : in out Canvas_Type;
                  X, Y   : in     Integer;
                  Text   : in     GString);
   --  Put text on canvas

   procedure Put (Canvas    : in out Canvas_Type;
                  X, Y      : in     Integer;
                  Text      : in     GString;
                  Clip_Area : in     GWindows.Types.Rectangle_Type);
   --  Put text on canvas with clipping path

   procedure Put (Canvas : in out Canvas_Type;
                  Text   : in     GString);
   --  Put text on canvas at current position

   procedure Put (Canvas    : in out Canvas_Type;
                  Text      : in     GString;
                  Clip_Area : in     GWindows.Types.Rectangle_Type);
   --  Put text on canvas with clipping path at current position

   function Text_Output_Size (Canvas : in Canvas_Type;
                              Text   : in GString)
                             return GWindows.Types.Size_Type;
   --  Calculates width and height of text in graphic units

   function Point (Canvas : in Canvas_Type;
                   X, Y   : in Integer)
                  return GWindows.Colors.Color_Type;
   --  Returns the color at X, Y

   procedure Point (Canvas : in out Canvas_Type;
                    X, Y   : in     Integer;
                    Color  : in     GWindows.Colors.Color_Type);
   --  Sets the color at x, y

   procedure Line (Canvas         : in out Canvas_Type;
                   X1, Y1, X2, Y2 : in     Integer);
   --  Draw line from X1, Y1 to X2, Y2 using current pen

   procedure Arc (Canvas : in out Canvas_Type;
                  Left, Top, Right, Bottom,
                    XStartArc, YStartArc, XEndArc, YEndArc : Integer);
   --  Draw arc with current pen

   procedure Beziers (Canvas : in out Canvas_Type;
                      Points : in     GWindows.Types.Point_Array_Type);
   --  Draw bezier curves using current pen
   --  Draws cubic Bezier curves by using the endpoints and control
   --  points specified by the points parameter. The first curve is drawn
   --  from the first point to the fourth point by using the second and
   --  third points as control points. Each subsequent curve in the
   --  sequence needs exactly three more points: the ending point of
   --  the previous curve is used as the starting point, the next two
   --  points in the sequence are control points, and the third is the
   --  ending point.

   procedure Lines (Canvas : in out Canvas_Type;
                    Points : in     GWindows.Types.Point_Array_Type);
   --  Draw lines using current pen

   type Draw_Direction_Type is (Counter_Clock_Wise, Clock_Wise);

   procedure Draw_Direction (Canvas    : in out Canvas_Type;
                             Direction : in     Draw_Direction_Type);
   --  Sets drawing direction for Arc, Chord, Ellipse, Pie, Rectangle,
   --  Rounded_Rectangle

   type Vertical_Alignment_Type is (Base_Line, Bottom, Top);

   procedure Vertical_Text_Alignment
     (Canvas               : in out Canvas_Type;
      Alignment            : in     Vertical_Alignment_Type;
      Use_Current_Position : in     Boolean                 := False);
   function Vertical_Text_Alignment (Canvas : in Canvas_Type)
                                    return Vertical_Alignment_Type;

   type Horizontal_Alignment_Type is (Center, Left, Right);

   procedure Horizontal_Text_Alignment
     (Canvas               : in out Canvas_Type;
      Alignment            : in     Horizontal_Alignment_Type;
      Use_Current_Position : in     Boolean                   := False);
   function Horizontal_Text_Alignment (Canvas : in Canvas_Type)
                                      return Horizontal_Alignment_Type;

   procedure Text_Color
     (Canvas : in out Canvas_Type;
      Color  : in     GWindows.Colors.Color_Type);
   function Text_Color (Canvas : in Canvas_Type)
                       return GWindows.Colors.Color_Type;

   function Current_Font_Name (Canvas : in Canvas_Type) return GString;

   type Font_Metrics_Type is
      record
         Height                  : Integer;
         Ascent                  : Integer;
         Descent                 : Integer;
         Internal_Leading        : Integer;
         External_Leading        : Integer;
         Average_Character_Width : Integer;
         Maximum_Character_Width : Integer;
         Weight                  : Integer;
         Overhang                : Integer;
         Digitized_Aspect_X      : Integer;
         Digitized_Aspect_Y      : Integer;
         First_Character         : GCharacter;
         Last_Character          : GCharacter;
         Default_Character       : GCharacter;
         Word_Break_Character    : GCharacter;
         Italic                  : Boolean;
         Underlined              : Boolean;
         Struck_Out              : Boolean;
      end record;

   function Current_Font_Metrics (Canvas : in Canvas_Type)
                                 return Font_Metrics_Type;

   function Current_Font_Is_True_Type (Canvas : in Canvas_Type) return Boolean;

   function Current_Font_Is_Vector (Canvas : in Canvas_Type) return Boolean;

   function Current_Font_Is_Variable_Width (Canvas : in Canvas_Type)
                                           return Boolean;

   function Current_Font_Is_Device_Font (Canvas : in Canvas_Type)
                                        return Boolean;

   procedure Background_Color
     (Canvas : in out Canvas_Type;
      Color  : in     GWindows.Colors.Color_Type);
   function Background_Color (Canvas : in Canvas_Type)
                             return GWindows.Colors.Color_Type;

   type Background_Mode_Type is (Opaque, Transparent);

   procedure Background_Mode (Canvas : in out Canvas_Type;
                              Mode   : in     Background_Mode_Type);
   function Background_Mode (Canvas : in Canvas_Type)
                            return Background_Mode_Type;

   R2_BLACK                        : constant := 1;
   R2_NOTMERGEPEN                  : constant := 2;
   R2_MASKNOTPEN                   : constant := 3;
   R2_NOTCOPYPEN                   : constant := 4;
   R2_MASKPENNOT                   : constant := 5;
   R2_NOT                          : constant := 6;
   R2_XORPEN                       : constant := 7;
   R2_NOTMASKPEN                   : constant := 8;
   R2_MASKPEN                      : constant := 9;
   R2_NOTXORPEN                    : constant := 10;
   R2_NOP                          : constant := 11;
   R2_MERGENOTPEN                  : constant := 12;
   R2_COPYPEN                      : constant := 13;
   R2_MERGEPENNOT                  : constant := 14;
   R2_MERGEPEN                     : constant := 15;
   R2_WHITE                        : constant := 16;
   --  Standard raster operation mix modes

   procedure Set_Mix_Mode (Canvas : in Canvas_Type;
                           Mode   : in Natural);
   --  Sets color mixture mode

   SRCCOPY                         : constant := 13369376;
   --  Copy source bits over destination bits
   SRCPAINT                        : constant := 15597702;
   --  OR source bits onto destination
   SRCAND                          : constant := 8913094;
   --  AND source bits onto destination
   SRCINVERT                       : constant := 6684742;
   --  XOR source bits onto destination
   SRCERASE                        : constant := 4457256;
   --  Invert destination bits, AND the result with source bits
   NOTSRCCOPY                      : constant := 3342344;
   --  Invert source bits, copy to destination
   NOTSRCERASE                     : constant := 1114278;
   --  OR source bits onto destination bits, invert result
   MERGECOPY                       : constant := 12583114;
   --  AND source bits onto pattern bits, copy to destination
   MERGEPAINT                      : constant := 12255782;
   --  Invert source bits, AND result onto destination
   PATCOPY                         : constant := 15728673;
   --  Copy pattern bits over destination bits
   PATPAINT                        : constant := 16452105;
   --  Invert source bits, OR result with pattern, OR result with
   --  destination
   PATINVERT                       : constant := 5898313;
   --  XOR pattern bits onto destination
   DSTINVERT                       : constant := 5570569;
   --  Invert destination bits
   BLACKNESS                       : constant := 66;
   --  Turn destination bits black (0)
   WHITENESS                       : constant := 16711778;
   --  Turn destination bits white (1)

   --  Standard Raster Operation Codes

   procedure BitBlt
     (Destination_Canvas                    : in out Canvas_Type;
      Destination_X, Destination_Y          : in     Integer;
      Destination_Width, Destination_Height : in     Integer;
      Source_Canvas                         : in     Canvas_Type'Class;
      Source_X, Source_Y                    : in     Integer;
      Raster_Operation_Code
        : in Interfaces.C.unsigned := SRCCOPY);
   --  Do a BitBlt (Copy a set of pixels from one canvas to another)

   procedure StretchBlt
     (Destination_Canvas                    : in out Canvas_Type;
      Destination_X, Destination_Y          : in     Integer;
      Destination_Width, Destination_Height : in     Integer;
      Source_Canvas                         : in     Canvas_Type'Class;
      Source_X, Source_Y                    : in     Integer;
      Source_Width, Source_Height           : in     Integer;
      Raster_Operation_Code
        : in     Interfaces.C.unsigned := SRCCOPY);
   --  Do a StretchBlt (Copy a set of pixels from one canvas to another
   --  stretching or shrinking the image as needed)

   procedure Create_Compatible_Bitmap
     (Canvas        : in     Canvas_Type;
      Bitmap        : in out GWindows.Drawing_Objects.Bitmap_Type;
      Width, Height : in     Natural);
   --  Creates a bitmap that matches this Canvas in a way that can be edited
   --  by selecting it in to a Memory_Canvas and later blt'ed on to this
   --  Canvas

   procedure Paint_Bitmap
     (Canvas                : in out Canvas_Type;
      Bitmap                : in out GWindows.Drawing_Objects.Bitmap_Type;
      X, Y, Width, Height   : in     Integer;
      Raster_Operation_Code :
        in     Interfaces.C.unsigned              := SRCCOPY);
   --  Paint width and height of bitmap at X, Y on Canvas

   procedure Paint_Icon (Canvas : in out Canvas_Type;
                         Icon   : in out GWindows.Drawing_Objects.Icon_Type;
                         X, Y   : in     Integer);
   --  Paint icon at X, Y

   procedure Select_Object
     (Canvas : in out Canvas_Type;
      Object : in     GWindows.Drawing_Objects.Drawing_Object_Type'Class);
   --  Select a drawing object on to canvas

   function Y_Pixels_Per_Inch (Canvas : in Canvas_Type) return Integer;
   function X_Pixels_Per_Inch (Canvas : in Canvas_Type) return Integer;
   --  Calculates the logical pixels per inch

   function Point_Size (Canvas : in Canvas_Type;
                        Size   : in Natural)
                       return Integer;
   --  Calculates the the size of a font of Point_Size for this device

   function Clipping_Area (Canvas : in Canvas_Type)
                          return GWindows.Types.Rectangle_Type;
   --  Returns the current clipping
   --  (ie. visible, drawable, non-clipped out, area)

   procedure Include_Clipping_Area
     (Canvas : in out Canvas_Type;
      Left, Top, Right, Bottom : Integer);
   --  Includes an area in to the current clipping region

   procedure Exclude_Clipping_Area
     (Canvas : in out Canvas_Type;
      Left, Top, Right, Bottom : Integer);
   --  Excludes an area from the current clipping region

   function Inside_Clipping_Area (Canvas : in Canvas_Type;
                                  X, Y   : in Integer)
                                 return Boolean;
   --  True if (x,y) in clipping area

   function Inside_Clipping_Area (Canvas                   : in Canvas_Type;
                                  Left, Top, Right, Bottom : in Integer)
                                 return Boolean;
   --  True if any part of rectangular area is in the clipping area

   MM_TEXT                         : constant := 1;
   MM_LOMETRIC                     : constant := 2;
   MM_HIMETRIC                     : constant := 3;
   MM_LOENGLISH                    : constant := 4;
   MM_HIENGLISH                    : constant := 5;
   MM_TWIPS                        : constant := 6;
   MM_ISOTROPIC                    : constant := 7;
   MM_ANISOTROPIC                  : constant := 8;

   procedure Map_Mode (Canvas : in out Canvas_Type;
                       Mode   : in     Integer);
   function Map_Mode (Canvas : in Canvas_Type) return Integer;
   --  Logical pixel mapping mode of device

   GM_COMPATIBLE                   : constant := 1;
   GM_ADVANCED                     : constant := 2;

   procedure Graphics_Mode (Canvas : in out Canvas_Type;
                            Mode   : in     Integer);
   function Graphics_Mode (Canvas : in Canvas_Type) return Integer;
   --  Graphics mode of device
   --         Compatible (default) => compatible with 16-bit Windows
   --         Advanced-mode        => allows world transformations

   type XFORM is
      record
         eM11 : Float;
         eM12 : Float;
         eM21 : Float;
         eM22 : Float;
         eDx  : Float;
         eDy  : Float;
      end record;

   procedure World_Transform (Canvas  : in out Canvas_Type;
                              X_form  : in     XFORM);
   function World_Transform (Canvas  : in Canvas_Type) return XFORM;
   --  sets a two-dimensional linear transformation between world space
   --  and page space for device.
   --  This transformation can be used to scale, rotate, shear,
   --  or translate graphics output.

   procedure Rotate_World (Canvas   : in out Canvas_Type;
                           X_Origin : in     Integer;
                           Y_Origin : in     Integer;
                           Angle    : in     Float);
   --  Using world transform to rotate device

   procedure Viewport_Extents
     (Canvas        : in out Canvas_Type;
      Width, Height : in     Integer);

   function Viewport_Extents (Canvas : in Canvas_Type)
                             return GWindows.Types.Size_Type;

   procedure Scale_Viewport_Extents
     (Canvas                    : in out Canvas_Type;
      X_Multiplicand, X_Divisor : in     Integer;
      Y_Multiplicand, Y_Divisor : in     Integer);

   --  Viewport extents

   procedure Viewport_Origin (Canvas : in out Canvas_Type;
                              X, Y   : in     Integer);

   function Viewport_Origin (Canvas : in Canvas_Type)
                            return GWindows.Types.Point_Type;

   procedure Offset_Viewport_Origin (Canvas : in out Canvas_Type;
                                     DX, DY : in     Integer);

   --  Viewport Origin

   procedure Window_Extents
     (Canvas        : in out Canvas_Type;
      Width, Height : in     Integer);

   function Window_Extents (Canvas : in Canvas_Type)
                             return GWindows.Types.Size_Type;

   procedure Scale_Window_Extents
     (Canvas                    : in out Canvas_Type;
      X_Multiplicand, X_Divisor : in     Integer;
      Y_Multiplicand, Y_Divisor : in     Integer);

   --  Window extents

   procedure Window_Origin (Canvas : in out Canvas_Type;
                            X, Y   : in     Integer);

   function Window_Origin (Canvas : in Canvas_Type)
                            return GWindows.Types.Point_Type;

   procedure Offset_Window_Origin (Canvas : in out Canvas_Type;
                                   DX, DY : in     Integer);

   --  Window Origin

   procedure Device_To_Logical
     (Canvas : in     Canvas_Type;
      Points : in out GWindows.Types.Point_Array_Type);
   procedure Device_To_Logical (Canvas : in     Canvas_Type;
                                Point  : in out GWindows.Types.Point_Type);
   function Device_To_Logical (Canvas : in Canvas_Type;
                               X, Y   : in Integer)
                              return GWindows.Types.Point_Type;

   procedure Logical_To_Device
     (Canvas : in     Canvas_Type;
      Points : in out GWindows.Types.Point_Array_Type);
   procedure Logical_To_Device (Canvas : in     Canvas_Type;
                                Point  : in out GWindows.Types.Point_Type);
   function Logical_To_Device (Canvas : in Canvas_Type;
                               X, Y   : in Integer)
                              return GWindows.Types.Point_Type;

   --  Convert between Logical and Device points

   -------------------------------------------------------------------------
   --  Memory_Canvas_Type
   -------------------------------------------------------------------------
   --  An in memory canvas that can be used for drawing and manipulation
   --  in memory

   type Memory_Canvas_Type is new Canvas_Type with private;

   procedure Create_Memory_Canvas
     (Canvas        : in out Memory_Canvas_Type;
      Source_Canvas : in     Canvas_Type'Class);
   --  Create a Memory_Canvas that is compatible with Source_Canvas
   --  A Bitmap memory image must be created using Create_Compatible_Bitmap
   --  then selected in to the Memory_Canvas using Select_Object

   procedure Finalize (Canvas : in out Memory_Canvas_Type);

   -------------------------------------------------------------------------
   --  Printer_Canvas_Type
   -------------------------------------------------------------------------

   type Printer_Canvas_Type is new Canvas_Type with private;

   procedure Start_Page (Canvas : in out Printer_Canvas_Type);

   procedure End_Page (Canvas : in out Printer_Canvas_Type);

   procedure Start_Document (Canvas        : in out Printer_Canvas_Type;
                             Document_Name : in     GString);

   procedure End_Document (Canvas : in out Printer_Canvas_Type);

   procedure Abort_Document (Canvas : in out Printer_Canvas_Type);

private

   type Information_Canvas_Type is
     new Ada.Finalization.Limited_Controlled with
      record
         HDC        : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         HWND       : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         Orig_State : Canvas_State_Type     := 0;
      end record;

   type Canvas_Type is new Information_Canvas_Type with null record;

   type Memory_Canvas_Type is new Canvas_Type with null record;

   type Printer_Canvas_Type is new Canvas_Type with null record;

end GWindows.Drawing;
