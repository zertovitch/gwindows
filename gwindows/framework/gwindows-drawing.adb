------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                     G W I N D O W S . D R A W I N G                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2012 David Botton                   --
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
-- be located on the web at one of the following places:                    --
--   http://sf.net/projects/gnavi/                                          --
--   http://www.gnavi.org/gwindows                                          --
--   http://www.adapower.com/gwindows                                       --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Numerics;
with Ada.Numerics.Elementary_Functions;
with GWindows.Drawing.Capabilities;
with GWindows.GStrings;

package body GWindows.Drawing is

   -------------------------------------------------------------------------
   --  Operating System Imports
   -------------------------------------------------------------------------

--   TA_NOUPDATECP                   : constant := 0;
   TA_UPDATECP                     : constant := 1;
--   TA_LEFT                         : constant := 0;
   TA_RIGHT                        : constant := 2;
   TA_CENTER                       : constant := 6;
--   TA_TOP                          : constant := 0;
   TA_BOTTOM                       : constant := 8;
   TA_BASELINE                     : constant := 24;
--   TA_RTLREADING                   : constant := 256;
   MODE_TRANSPARENT                : constant := 1;
   MODE_OPAQUE                     : constant := 2;
   TMPF_FIXED_PITCH                : constant := 1;
   TMPF_VECTOR                     : constant := 2;
   TMPF_TRUETYPE                   : constant := 4;
   TMPF_DEVICE                     : constant := 8;

   subtype Byte is Interfaces.C.unsigned_char;

   type Font_Metrics_W is
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
         Italic                  : Byte;
         Underlined              : Byte;
         Struck_Out              : Byte;
         Pitch_And_Family        : Byte;
         Character_Set           : Byte;
      end record;

   procedure GetTextMetrics
     (HDC : in  GWindows.Types.Handle;
      M   : out Font_Metrics_W);
   pragma Import (StdCall, GetTextMetrics,
                    "GetTextMetrics" & Character_Mode_Identifier);

   procedure SetTextColor
     (hDC    : GWindows.Types.Handle;
      clrref : GWindows.Colors.Color_Type);
   pragma Import (StdCall, SetTextColor, "SetTextColor");

   function GetTextColor
     (hDC : GWindows.Types.Handle)
     return GWindows.Colors.Color_Type;
   pragma Import (StdCall, GetTextColor, "GetTextColor");

   procedure SetTextAlign
     (hDC   : GWindows.Types.Handle;
      fmode : Interfaces.C.unsigned);
   pragma Import (StdCall, SetTextAlign, "SetTextAlign");

   function GetTextAlign
     (hDC : GWindows.Types.Handle)
     return Interfaces.C.unsigned;
   pragma Import (StdCall, GetTextAlign, "GetTextAlign");

   procedure SetBkColor
     (hDC    : GWindows.Types.Handle;
      clrref : GWindows.Colors.Color_Type);
   pragma Import (StdCall, SetBkColor, "SetBkColor");

   procedure SetBkMode
     (hDC      : GWindows.Types.Handle;
      fnBkMode : Interfaces.C.int);
   pragma Import (StdCall, SetBkMode, "SetBkMode");

   function GetBkColor
     (hDC : GWindows.Types.Handle)
     return GWindows.Colors.Color_Type;
   pragma Import (StdCall, GetBkColor, "GetBkColor");

   function GetBkMode
     (hDC : GWindows.Types.Handle)
     return Interfaces.C.int;
   pragma Import (StdCall, GetBkMode, "GetBkMode");

   -------------------------------------------------------------------------
   --  Package Body
   -------------------------------------------------------------------------

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Information_Canvas_Type)
   is
      pragma Warnings (Off, Object);
   begin
      null;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Information_Canvas_Type) is
      use type GWindows.Types.Handle;
      use type Interfaces.C.long;
   begin
      if Object.HWND /= GWindows.Types.Null_Handle then
         Release (Object);
      elsif Object.HDC /= GWindows.Types.Null_Handle then
         Load_State (Object, Object.Orig_State);
      end if;

   end Finalize;

   ----------------
   -- Save_State --
   ----------------

   function Save_State (Canvas : in Information_Canvas_Type)
                       return Canvas_State_Type
   is
      function SaveDC
        (hDC : GWindows.Types.Handle := Canvas.HDC)
        return Canvas_State_Type;
      pragma Import (StdCall, SaveDC, "SaveDC");
   begin
      return SaveDC;
   end Save_State;

   ----------------
   -- Load_State --
   ----------------

   procedure Load_State (Canvas : in out Information_Canvas_Type;
                         State  : in     Canvas_State_Type)
   is
      procedure RestoreDC
        (hDC       : GWindows.Types.Handle := Canvas.HDC;
         SaveState : Canvas_State_Type := State);
      pragma Import (StdCall, RestoreDC, "RestoreDC");
   begin
      RestoreDC;
   end Load_State;

   ------------
   -- Handle --
   ------------

   procedure Handle
     (Canvas  : in out Information_Canvas_Type;
      HDC     : in     GWindows.Types.Handle)
   is
   begin
      Canvas.HDC := HDC;
   end Handle;

   function Handle (Canvas : in Information_Canvas_Type)
                   return GWindows.Types.Handle is
   begin
      return Canvas.HDC;
   end Handle;

   ---------
   -- Put --
   ---------

   procedure Put
     (Canvas               : in out Canvas_Type;
      X, Y                 : in     Integer;
      Text                 : in     GString)
   is
      C_Text : constant GString_C := GWindows.GStrings.To_GString_C (Text);

      procedure TextOut
        (HDC  : GWindows.Types.Handle := Canvas.HDC;
         nX   : Integer               := X;
         nY   : Integer               := Y;
         Text : GString_C             := C_Text;
         Size : Integer               := C_Text'Length - 1); -- W/O c_nul
      pragma Import (StdCall, TextOut, "TextOut" & Character_Mode_Identifier);

   begin
      TextOut;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Canvas    : in out Canvas_Type;
                  X, Y      : in     Integer;
                  Text      : in     GString;
                  Clip_Area : in     GWindows.Types.Rectangle_Type)
   is
      C_Text : constant GString_C := GWindows.GStrings.To_GString_C (Text);
      ETO_CLIPPED : constant := 4;

      procedure ExtTextOut
        (HDC       : GWindows.Types.Handle         := Canvas.HDC;
         nX        : Integer                       := X;
         nY        : Integer                       := Y;
         fuOptions : Integer                       := ETO_CLIPPED;
         lprc      : GWindows.Types.Rectangle_Type := Clip_Area;
         Text      : GString_C                     := C_Text;
         Size      : Integer                       := C_Text'Length - 1;
         lpDX      : GWindows.Types.Lparam         := 0);
      pragma Import (StdCall, ExtTextOut,
                       "ExtTextOut" & Character_Mode_Identifier);
   begin
      ExtTextOut;
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Canvas : in out Canvas_Type;
                  Text   : in     GString)
   is
   begin
      Put (Canvas, 0, 0, Text);
   end Put;

   ---------
   -- Put --
   ---------

   procedure Put (Canvas    : in out Canvas_Type;
                  Text      : in     GString;
                  Clip_Area : in     GWindows.Types.Rectangle_Type)
   is
   begin
      Put (Canvas, 0, 0, Text, Clip_Area);
   end Put;

   ----------------------
   -- Text_Output_Size --
   ----------------------

   function Text_Output_Size (Canvas : in Canvas_Type;
                              Text   : in GString)
                             return GWindows.Types.Size_Type
   is
      C_Text    : constant GString_C := GWindows.GStrings.To_GString_C (Text);
      Text_Size : GWindows.Types.Size_Type := (0, 0);

      procedure GetTextExtentPoint32
        (HDC  :     GWindows.Types.Handle    := Canvas.HDC;
         Text :     GString_C                := C_Text;
         Size :     Integer                  := C_Text'Length - 1;
         Extn : out GWindows.Types.Size_Type);
      pragma Import (StdCall, GetTextExtentPoint32,
                       "GetTextExtentPoint32" & Character_Mode_Identifier);
   begin
      GetTextExtentPoint32 (Extn => Text_Size);

      return Text_Size;
   end Text_Output_Size;

   -----------------------------
   -- Vertical_Text_Alignment --
   -----------------------------

   procedure Vertical_Text_Alignment
     (Canvas               : in out Canvas_Type;
      Alignment            : in     Vertical_Alignment_Type;
      Use_Current_Position : in     Boolean                 := False)
   is
      use type Interfaces.C.unsigned;

      Mode : Interfaces.C.unsigned := GetTextAlign (Canvas.HDC);
   begin
      Mode := Mode - (Mode and TA_BASELINE);

      if Use_Current_Position then
         Mode := Mode or TA_UPDATECP;
      else
         Mode := Mode and (not TA_UPDATECP);
      end if;

      if Alignment = Base_Line then
         Mode := Mode + TA_BASELINE;
      elsif Alignment = Bottom then
         Mode := Mode + TA_BOTTOM;
      end if;

      SetTextAlign (Canvas.HDC, Mode);
   end Vertical_Text_Alignment;

   function Vertical_Text_Alignment (Canvas : in Canvas_Type)
                                    return Vertical_Alignment_Type
   is
      use type Interfaces.C.unsigned;

      Mode : constant Interfaces.C.unsigned := GetTextAlign (Canvas.HDC);
   begin
      if (Mode and TA_BASELINE) = TA_BASELINE then
         return Base_Line;
      elsif (Mode and TA_BOTTOM) = TA_BOTTOM then
         return Bottom;
      else
         return Top;
      end if;
   end Vertical_Text_Alignment;

   -------------------------------
   -- Horizontal_Text_Alignment --
   -------------------------------

   procedure Horizontal_Text_Alignment
     (Canvas               : in out Canvas_Type;
      Alignment            : in     Horizontal_Alignment_Type;
      Use_Current_Position : in     Boolean                   := False)
   is
      use type Interfaces.C.unsigned;

      Mode : Interfaces.C.unsigned := GetTextAlign (Canvas.HDC);
   begin
      Mode := Mode - (Mode and TA_CENTER);

      if Use_Current_Position then
         Mode := Mode or TA_UPDATECP;
      else
         Mode := Mode and (not TA_UPDATECP);
      end if;

      if Alignment = Center then
         Mode := Mode + TA_CENTER;
      elsif Alignment = Right then
         Mode := Mode + TA_RIGHT;
      end if;

      SetTextAlign (Canvas.HDC, Mode);
   end Horizontal_Text_Alignment;

   function Horizontal_Text_Alignment (Canvas : in Canvas_Type)
                                      return Horizontal_Alignment_Type
   is
      use type Interfaces.C.unsigned;

      Mode : constant Interfaces.C.unsigned := GetTextAlign (Canvas.HDC);
   begin
      if (Mode and TA_CENTER) = TA_CENTER then
         return Center;
      elsif (Mode and TA_RIGHT) = TA_RIGHT then
         return Right;
      else
         return Left;
      end if;
   end Horizontal_Text_Alignment;

   ----------------
   -- Text_Color --
   ----------------

   procedure Text_Color
     (Canvas : in out Canvas_Type;
      Color  : in     GWindows.Colors.Color_Type)
   is
   begin
      SetTextColor (Canvas.HDC, Color);
   end Text_Color;

   function Text_Color (Canvas : in Canvas_Type)
                       return GWindows.Colors.Color_Type
   is
   begin
      return GetTextColor (Canvas.HDC);
   end Text_Color;

   ----------------------
   -- Background_Color --
   ----------------------

   procedure Background_Color (Canvas : in out Canvas_Type;
                               Color  : in     GWindows.Colors.Color_Type)
   is
   begin
      SetBkColor (Canvas.HDC, Color);
   end Background_Color;

   function Background_Color (Canvas : in Canvas_Type)
                             return GWindows.Colors.Color_Type
   is
   begin
      return GetBkColor (Canvas.HDC);
   end Background_Color;

   ---------------------
   -- Background_Mode --
   ---------------------

   procedure Background_Mode (Canvas : in out Canvas_Type;
                              Mode   : in     Background_Mode_Type)
   is
      Mode_Int : Interfaces.C.int := MODE_OPAQUE;
   begin
      if Mode = Transparent then
         Mode_Int := MODE_TRANSPARENT;
      end if;

      SetBkMode (Canvas.HDC, Mode_Int);
   end Background_Mode;

   function Background_Mode (Canvas : in Canvas_Type)
                            return Background_Mode_Type
   is
      use type Interfaces.C.int;
   begin
      if GetBkMode (Canvas.HDC) = MODE_OPAQUE then
         return Opaque;
      else
         return Transparent;
      end if;
   end Background_Mode;

   --------------------
   -- Fill_Rectangle --
   --------------------

   procedure Fill_Rectangle
     (Canvas      : in out Canvas_Type;
      Rectangle   : in     GWindows.Types.Rectangle_Type;
      Color_Const : in     Integer)
   is
      procedure FillRect
        (hDC  : GWindows.Types.Handle         := Canvas.HDC;
         rect : GWindows.Types.Rectangle_Type := Rectangle;
         hbr  : Interfaces.C.long             :=
           Interfaces.C.long (Color_Const + 1));
      pragma Import (StdCall, FillRect, "FillRect");
   begin
      FillRect;
   end Fill_Rectangle;

   procedure Fill_Rectangle
     (Canvas    : in out Canvas_Type;
      Rectangle : in     GWindows.Types.Rectangle_Type;
      Brush     : in     GWindows.Drawing_Objects.Brush_Type)
   is
      procedure FillRect
        (hDC  : GWindows.Types.Handle         := Canvas.HDC;
         rect : GWindows.Types.Rectangle_Type := Rectangle;
         hbr  : GWindows.Types.Handle         :=
           GWindows.Drawing_Objects.Handle (Brush));
      pragma Import (StdCall, FillRect, "FillRect");
   begin
      FillRect;
   end Fill_Rectangle;

   ---------------------
   -- Frame_Rectangle --
   ---------------------

   procedure Frame_Rectangle
     (Canvas    : in out Canvas_Type;
      Rectangle : in     GWindows.Types.Rectangle_Type;
      Brush     : in     GWindows.Drawing_Objects.Brush_Type)
   is
      procedure FrameRect
        (hDC  : GWindows.Types.Handle         := Canvas.HDC;
         rect : GWindows.Types.Rectangle_Type := Rectangle;
         hbr  : GWindows.Types.Handle         :=
           GWindows.Drawing_Objects.Handle (Brush));
      pragma Import (StdCall, FrameRect, "FrameRect");
   begin
      FrameRect;
   end Frame_Rectangle;

   ----------------------
   -- Invert_Rectangle --
   ----------------------

   procedure Invert_Rectangle
     (Canvas    : in out Canvas_Type;
      Rectangle : in     GWindows.Types.Rectangle_Type)
   is
      procedure InvertRect
        (hDC  : GWindows.Types.Handle         := Canvas.HDC;
         rect : GWindows.Types.Rectangle_Type := Rectangle);
      pragma Import (StdCall, InvertRect, "InvertRect");
   begin
      InvertRect;
   end Invert_Rectangle;

   -------------------
   -- Select_Object --
   -------------------

   procedure Select_Object
     (Canvas : in out Canvas_Type;
      Object : in     GWindows.Drawing_Objects.Drawing_Object_Type'Class)
   is
      procedure SelectObject
        (hDC     : GWindows.Types.Handle := Canvas.HDC;
         HOBJECT : GWindows.Types.Handle :=
           GWindows.Drawing_Objects.Handle (Object));
      pragma Import (StdCall, SelectObject, "SelectObject");
   begin
      SelectObject;
   end Select_Object;

   -------------
   -- Capture --
   -------------

   procedure Capture (Canvas : in out Information_Canvas_Type;
                      HWND   : in     GWindows.Types.Handle;
                      HDC    : in     GWindows.Types.Handle)
   is
   begin
      Handle (Canvas, HDC);
      Canvas.HWND := HWND;
      Canvas.Orig_State := Save_State (Canvas);
   end Capture;

   -------------
   -- Release --
   -------------

   procedure Release (Canvas : in out Information_Canvas_Type) is
      use type GWindows.Types.Handle;
      use type Interfaces.C.long;

      procedure ReleaseDC
        (hWnd : GWindows.Types.Handle := Canvas.HWND;
         hDC  : GWindows.Types.Handle := Canvas.HDC);
      pragma Import (StdCall, ReleaseDC, "ReleaseDC");
   begin
      if Canvas.HDC /= GWindows.Types.Null_Handle then
         Load_State (Canvas, Canvas.Orig_State);
      end if;

      ReleaseDC;

      Canvas.HDC := GWindows.Types.Null_Handle;
      Canvas.HWND := GWindows.Types.Null_Handle;
   end Release;

   ---------------
   -- Rectangle --
   ---------------

   procedure Rectangle (Canvas : in out Canvas_Type;
                        Left, Top, Right, Bottom : Integer)
   is
      procedure GDI_Rectangle
        (hDC     : GWindows.Types.Handle := Canvas.HDC;
         NLeft   : Integer           := Left;
         NTop    : Integer           := Top;
         NRight  : Integer           := Right;
         NBottom : Integer           := Bottom);
      pragma Import (StdCall, GDI_Rectangle, "Rectangle");
   begin
      GDI_Rectangle;
   end Rectangle;

   -----------------------
   -- Rounded_Rectangle --
   -----------------------

   procedure Rounded_Rectangle (Canvas : in out Canvas_Type;
                                Left, Top, Right, Bottom,
                                  Ellipse_Width,
                                  Ellipse_Height : Integer)
   is
      procedure GDI_RoundRect
        (hDC     : GWindows.Types.Handle := Canvas.HDC;
         NLeft   : Integer           := Left;
         NTop    : Integer           := Top;
         NRight  : Integer           := Right;
         NBottom : Integer           := Bottom;
         NWidth  : Integer           := Ellipse_Width;
         NHeight : Integer           := Ellipse_Height);
      pragma Import (StdCall, GDI_RoundRect, "RoundRect");
   begin
      GDI_RoundRect;
   end Rounded_Rectangle;

   -----------
   -- Chord --
   -----------

   procedure Chord (Canvas : in out Canvas_Type;
                    Left, Top, Right, Bottom,
                      XRadial1, YRadial1, XRadial2, YRadial2 : Integer)
   is
      procedure GDI_Chord
        (hDC     : GWindows.Types.Handle := Canvas.HDC;
         NLeft   : Integer           := Left;
         NTop    : Integer           := Top;
         NRight  : Integer           := Right;
         NBottom : Integer           := Bottom;
         NXR1    : Integer           := XRadial1;
         NYR1    : Integer           := YRadial1;
         NXR2    : Integer           := XRadial2;
         NYR2    : Integer           := YRadial2);
      pragma Import (StdCall, GDI_Chord, "Chord");
   begin
      GDI_Chord;
   end Chord;

   -------------
   -- Ellipse --
   -------------

   procedure Ellipse (Canvas : in out Canvas_Type;
                      Left, Top, Right, Bottom : Integer)
   is
      procedure GDI_Ellipse
        (hDC     : GWindows.Types.Handle := Canvas.HDC;
         NLeft   : Integer           := Left;
         NTop    : Integer           := Top;
         NRight  : Integer           := Right;
         NBottom : Integer           := Bottom);
      pragma Import (StdCall, GDI_Ellipse, "Ellipse");
   begin
      GDI_Ellipse;
   end Ellipse;

   ---------
   -- Pie --
   ---------

   procedure Pie (Canvas : in out Canvas_Type;
                  Left, Top, Right, Bottom,
                    XRadial1, YRadial1, XRadial2, YRadial2 : Integer)
   is
      procedure GDI_Pie
        (hDC     : GWindows.Types.Handle := Canvas.HDC;
         NLeft   : Integer           := Left;
         NTop    : Integer           := Top;
         NRight  : Integer           := Right;
         NBottom : Integer           := Bottom;
         NXR1    : Integer           := XRadial1;
         NYR1    : Integer           := YRadial1;
         NXR2    : Integer           := XRadial2;
         NYR2    : Integer           := YRadial2);
      pragma Import (StdCall, GDI_Pie, "Pie");
   begin
      GDI_Pie;
   end Pie;

   -------------
   -- Polygon --
   -------------

   procedure Polygon (Canvas   : in out Canvas_Type;
                      Vertices : in     GWindows.Types.Point_Array_Type)
   is
      procedure GDI_Polygon
        (hDC    : GWindows.Types.Handle           := Canvas.HDC;
         Points : GWindows.Types.Point_Array_Type := Vertices;
         Count  : Integer                         := Vertices'Length);
      pragma Import (StdCall, GDI_Polygon, "Polygon");
   begin
      GDI_Polygon;
   end Polygon;

   -----------
   -- Point --
   -----------

   function Point (Canvas : in Canvas_Type;
                   X, Y   : in Integer)
                  return GWindows.Colors.Color_Type
   is
      function GetPixel
        (hDC   : GWindows.Types.Handle := Canvas.HDC;
         nXPos : Integer           := X;
         nYPos : Integer           := Y)
        return GWindows.Colors.Color_Type;
      pragma Import (StdCall, GetPixel, "GetPixel");
   begin
      return GetPixel;
   end Point;

   procedure Point (Canvas : in out Canvas_Type;
                    X, Y   : in     Integer;
                    Color  : in     GWindows.Colors.Color_Type)
   is
      procedure SetPixelV
        (hDC    : GWindows.Types.Handle      := Canvas.HDC;
         nXPos  : Integer                    := X;
         nYPos  : Integer                    := Y;
         clrref : GWindows.Colors.Color_Type := Color);
      pragma Import (StdCall, SetPixelV, "SetPixelV");
   begin
      SetPixelV;
   end Point;

   ----------
   -- Line --
   ----------

   procedure Line (Canvas         : in out Canvas_Type;
                   X1, Y1, X2, Y2 : in     Integer)
   is
      procedure MoveToEx
        (hDC    : GWindows.Types.Handle := Canvas.HDC;
         X      : Integer := X1;
         Y      : Integer := Y1;
         Unused : Integer := 0);
      pragma Import (StdCall, MoveToEx, "MoveToEx");

      procedure LineTo
        (hDC    : GWindows.Types.Handle := Canvas.HDC;
         X      : Integer := X2;
         Y      : Integer := Y2);
      pragma Import (StdCall, LineTo, "LineTo");
   begin
      MoveToEx;
      LineTo;
   end Line;

   ---------
   -- Arc --
   ---------

   procedure Arc (Canvas : in out Canvas_Type;
                  Left, Top, Right, Bottom,
                    XStartArc, YStartArc, XEndArc, YEndArc : Integer)
   is
      procedure GDI_Arc
        (hDC     : GWindows.Types.Handle := Canvas.HDC;
         NLeft   : Integer           := Left;
         NTop    : Integer           := Top;
         NRight  : Integer           := Right;
         NBottom : Integer           := Bottom;
         NXR1    : Integer           := XStartArc;
         NYR1    : Integer           := YStartArc;
         NXR2    : Integer           := XEndArc;
         NYR2    : Integer           := YEndArc);
      pragma Import (StdCall, GDI_Arc, "Arc");
   begin
      GDI_Arc;
   end Arc;

   -------------
   -- Beziers --
   -------------

   procedure Beziers (Canvas : in out Canvas_Type;
                      Points : in     GWindows.Types.Point_Array_Type)
   is
      procedure PolyBezier
        (hDC     : GWindows.Types.Handle           := Canvas.HDC;
         pPoints : GWindows.Types.Point_Array_Type := Points;
         Count   : Integer                         := Points'Length);
      pragma Import (StdCall, PolyBezier, "PolyBezier");
   begin
      PolyBezier;
   end Beziers;

   -----------
   -- Lines --
   -----------

   procedure Lines (Canvas : in out Canvas_Type;
                    Points : in     GWindows.Types.Point_Array_Type)
   is
      procedure Polyline
        (hDC     : GWindows.Types.Handle           := Canvas.HDC;
         pPoints : GWindows.Types.Point_Array_Type := Points;
         Count   : Integer                         := Points'Length);
      pragma Import (StdCall, Polyline, "Polyline");
   begin
      Polyline;
   end Lines;

   --------------------
   -- Draw_Direction --
   --------------------

   procedure Draw_Direction (Canvas    : in out Canvas_Type;
                             Direction : in     Draw_Direction_Type)
   is
      AD_COUNTERCLOCKWISE             : constant := 1;
      AD_CLOCKWISE                    : constant := 2;

      procedure SetArcDirection
        (hDC    : GWindows.Types.Handle := Canvas.HDC;
         Direct : Integer);
      pragma Import (StdCall, SetArcDirection, "SetArcDirection");
   begin
      if Direction = Clock_Wise then
         SetArcDirection (Direct => AD_CLOCKWISE);
      else
         SetArcDirection (Direct => AD_COUNTERCLOCKWISE);
      end if;
   end Draw_Direction;

   ----------------
   -- Point_Size --
   ----------------

   function Point_Size (Canvas : in Canvas_Type;
                        Size   : in Natural)
                       return Integer
   is
   begin
      return -(Size *  Y_Pixels_Per_Inch (Canvas)) / 72;
   end Point_Size;

   -----------------------
   -- Y_Pixels_Per_Inch --
   -----------------------

   function Y_Pixels_Per_Inch (Canvas : in Canvas_Type) return Integer
   is
      use GWindows.Drawing.Capabilities;
   begin
      return Integer (Get_Capability (Canvas, LOGPIXELSY));
   end Y_Pixels_Per_Inch;

   -----------------------
   -- X_Pixels_Per_Inch --
   -----------------------

   function X_Pixels_Per_Inch (Canvas : in Canvas_Type) return Integer
   is
      use GWindows.Drawing.Capabilities;
   begin
      return Integer (Get_Capability (Canvas, LOGPIXELSX));
   end X_Pixels_Per_Inch;

   ------------------------------
   -- Create_Compatible_Bitmap --
   ------------------------------

   procedure Create_Compatible_Bitmap
     (Canvas        : in     Canvas_Type;
      Bitmap        : in out GWindows.Drawing_Objects.Bitmap_Type;
      Width, Height : in     Natural)
   is
      function CreateCompatibleBitmap
        (hDC     : GWindows.Types.Handle := Canvas.HDC;
         nWidth  : Integer           := Width;
         nHeight : Integer           := Height)
        return GWindows.Types.Handle;
      pragma Import
        (StdCall, CreateCompatibleBitmap, "CreateCompatibleBitmap");

   begin
      GWindows.Drawing_Objects.Handle (Bitmap, CreateCompatibleBitmap);
   end Create_Compatible_Bitmap;

   ------------------
   -- Paint_Bitmap --
   ------------------

   procedure Paint_Bitmap
     (Canvas                : in out Canvas_Type;
      Bitmap                : in out GWindows.Drawing_Objects.Bitmap_Type;
      X, Y, Width, Height   : in     Integer;
      Raster_Operation_Code :
        in     Interfaces.C.unsigned              := SRCCOPY)
   is
      MDC : Memory_Canvas_Type;
   begin
      Create_Memory_Canvas (MDC, Canvas);
      Select_Object (MDC, Bitmap);
      BitBlt (Canvas, X, Y, Width, Height, MDC, 0, 0, Raster_Operation_Code);
   end Paint_Bitmap;

   ----------------
   -- Paint_Icon --
   ----------------

   procedure Paint_Icon (Canvas : in out Canvas_Type;
                         Icon   : in out GWindows.Drawing_Objects.Icon_Type;
                         X, Y   : in     Integer)
   is
      procedure DrawIcon
        (hDC   : GWindows.Types.Handle := Canvas.HDC;
         iX    : Integer               := X;
         iY    : Integer               := Y;
         hIcon : GWindows.Types.Handle :=
                 GWindows.Drawing_Objects.Handle (Icon));
      pragma Import (StdCall, DrawIcon, "DrawIcon");
   begin
      DrawIcon;
   end Paint_Icon;

   ------------
   -- BitBlt --
   ------------

   procedure BitBlt
     (Destination_Canvas                    : in out Canvas_Type;
      Destination_X, Destination_Y          : in     Integer;
      Destination_Width, Destination_Height : in     Integer;
      Source_Canvas                         : in     Canvas_Type'Class;
      Source_X, Source_Y                    : in     Integer;
      Raster_Operation_Code
        : in     Interfaces.C.unsigned := SRCCOPY)
   is
      procedure GDI_BitBlt
        (hdcDest : GWindows.Types.Handle := Handle (Destination_Canvas);
         nXDest  : Integer := Destination_X;
         nYDest  : Integer := Destination_Y;
         nWidth  : Integer := Destination_Width;
         nHeight : Integer := Destination_Height;
         hdcSrc  : GWindows.Types.Handle := Handle (Source_Canvas);
         nXSrc   : Integer := Source_X;
         nYSrc   : Integer := Source_Y;
         dwRop   : Interfaces.C.unsigned := Raster_Operation_Code);
      pragma Import (StdCall, GDI_BitBlt, "BitBlt");
   begin
      GDI_BitBlt;
   end BitBlt;

   ----------------
   -- StretchBlt --
   ----------------

   procedure StretchBlt
     (Destination_Canvas                    : in out Canvas_Type;
      Destination_X, Destination_Y          : in     Integer;
      Destination_Width, Destination_Height : in     Integer;
      Source_Canvas                         : in     Canvas_Type'Class;
      Source_X, Source_Y                    : in     Integer;
      Source_Width, Source_Height           : in     Integer;
      Raster_Operation_Code
        : in     Interfaces.C.unsigned := SRCCOPY)
   is
      procedure GDI_StretchBlt
        (hdcDest    : GWindows.Types.Handle := Handle (Destination_Canvas);
         nXDest     : Integer               := Destination_X;
         nYDest     : Integer               := Destination_Y;
         nWidth     : Integer               := Destination_Width;
         nHeight    : Integer               := Destination_Height;
         hdcSrc     : GWindows.Types.Handle := Handle (Source_Canvas);
         nXSrc      : Integer               := Source_X;
         nYSrc      : Integer               := Source_Y;
         nWidthSrc  : Integer               := Source_Width;
         nHeightSrc : Integer               := Source_Height;
         dwRop      : Interfaces.C.unsigned := Raster_Operation_Code);
      pragma Import (StdCall, GDI_StretchBlt, "StretchBlt");
   begin
      GDI_StretchBlt;
   end StretchBlt;

   --------------------------
   -- Create_Memory_Canvas --
   --------------------------

   procedure Create_Memory_Canvas (Canvas        : in out Memory_Canvas_Type;
                                   Source_Canvas : in     Canvas_Type'Class)
   is
      function CreateCompatibleDC
        (hDC : GWindows.Types.Handle := Source_Canvas.HDC)
        return GWindows.Types.Handle;
      pragma Import (StdCall, CreateCompatibleDC, "CreateCompatibleDC");
   begin
      Handle (Canvas, CreateCompatibleDC);
      Canvas.Orig_State := Save_State (Canvas);
   end Create_Memory_Canvas;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Canvas : in out Memory_Canvas_Type) is
      procedure DeleteDC (hdc : GWindows.Types.Handle := Canvas.HDC);
      pragma Import (StdCall, DeleteDC, "DeleteDC");
   begin
      DeleteDC;
   end Finalize;

   ------------------
   -- Set_Mix_Mode --
   ------------------

   procedure Set_Mix_Mode (Canvas : in Canvas_Type;
                           Mode   : in Natural)
   is
      procedure SetROP2
        (hDC : GWindows.Types.Handle := Canvas.HDC;
         ROP : Natural := Mode);
      pragma Import (StdCall, SetROP2, "SetROP2");
   begin
      SetROP2;
   end Set_Mix_Mode;

   -------------------
   -- Clipping_Area --
   -------------------

   function Clipping_Area (Canvas : in Canvas_Type)
                          return GWindows.Types.Rectangle_Type
   is
      Result : GWindows.Types.Rectangle_Type := (0, 0, 0, 0);

      procedure GetClipBox
        (hDC  : GWindows.Types.Handle := Canvas.HDC;
         Rect : out GWindows.Types.Rectangle_Type);
      pragma Import (StdCall, GetClipBox, "GetClipBox");
   begin
      GetClipBox (Rect => Result);
      return Result;
   end Clipping_Area;

   ---------------------------
   -- Include_Clipping_Area --
   ---------------------------

   procedure Include_Clipping_Area
     (Canvas : in out Canvas_Type;
      Left, Top, Right, Bottom : Integer)
   is
      procedure IntersectClipRect
        (DC : GWindows.Types.Handle := Canvas.HDC;
         X  : Integer               := Left;
         Y  : Integer               := Top;
         X2 : Integer               := Right;
         Y2 : Integer               := Bottom);
      pragma Import (StdCall, IntersectClipRect, "IntersectClipRect");
   begin
      IntersectClipRect;
   end Include_Clipping_Area;

   ---------------------------
   -- Exclude_Clipping_Area --
   ---------------------------

   procedure Exclude_Clipping_Area
     (Canvas : in out Canvas_Type;
      Left, Top, Right, Bottom : Integer)
   is
      procedure ExcludeClipRect
        (DC : GWindows.Types.Handle := Canvas.HDC;
         X  : Integer               := Left;
         Y  : Integer               := Top;
         X2 : Integer               := Right;
         Y2 : Integer               := Bottom);
      pragma Import (StdCall, ExcludeClipRect, "ExcludeClipRect");
   begin
      ExcludeClipRect;
   end Exclude_Clipping_Area;

   --------------------------
   -- Inside_Clipping_Area --
   --------------------------

   function Inside_Clipping_Area (Canvas : in Canvas_Type;
                                  X, Y   : in Integer)
                                 return Boolean
   is
      function PtVisible
        (DC : GWindows.Types.Handle := Canvas.HDC;
         X1 : Integer               := X;
         Y1 : Integer               := Y)
        return Boolean;
      pragma Import (StdCall, PtVisible, "PtVisible");
   begin
      return PtVisible;
   end Inside_Clipping_Area;

   function Inside_Clipping_Area (Canvas                   : in Canvas_Type;
                                  Left, Top, Right, Bottom : in Integer)
                                 return Boolean
   is
      Rect : constant GWindows.Types.Rectangle_Type :=
        (Left, Top, Right, Bottom);

      function RectVisible
        (DC : GWindows.Types.Handle         := Canvas.HDC;
         R1 : GWindows.Types.Rectangle_Type := Rect)
        return Boolean;
      pragma Import (StdCall, RectVisible, "RectVisible");
   begin
      return RectVisible;
   end Inside_Clipping_Area;

   ----------------
   -- Start_Page --
   ----------------

   procedure Start_Page (Canvas : in out Printer_Canvas_Type)
   is
      procedure StartPage (HDC : GWindows.Types.Handle := Canvas.HDC);
      pragma Import (StdCall, StartPage, "StartPage");
   begin
      StartPage;
   end Start_Page;

   --------------
   -- End_Page --
   --------------

   procedure End_Page (Canvas : in out Printer_Canvas_Type)
   is
      procedure EndPage (HDC : GWindows.Types.Handle := Canvas.HDC);
      pragma Import (StdCall, EndPage, "EndPage");
   begin
      EndPage;
   end End_Page;

   --------------------
   -- Start_Document --
   --------------------

   procedure Start_Document (Canvas        : in out Printer_Canvas_Type;
                             Document_Name : in     GString)
   is
      C_Text : GString_C := GWindows.GStrings.To_GString_C (Document_Name);
      type Pointer_To_Char is access all GChar_C;

      type DOCINFO is
         record
            cbSize       : Integer := 20;
            lpszDocName  : Pointer_To_Char := C_Text (0)'Unchecked_Access;
            lpszOutput   : Interfaces.C.long := 0;
            lpszDatatype : Interfaces.C.long := 0;
            fwType       : Interfaces.C.long := 0;
         end record;

      DInfo : DOCINFO;

      procedure StartDoc (HDC : GWindows.Types.Handle := Canvas.HDC;
                          DI  : DOCINFO               := DInfo);
      pragma Import (StdCall, StartDoc,
                       "StartDoc" & Character_Mode_Identifier);
   begin
      StartDoc;
   end Start_Document;

   ------------------
   -- End_Document --
   ------------------

   procedure End_Document (Canvas : in out Printer_Canvas_Type)
   is
      procedure EndDoc (HDC : GWindows.Types.Handle := Canvas.HDC);
      pragma Import (StdCall, EndDoc, "EndDoc");
   begin
      EndDoc;
   end End_Document;

   --------------------
   -- Abort_Document --
   --------------------

   procedure Abort_Document (Canvas : in out Printer_Canvas_Type)
   is
      procedure AbortDoc (HDC : GWindows.Types.Handle := Canvas.HDC);
      pragma Import (StdCall, AbortDoc, "AbortDoc");
   begin
      AbortDoc;
   end Abort_Document;

   --------------
   -- Map_Mode --
   --------------

   procedure Map_Mode (Canvas : in out Canvas_Type;
                       Mode   : in     Integer)
   is
      procedure SetMapMode (HDC     : GWindows.Types.Handle := Canvas.HDC;
                            MapMode : Integer               := Mode);
      pragma Import (StdCall, SetMapMode, "SetMapMode");
   begin
      SetMapMode;
   end Map_Mode;

   function Map_Mode (Canvas : in Canvas_Type) return Integer
   is
      function GetMapMode (HDC : GWindows.Types.Handle := Canvas.HDC)
                          return Integer;
      pragma Import (StdCall, GetMapMode, "GetMapMode");
   begin
      return GetMapMode;
   end Map_Mode;

   -------------------
   -- Graphics_Mode --
   -------------------
   procedure Graphics_Mode (Canvas : in out Canvas_Type;
                            Mode   : in     Integer)
   is
      procedure SetGraphicsMode (HDC   : GWindows.Types.Handle := Canvas.HDC;
                                 iMode : Integer               := Mode);
      pragma Import (StdCall, SetGraphicsMode, "SetGraphicsMode");
   begin
      SetGraphicsMode;
   end Graphics_Mode;

   function Graphics_Mode (Canvas : in Canvas_Type) return Integer
   is
      function GetGraphicsMode (HDC : GWindows.Types.Handle := Canvas.HDC)
        return Integer;
      pragma Import (StdCall, GetGraphicsMode, "GetGraphicsMode");
   begin
      return GetGraphicsMode;
   end Graphics_Mode;

   ---------------------
   -- World Transform --
   ---------------------

   procedure World_Transform (Canvas  : in out Canvas_Type;
                              X_form  : in     XFORM)
   is
      procedure SetWorldTransform (HDC   : GWindows.Types.Handle := Canvas.HDC;
                                   Xfor  : XFORM                 := X_form);
      pragma Import (StdCall, SetWorldTransform, "SetWorldTransform");
   begin
      SetWorldTransform;
   end World_Transform;

   function World_Transform (Canvas : in Canvas_Type) return XFORM
   is
      X_Form : XFORM;
      procedure GetWorldTransform (HDC  : in     GWindows.Types.Handle;
                                   Xfor : in out XFORM);
      pragma Import (StdCall, GetWorldTransform, "GetWorldTransform");

   begin
      GetWorldTransform (Canvas.HDC, X_Form);
      return X_Form;
   end World_Transform;

   ------------------
   -- Rotate World --
   ------------------

   procedure Rotate_World (Canvas   : in out Canvas_Type;
                           X_Origin : in     Integer;
                           Y_Origin : in     Integer;
                           Angle    : in     Float) is
      X_form  : XFORM;
      Radians : constant Float := Angle * 2.0 * Ada.Numerics.Pi / 360.0;
   begin
      X_form.eM11 := Ada.Numerics.Elementary_Functions.Cos (Radians);
      X_form.eM12 := Ada.Numerics.Elementary_Functions.Sin (Radians);
      X_form.eM21 := -X_form.eM12;
      X_form.eM22 := X_form.eM11;
      X_form.eDx  := Float (X_Origin);
      X_form.eDy  := Float (Y_Origin);
      World_Transform (Canvas, X_form);
   end Rotate_World;

   ----------------------
   -- Viewport_Extents --
   ----------------------

   procedure Viewport_Extents
     (Canvas        : in out Canvas_Type;
      Width, Height : in     Integer)
   is
      procedure SetViewportExtEx
        (HDC : GWindows.Types.Handle := Canvas.HDC;
         W   : Integer := Width;
         H   : Integer := Height;
         LPS : Integer := 0);
      pragma Import (StdCall, SetViewportExtEx, "SetViewportExtEx");
   begin
      SetViewportExtEx;
   end Viewport_Extents;

   function Viewport_Extents (Canvas : in Canvas_Type)
                             return GWindows.Types.Size_Type
   is
      Result : GWindows.Types.Size_Type := (0, 0);

      procedure GetViewportExtEx
        (HDC :     GWindows.Types.Handle    := Canvas.HDC;
         LPS : out GWindows.Types.Size_Type);
      pragma Import (StdCall, GetViewportExtEx, "GetViewportExtEx");

   begin
      GetViewportExtEx (LPS => Result);
      return Result;
   end Viewport_Extents;

   ----------------------------
   -- Scale_Viewport_Extents --
   ----------------------------

   procedure Scale_Viewport_Extents
     (Canvas                    : in out Canvas_Type;
      X_Multiplicand, X_Divisor : in     Integer;
      Y_Multiplicand, Y_Divisor : in     Integer)
   is
      procedure ScaleViewportExtEx
        (HDC : GWindows.Types.Handle := Canvas.HDC;
         X1  : Integer := X_Multiplicand;
         X2  : Integer := X_Divisor;
         Y1  : Integer := Y_Multiplicand;
         Y2  : Integer := Y_Divisor;
         LPS : Integer := 0);
      pragma Import (StdCall, ScaleViewportExtEx, "ScaleViewportExtEx");
   begin
      ScaleViewportExtEx;
   end Scale_Viewport_Extents;

   ---------------------
   -- Viewport_Origin --
   ---------------------

   procedure Viewport_Origin (Canvas : in out Canvas_Type;
                              X, Y   : in     Integer)
   is
      procedure SetViewportOrgEx
        (HDC : GWindows.Types.Handle := Canvas.HDC;
         X1  : Integer := X;
         Y1  : Integer := Y;
         LPP : Integer := 0);
      pragma Import (StdCall, SetViewportOrgEx, "SetViewportOrgEx");
   begin
      SetViewportOrgEx;
   end Viewport_Origin;

   function Viewport_Origin (Canvas : in Canvas_Type)
                            return GWindows.Types.Point_Type
   is
      Result : GWindows.Types.Point_Type := (0, 0);

      procedure GetViewportOrgEx
        (HDC :     GWindows.Types.Handle     := Canvas.HDC;
         LPS : out GWindows.Types.Point_Type);
      pragma Import (StdCall, GetViewportOrgEx, "GetViewportOrgEx");

   begin
      GetViewportOrgEx (LPS => Result);

      return Result;
   end Viewport_Origin;

   ----------------------------
   -- Offset_Viewport_Origin --
   ----------------------------

   procedure Offset_Viewport_Origin (Canvas : in out Canvas_Type;
                                     DX, DY : in     Integer)
   is
      procedure OffsetViewportOrgEx
        (HDC : GWindows.Types.Handle := Canvas.HDC;
         X1  : Integer := DX;
         Y1  : Integer := DY;
         LPS : Integer := 0);
      pragma Import (StdCall, OffsetViewportOrgEx, "OffsetViewportOrgEx");
   begin
      OffsetViewportOrgEx;
   end Offset_Viewport_Origin;

   --------------------
   -- Window_Extents --
   --------------------

   procedure Window_Extents
     (Canvas        : in out Canvas_Type;
      Width, Height : in     Integer)
   is
      procedure SetWindowExtEx
        (HDC : GWindows.Types.Handle := Canvas.HDC;
         W   : Integer := Width;
         H   : Integer := Height;
         LPS : Integer := 0);
      pragma Import (StdCall, SetWindowExtEx, "SetWindowExtEx");
   begin
      SetWindowExtEx;
   end Window_Extents;

   function Window_Extents (Canvas : in Canvas_Type)
                           return GWindows.Types.Size_Type
   is
      Result : GWindows.Types.Size_Type := (0, 0);

      procedure GetWindowExtEx
        (HDC :     GWindows.Types.Handle    := Canvas.HDC;
         LPS : out GWindows.Types.Size_Type);
      pragma Import (StdCall, GetWindowExtEx, "GetWindowExtEx");

   begin
      GetWindowExtEx (LPS => Result);
      return Result;
   end Window_Extents;

   ---------------------------
   -- Scale_Window_Extents --
   ---------------------------

   procedure Scale_Window_Extents
     (Canvas                    : in out Canvas_Type;
      X_Multiplicand, X_Divisor : in     Integer;
      Y_Multiplicand, Y_Divisor : in     Integer)
   is
      procedure ScaleWindowExtEx
        (HDC : GWindows.Types.Handle := Canvas.HDC;
         X1  : Integer := X_Multiplicand;
         X2  : Integer := X_Divisor;
         Y1  : Integer := Y_Multiplicand;
         Y2  : Integer := Y_Divisor;
         LPS : Integer := 0);
      pragma Import (StdCall, ScaleWindowExtEx, "ScaleWindowExtEx");
   begin
      ScaleWindowExtEx;
   end Scale_Window_Extents;

   -------------------
   -- Window_Origin --
   -------------------

   procedure Window_Origin (Canvas : in out Canvas_Type;
                            X, Y   : in     Integer)
   is
      procedure SetWindowOrgEx
        (HDC : GWindows.Types.Handle := Canvas.HDC;
         X1  : Integer := X;
         Y1  : Integer := Y;
         LPP : Integer := 0);
      pragma Import (StdCall, SetWindowOrgEx, "SetWindowOrgEx");
   begin
      SetWindowOrgEx;
   end Window_Origin;

   function Window_Origin (Canvas : in Canvas_Type)
                          return GWindows.Types.Point_Type
   is
      Result : GWindows.Types.Point_Type := (0, 0);

      procedure GetWindowOrgEx
        (HDC :     GWindows.Types.Handle     := Canvas.HDC;
         LPS : out GWindows.Types.Point_Type);
      pragma Import (StdCall, GetWindowOrgEx, "GetWindowOrgEx");

   begin
      GetWindowOrgEx (LPS => Result);
      return Result;
   end Window_Origin;

   --------------------------
   -- Offset_Window_Origin --
   --------------------------

   procedure Offset_Window_Origin (Canvas : in out Canvas_Type;
                                   DX, DY : in     Integer)
   is
      procedure OffsetWindowOrgEx
        (HDC : GWindows.Types.Handle := Canvas.HDC;
         X1  : Integer := DX;
         Y1  : Integer := DY;
         LPS : Integer := 0);
      pragma Import (StdCall, OffsetWindowOrgEx, "OffsetWindowOrgEx");
   begin
      OffsetWindowOrgEx;
   end Offset_Window_Origin;

   -----------------------
   -- Device_To_Logical --
   -----------------------

   procedure Device_To_Logical
     (Canvas : in     Canvas_Type;
      Points : in out GWindows.Types.Point_Array_Type)
   is
      procedure DPtoLP
        (HDC :        GWindows.Types.Handle           := Canvas.HDC;
         LPP : in out GWindows.Types.Point_Array_Type;
         CNT :        Integer                         := Points'Length);
      pragma Import (StdCall, DPtoLP, "DPtoLP");
   begin
      DPtoLP (LPP => Points);
   end Device_To_Logical;

   procedure Device_To_Logical (Canvas : in     Canvas_Type;
                                Point  : in out GWindows.Types.Point_Type)
   is
      Result : GWindows.Types.Point_Array_Type := (1 => Point);
   begin
      Device_To_Logical (Canvas, Result);
      Point := Result (1);
   end Device_To_Logical;

   function Device_To_Logical (Canvas : in Canvas_Type;
                               X, Y   : in Integer)
                              return GWindows.Types.Point_Type
   is
      Result : GWindows.Types.Point_Type := (X, Y);
   begin
      Device_To_Logical (Canvas, Result);
      return Result;
   end Device_To_Logical;

   -----------------------
   -- Logical_To_Device --
   -----------------------

   procedure Logical_To_Device
     (Canvas : in     Canvas_Type;
      Points : in out GWindows.Types.Point_Array_Type)
   is
      procedure LPtoDP
        (HDC :        GWindows.Types.Handle           := Canvas.HDC;
         LPP : in out GWindows.Types.Point_Array_Type;
         CNT :        Integer                         := Points'Length);
      pragma Import (StdCall, LPtoDP, "LPtoDP");
   begin
      LPtoDP (LPP => Points);
   end Logical_To_Device;

   procedure Logical_To_Device (Canvas : in     Canvas_Type;
                                Point  : in out GWindows.Types.Point_Type)
   is
      Result : GWindows.Types.Point_Array_Type := (1 => Point);
   begin
      Logical_To_Device (Canvas, Result);
      Point := Result (1);
   end Logical_To_Device;

   function Logical_To_Device (Canvas : in Canvas_Type;
                               X, Y   : in Integer)
                              return GWindows.Types.Point_Type
   is
      Result : GWindows.Types.Point_Type := (X, Y);
   begin
      Logical_To_Device (Canvas, Result);
      return Result;
   end Logical_To_Device;

   ------------------
   -- Rectangle_3D --
   ------------------

   procedure Rectangle_3D
     (Canvas                   : in out Canvas_Type;
      Left, Top, Right, Bottom : in     Integer;
      Direction                : in     ThreeD_Type := Raised;
      Thickness                : in     Integer     := 1)
   is
      use GWindows.Colors;
   begin
      if Direction = Raised then
         Rectangle_3D (Canvas, Left, Top, Right, Bottom,
                       System_Color (COLOR_BTNHILIGHT),
                       System_Color (COLOR_BTNSHADOW),
                       Thickness);
      else
         Rectangle_3D (Canvas, Left, Top, Right, Bottom,
                       System_Color (COLOR_BTNSHADOW),
                       System_Color (COLOR_BTNHILIGHT),
                       Thickness);
      end if;
   end Rectangle_3D;

   procedure Rectangle_3D
     (Canvas               : in out Canvas_Type;
      Left, Top,
        Right, Bottom      : in     Integer;
      Top_Left_Color,
        Bottom_Right_Color : in     GWindows.Colors.Color_Type;
      Thickness            : in     Integer                   := 1)
   is
      use GWindows.Drawing_Objects;
   begin
      if Thickness > 1 then
         Rectangle_3D (Canvas,
                       Left + 1, Top + 1, Right - 1, Bottom - 1,
                       Top_Left_Color, Bottom_Right_Color,
                       Thickness - 1);
      end if;

      declare
         Top_Left_Brush     : Brush_Type;
         Bottom_Right_Brush : Brush_Type;
      begin
         Create_Solid_Brush (Top_Left_Brush, Top_Left_Color);
         Create_Solid_Brush (Bottom_Right_Brush, Bottom_Right_Color);

         Fill_Rectangle
           (Canvas, (Left, Top, Right - 1, Top + 1),
            Top_Left_Brush);
         Fill_Rectangle
           (Canvas, (Left, Top, Left + 1, Bottom - 1),
            Top_Left_Brush);
         Fill_Rectangle
           (Canvas, (Right, Top, Right - 1, Bottom),
            Bottom_Right_Brush);
         Fill_Rectangle
           (Canvas, (Left, Bottom, Right, Bottom - 1),
            Bottom_Right_Brush);
      end;
   end Rectangle_3D;

   -----------------------
   -- Current_Font_Name --
   -----------------------

   function Current_Font_Name (Canvas : in Canvas_Type) return GString
   is
      Buffer : GString_C (1 .. 255);

      procedure GetTextFace
        (HDC : in     GWindows.Types.Handle := Canvas.HDC;
         NC  : in     Integer               := Buffer'Length;
         BUF : access GChar_C               := Buffer (Buffer'First)'Access);
      pragma Import (StdCall, GetTextFace,
                       "GetTextFace" & Character_Mode_Identifier);
   begin
      GetTextFace;
      return GWindows.GStrings.To_GString_From_C (Buffer);
   end Current_Font_Name;

   --------------------------
   -- Current_Font_Metrics --
   --------------------------

   function Current_Font_Metrics (Canvas : in Canvas_Type)
                                 return Font_Metrics_Type
   is
      use type Interfaces.C.unsigned_char;

      R : Font_Metrics_Type;
      W : Font_Metrics_W;

   begin
      GetTextMetrics (Canvas.HDC, M => W);
      R.Height := W.Height;
      R.Ascent := W.Ascent;
      R.Descent := W.Descent;
      R.Internal_Leading := W.Internal_Leading;
      R.External_Leading := W.External_Leading;
      R.Average_Character_Width := W.Average_Character_Width;
      R.Maximum_Character_Width := W.Maximum_Character_Width;
      R.Weight := W.Weight;
      R.Overhang := W.Overhang;
      R.Digitized_Aspect_X := W.Digitized_Aspect_X;
      R.Digitized_Aspect_Y := W.Digitized_Aspect_Y;
      R.First_Character := W.First_Character;
      R.Last_Character := W.Last_Character;
      R.Default_Character := W.Default_Character;
      R.Word_Break_Character := W.Word_Break_Character;
      R.Italic := W.Italic > 0;
      R.Underlined := W.Underlined > 0;
      R.Struck_Out := W.Struck_Out > 0;

      return R;
   end Current_Font_Metrics;

   -------------------------------
   -- Current_Font_Is_True_Type --
   -------------------------------

   function Current_Font_Is_True_Type (Canvas : in Canvas_Type) return Boolean
   is
      use type Interfaces.C.unsigned_char;

      W : Font_Metrics_W;
   begin
      GetTextMetrics (Canvas.HDC, M => W);
      return (W.Pitch_And_Family and TMPF_TRUETYPE) = TMPF_TRUETYPE;
   end Current_Font_Is_True_Type;

   ----------------------------
   -- Current_Font_Is_Vector --
   ----------------------------

   function Current_Font_Is_Vector (Canvas : in Canvas_Type) return Boolean
   is
      use type Interfaces.C.unsigned_char;

      W : Font_Metrics_W;
   begin
      GetTextMetrics (Canvas.HDC, M => W);
      return (W.Pitch_And_Family and TMPF_VECTOR) = TMPF_VECTOR;
   end Current_Font_Is_Vector;

   function Current_Font_Is_Variable_Width (Canvas : in Canvas_Type)
                                           return Boolean
   is
      use type Interfaces.C.unsigned_char;

      W : Font_Metrics_W;
   begin
      GetTextMetrics (Canvas.HDC, M => W);
      return (W.Pitch_And_Family and TMPF_FIXED_PITCH) = TMPF_FIXED_PITCH;
      --  Note: This is an oddity about Win32 that TMPF_FIXED_PITCH flag
      --        is false when FIXED_PITCH = TRUE
   end Current_Font_Is_Variable_Width;

   ---------------------------------
   -- Current_Font_Is_Device_Font --
   ---------------------------------

   function Current_Font_Is_Device_Font (Canvas : in Canvas_Type)
                                        return Boolean
   is
      use type Interfaces.C.unsigned_char;

      W : Font_Metrics_W;
   begin
      GetTextMetrics (Canvas.HDC, M => W);
      return (W.Pitch_And_Family and TMPF_DEVICE) = TMPF_DEVICE;
   end Current_Font_Is_Device_Font;

end GWindows.Drawing;
