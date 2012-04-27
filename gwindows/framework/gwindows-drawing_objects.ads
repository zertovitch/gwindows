------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--              G W I N D O W S . D R A W I N G _ O B J E C T S             --
--                                                                          --
--                                 S p e c                                  --
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
-- be located on the web at http://www.gnavi.org/gwindows                   --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Finalization;
with System;

with GWindows.Types;
with GWindows.Colors;

package GWindows.Drawing_Objects is

   -------------------------------------------------------------------------
   --  Drawing_Object_Type
   -------------------------------------------------------------------------
   --  Drawing_Obect_Type is the parent class of all GWindows drawing
   --  objects. It handles automatic resource allocation and deallocation.

   type Drawing_Object_Type is
     new Ada.Finalization.Limited_Controlled with private;
   procedure Initialize (Object : in out Drawing_Object_Type);
   procedure Finalize (Object : in out Drawing_Object_Type);
   --  Insures that drawing objects are properly destroyed, etc.

   procedure Delete (Object : in out Drawing_Object_Type);
   --  Delete internal object

   procedure Handle (Object  : in out Drawing_Object_Type;
                     HOBJECT : in     GWindows.Types.Handle);
   function Handle (Object : in Drawing_Object_Type)
                   return GWindows.Types.Handle;
   --  Win32 Handle to Drawing Object

   procedure Protected_Object (Object : in out Drawing_Object_Type;
                               Value  : in     Boolean := True);
   function Protected_Object (Object : in Drawing_Object_Type)
                             return Boolean;
   --  When true GDI object is protected from deletion, ie. the
   --  internal handle will be set to 0 and Protected_Object property
   --  will be reset to false when deleted, but it will not delete the
   --  Win32 GDI object it wraps. False by default

   -------------------------------------------------------------------------
   --  Bitmap_Type
   -------------------------------------------------------------------------

   type Bitmap_Type is new Drawing_Object_Type with private;

   OBM_CLOSE                  : constant := 32754;
   OBM_UPARROW                : constant := 32753;
   OBM_DNARROW                : constant := 32752;
   OBM_RGARROW                : constant := 32751;
   OBM_LFARROW                : constant := 32750;
   OBM_REDUCE                 : constant := 32749;
   OBM_ZOOM                   : constant := 32748;
   OBM_RESTORE                : constant := 32747;
   OBM_REDUCED                : constant := 32746;
   OBM_ZOOMD                  : constant := 32745;
   OBM_RESTORED               : constant := 32744;
   OBM_UPARROWD               : constant := 32743;
   OBM_DNARROWD               : constant := 32742;
   OBM_RGARROWD               : constant := 32741;
   OBM_LFARROWD               : constant := 32740;
   OBM_MNARROW                : constant := 32739;
   OBM_COMBO                  : constant := 32738;
   OBM_UPARROWI               : constant := 32737;
   OBM_DNARROWI               : constant := 32736;
   OBM_RGARROWI               : constant := 32735;
   OBM_LFARROWI               : constant := 32734;
   OBM_OLD_CLOSE              : constant := 32767;
   OBM_SIZE                   : constant := 32766;
   OBM_OLD_UPARROW            : constant := 32765;
   OBM_OLD_DNARROW            : constant := 32764;
   OBM_OLD_RGARROW            : constant := 32763;
   OBM_OLD_LFARROW            : constant := 32762;
   OBM_BTSIZE                 : constant := 32761;
   OBM_CHECK                  : constant := 32760;
   OBM_CHECKBOXES             : constant := 32759;
   OBM_BTNCORNERS             : constant := 32758;
   OBM_OLD_REDUCE             : constant := 32757;
   OBM_OLD_ZOOM               : constant := 32756;
   OBM_OLD_RESTORE            : constant := 32755;
   --  Stock bitmaps

   procedure Load_Stock_Bitmap (Bitmap       : in out Bitmap_Type;
                                Bitmap_Const : in     Integer);
   --  Loads stock bitmap

   type Bitmap_Conversion_Type is (
      None,
      --  The bitmap is loaded as it was created.
      Monochrome,
      --  The bitmap is converted to monochrome.
      Transparent_Window,
      --  All occurrences of the color in the upper left
      --  hand corner of the bitmap is converted to the
      --  system Window color. (This makes the bitmap
      --  look transparent when drawn on a window.)
      Transparent_Button,
      --  All occurrences of the color in the upper left
      --  hand corner of the bitmap is converted to the
      --  system Button_Face color. (This makes the bitmap
      --  look transparent when drawn on a button or dialog.)
      Button_Conversion);
      --  All occurrences of the Dark Grey
      --  [RGB(128,128,128)] color are converted to the system
      --  Dark_Shadow_3D color; Grey [RGB(192,192,192)] color
      --  are converted to the system Button_Face color; and
      --  Light Grey [RGB(223,223,223)] color are converted
      --  to the system Light_3D color.

   procedure Load_Stock_Bitmap
                (Bitmap       : in out Bitmap_Type;
                 Bitmap_Const :        Integer;
                 Conversion   : in     Bitmap_Conversion_Type);

   procedure Load_Stock_Bitmap
                (Bitmap       : in out Bitmap_Type;
                 Bitmap_Const :        Integer;
                 Conversion   : in     Bitmap_Conversion_Type;
                 X, Y         : in     Integer);

   procedure Load_Bitmap (Bitmap     : in out Bitmap_Type;
                          Name       : in     GString;
                          Conversion : in     Bitmap_Conversion_Type := None);
   --  Loads a bitmap and convert from a resource file,
   --  use #NNN for numeric resources

   procedure Load_Bitmap_From_File
     (Bitmap     : in out Bitmap_Type;
      File_Name  : in     GString;
      Conversion : in     Bitmap_Conversion_Type := None);
   --  Loads a bitmap from a file

   procedure Create_Bitmap (Bitmap         : in out Bitmap_Type;
                            Width          : in     Positive;
                            Height         : in     Positive;
                            Planes         : in     Positive;
                            Bits_Per_Pixel : in     Positive;
                            Bits           : in     System.Address);
   --  Bits is a pointer to a buffer. For example:
   --  Type X is Array (1 .. 24) of Integer;
   --  You would pass in X(1)'Address for Bits

   -------------------------------------------------------------------------
   --  Brush_Type
   -------------------------------------------------------------------------

   type Brush_Type is new Drawing_Object_Type with private;

   type Stock_Brush_Type is (Black_Brush,
                             Dark_Gray_Brush,
                             Gray_Brush,
                             Hollow_Brush,
                             Light_Gray_Brush,
                             White_Brush);

   procedure Create_Stock_Brush (Brush       : in out Brush_Type;
                                 Stock_Brush : in     Stock_Brush_Type);
   --  Create a stock brush

   type Hatch_Style_Type is (Horizontal,
                             Vertical,
                             Cross,           -- #
                             Diagonal_Cross,
                             Back_Diagonal,    -- \
                             Forward_Diagonal); -- /

   procedure Create_Hatch_Brush (Brush : in out Brush_Type;
                                 Style : in     Hatch_Style_Type;
                                 Color : in     GWindows.Colors.Color_Type);
   --  Create a hatch brush

   procedure Create_Solid_Brush (Brush : in out Brush_Type;
                                 Color : in     GWindows.Colors.Color_Type);
   --  Create a solid brush

   procedure Create_System_Color_Brush (Brush       : in out Brush_Type;
                                        Color_Const : in     Integer);
   --  Creates a solid brush with a system color

   procedure Create_Pattern_Brush (Brush          : in out Brush_Type;
                                   Bitmap_Pattern : in     Bitmap_Type'Class);
   --  Creates a pattern brush using a bitmap

   -------------------------------------------------------------------------
   --  Pen_Type
   -------------------------------------------------------------------------

   type Pen_Type is new Drawing_Object_Type with private;

   type Stock_Pen_Type is (White_Pen,
                           Black_Pen,
                           Null_Pen);

   procedure Create_Stock_Pen (Pen       : in out Pen_Type;
                               Stock_Pen : in     Stock_Pen_Type);
   --  Create a stock pen

   type Pen_Style_Type is (Solid,
                           Inside_Frame,
                           Dash,
                           Dot,
                           Dash_Dot,
                           Dash_Dot_Dot,
                           Invisible);
   --  All styles other then Solid and Inside_Frame can only have a width of 1
   --  Inside_Frame paints the entire width of the pen inside of any bounding
   --  rectangle of a shape drawn using the pen

   procedure Create_Pen (Pen   : in out Pen_Type;
                         Style : in     Pen_Style_Type;
                         Width : in     Natural;
                         Color : in     GWindows.Colors.Color_Type);
   --  Create a custom pen
   --  If width = 0 pen is 1 pixel
   --  if width > 1 and style is not solid or inside_frame it is treated as 1

   -------------------------------------------------------------------------
   --  Font_Type
   -------------------------------------------------------------------------

   type Font_Type is new Drawing_Object_Type with private;

   type Stock_Font_Type is (ANSI_Fixed_Width,
                            ANSI_Variable_Width,
                            Default_GUI,
                            OEM_Fixed_Width,
                            System,
                            System_Fixed_Width);

   procedure Create_Stock_Font (Font       : in out Font_Type;
                                Stock_Font : in     Stock_Font_Type);
   --  Create a stock font

   FW_DONTCARE                     : constant := 0;
   FW_THIN                         : constant := 100;
   FW_EXTRALIGHT                   : constant := 200;
   FW_LIGHT                        : constant := 300;
   FW_NORMAL                       : constant := 400;
   FW_MEDIUM                       : constant := 500;
   FW_SEMIBOLD                     : constant := 600;
   FW_BOLD                         : constant := 700;
   FW_EXTRABOLD                    : constant := 800;
   FW_HEAVY                        : constant := 900;
   FW_ULTRALIGHT                   : constant := 200;
   FW_REGULAR                      : constant := 400;
   FW_DEMIBOLD                     : constant := 600;
   FW_ULTRABOLD                    : constant := 800;
   FW_BLACK                        : constant := 900;
   --  Standard Font Weights

   subtype Font_Weight_Type is Natural range 0 .. 1000;

   ANSI_CHARSET : constant := 0;
   DEFAULT_CHARSET : constant := 1;
   SYMBOL_CHARSET : constant := 2;
   MAC_CHARSET : constant := 77;
   SHIFTJIS_CHARSET : constant := 128;
   HANGEUL_CHARSET : constant := 129;
   HANGUL_CHARSET : constant := 129;
   JOHAB_CHARSET : constant := 130;
   GB2312_CHARSET : constant := 134;
   CHINESEBIG5_CHARSET : constant := 136;
   GREEK_CHARSET : constant := 161;
   TURKISH_CHARSET : constant := 162;
   VIETNAMESE_CHARSET : constant := 163;
   HEBREW_CHARSET : constant := 177;
   ARABIC_CHARSET : constant := 178;
   BALTIC_CHARSET : constant := 186;
   RUSSIAN_CHARSET : constant := 204;
   THAI_CHARSET : constant := 222;
   EASTEUROPE_CHARSET : constant := 238;
   OEM_CHARSET : constant := 255;
   --  Character Sets

   procedure Create_Font (Font       : in out Font_Type;
                          Name       : in     GString;
                          Size       : in     Integer;
                          Weight     : in     Font_Weight_Type := FW_NORMAL;
                          Italics    : in     Boolean          := False;
                          Underline  : in     Boolean          := False;
                          Strike_Out : in     Boolean          := False;
                          Angle      : in     Integer          := 0;
                          Char_Set   : in     Integer          := ANSI_CHARSET
                         );
   --  Create a font from system
   --  Size = 0   -  Default Size
   --  Size > 0   -  Cell height
   --  size < 0   -  Abs (size) = Character height
   --
   --  Angle is in 10ths of a degree

   -------------------------------------------------------------------------
   --  Icon_Type
   -------------------------------------------------------------------------

   type Icon_Type is new Drawing_Object_Type with private;

   IDI_APPLICATION            : constant := 32512;
   IDI_HAND                   : constant := 32513;
   IDI_QUESTION               : constant := 32514;
   IDI_EXCLAMATION            : constant := 32515;
   IDI_ASTERISK               : constant := 32516;
   IDI_WINLOGO                : constant := 32517;
   IDI_WARNING                : constant := 32515;
   IDI_ERROR                  : constant := 32513;
   IDI_INFORMATION            : constant := 32516;
   --  Stock icons

   procedure Load_Stock_Icon (Icon       : in out Bitmap_Type;
                              Icon_Const : in     Integer);
   --  Load stock icon

   procedure Load_Icon (Icon : in out Icon_Type;
                        Name : in     GString);
   --  Load an icon from a resource file, us #NNN for numeric resources

   procedure Load_Icon_From_File (Icon      : in out Icon_Type;
                                  File_Name : in     GString);
   --  Load an icon from a file

   procedure Extract_Icon_From_File (Icon      : in out Icon_Type;
                                     File_Name : in     GString;
                                     Index     : in     Integer   := 0);
   --  Extract icon from file_name or associate executable to file_name

private

   type Drawing_Object_Type is
     new Ada.Finalization.Limited_Controlled with
      record
         HOBJECT         : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         Protected_Object : Boolean := False;
      end record;

   type Brush_Type is new Drawing_Object_Type with null record;

   type Pen_Type is new Drawing_Object_Type with null record;

   type Font_Type is new Drawing_Object_Type with null record;

   type Bitmap_Type is new Drawing_Object_Type with null record;

   type Icon_Type is new Drawing_Object_Type with null record;

end GWindows.Drawing_Objects;
