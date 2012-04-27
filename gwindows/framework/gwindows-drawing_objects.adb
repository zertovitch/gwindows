------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--              G W I N D O W S . D R A W I N G _ O B J E C T S             --
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
-- be located on the web at http://www.gnavi.org/gwindows                   --
--                                                                          --
------------------------------------------------------------------------------

with System; use System;

with GWindows.GStrings;
with GWindows.Internal;

package body GWindows.Drawing_Objects is
   pragma Linker_Options ("-lShell32");

   use type Interfaces.C.unsigned;

   -------------------------------------------------------------------------
   --  Operating System Imports
   -------------------------------------------------------------------------

   DWHITE_BRUSH                    : constant := 0;
   DLTGRAY_BRUSH                   : constant := 1;
   DGRAY_BRUSH                     : constant := 2;
   DDKGRAY_BRUSH                   : constant := 3;
   DBLACK_BRUSH                    : constant := 4;
--   DNULL_BRUSH                     : constant := 5;
   DHOLLOW_BRUSH                   : constant := 5;

   DWHITE_PEN                      : constant := 6;
   DBLACK_PEN                      : constant := 7;
   DNULL_PEN                       : constant := 8;

   DOEM_FIXED_FONT                 : constant := 10;
   DANSI_FIXED_FONT                : constant := 11;
   DANSI_VAR_FONT                  : constant := 12;
   DSYSTEM_FONT                    : constant := 13;
--   DDEVICE_DEFAULT_FONT            : constant := 14;
--   DDEFAULT_PALETTE                : constant := 15;
   DSYSTEM_FIXED_FONT              : constant := 16;
   DDEFAULT_GUI_FONT               : constant := 17;

   PS_SOLID                        : constant := 0;
   PS_DASH                         : constant := 1;
   PS_DOT                          : constant := 2;
   PS_DASHDOT                      : constant := 3;
   PS_DASHDOTDOT                   : constant := 4;
   PS_NULL                         : constant := 5;
   PS_INSIDEFRAME                  : constant := 6;

   HS_HORIZONTAL                   : constant := 0;
   HS_VERTICAL                     : constant := 1;
   HS_FDIAGONAL                    : constant := 2;
   HS_BDIAGONAL                    : constant := 3;
   HS_CROSS                        : constant := 4;
   HS_DIAGCROSS                    : constant := 5;

   function GetStockObject (Object : Integer)
                           return GWindows.Types.Handle;
   pragma Import (StdCall, GetStockObject, "GetStockObject");

   -------------------------------------------------------------------------
   --  Package Body
   -------------------------------------------------------------------------

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out Drawing_Object_Type)
   is
      pragma Warnings (Off, Object);
   begin
      null;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out Drawing_Object_Type) is
      use type GWindows.Types.Handle;
      use type Interfaces.C.long;
   begin
      if Object.HOBJECT /= GWindows.Types.Null_Handle then
         Delete (Object);
      end if;
   end Finalize;

   ------------
   -- Delete --
   ------------

   procedure Delete (Object : in out Drawing_Object_Type) is
      use type Interfaces.C.long;
      use type GWindows.Types.Handle;
      procedure DeleteObject
        (HOBJECT : GWindows.Types.Handle := Object.HOBJECT);
      pragma Import (StdCall, DeleteObject, "DeleteObject");
   begin
      if Object.HOBJECT /= GWindows.Types.Null_Handle then
         if not Object.Protected_Object then
            DeleteObject;
         else
            Object.Protected_Object := False;
         end if;

         Object.HOBJECT := GWindows.Types.Null_Handle;
      end if;
   end Delete;

   ------------
   -- Handle --
   ------------

   procedure Handle (Object  : in out Drawing_Object_Type;
                     HOBJECT : in     GWindows.Types.Handle)
   is
   begin
      Object.HOBJECT := HOBJECT;
   end Handle;

   function Handle (Object : in Drawing_Object_Type)
                   return GWindows.Types.Handle
   is
   begin
      return Object.HOBJECT;
   end Handle;

   ----------------------
   -- Protected_Object --
   ----------------------

   procedure Protected_Object (Object : in out Drawing_Object_Type;
                               Value  : in     Boolean := True)
   is
   begin
      Object.Protected_Object := Value;
   end Protected_Object;

   function Protected_Object (Object : in Drawing_Object_Type)
                             return Boolean
   is
   begin
      return Object.Protected_Object;
   end Protected_Object;

   ------------------------
   -- Create_Stock_Brush --
   ------------------------

   procedure Create_Stock_Brush (Brush       : in out Brush_Type;
                                 Stock_Brush : in     Stock_Brush_Type)
   is
      Brush_Enum : Integer;
   begin
      Delete (Brush);

      case Stock_Brush is
         when Black_Brush =>
            Brush_Enum := DBLACK_BRUSH;
         when Dark_Gray_Brush =>
            Brush_Enum := DDKGRAY_BRUSH;
         when Gray_Brush =>
            Brush_Enum := DGRAY_BRUSH;
         when Hollow_Brush =>
            Brush_Enum := DHOLLOW_BRUSH;
         when Light_Gray_Brush =>
            Brush_Enum := DLTGRAY_BRUSH;
         when White_Brush =>
            Brush_Enum := DWHITE_BRUSH;
      end case;

      Handle (Brush, GetStockObject (Brush_Enum));
   end Create_Stock_Brush;

   --------------------------
   -- Create_Pattern_Brush --
   --------------------------

   procedure Create_Pattern_Brush (Brush          : in out Brush_Type;
                                   Bitmap_Pattern : in     Bitmap_Type'Class)
   is
      function CreatePatternBrush
        (hBitmap : GWindows.Types.Handle := Handle (Bitmap_Pattern))
        return GWindows.Types.Handle;
      pragma Import (StdCall, CreatePatternBrush, "CreatePatternBrush");
   begin
      Delete (Brush);
      Handle (Brush, CreatePatternBrush);
   end Create_Pattern_Brush;

   -----------------------
   -- Create_Stock_Font --
   -----------------------

   procedure Create_Stock_Font (Font       : in out Font_Type;
                                Stock_Font : in     Stock_Font_Type)
   is
      Font_Enum : Integer;
   begin
      Delete (Font);

      case Stock_Font is
         when OEM_Fixed_Width =>
            Font_Enum := DOEM_FIXED_FONT;
         when ANSI_Fixed_Width =>
            Font_Enum := DANSI_FIXED_FONT;
         when ANSI_Variable_Width =>
            Font_Enum := DANSI_VAR_FONT;
         when System =>
            Font_Enum := DSYSTEM_FONT;
         when System_Fixed_Width =>
            Font_Enum := DSYSTEM_FIXED_FONT;
         when Default_GUI =>
            Font_Enum := DDEFAULT_GUI_FONT;
      end case;

      Handle (Font, GetStockObject (Font_Enum));
   end Create_Stock_Font;

   ----------------------
   -- Create_Stock_Pen --
   ----------------------

   procedure Create_Stock_Pen (Pen       : in out Pen_Type;
                               Stock_Pen : in     Stock_Pen_Type)
   is
      Pen_Enum : Integer;
   begin
      Delete (Pen);

      case Stock_Pen is
         when White_Pen =>
            Pen_Enum := DWHITE_PEN;
         when Black_Pen =>
            Pen_Enum := DBLACK_PEN;
         when Null_Pen =>
            Pen_Enum := DNULL_PEN;
      end case;

      Handle (Pen, GetStockObject (Pen_Enum));
   end Create_Stock_Pen;

   -----------------
   -- Create_Font --
   -----------------

   procedure Create_Font (Font       : in out Font_Type;
                          Name       : in     GString;
                          Size       : in     Integer;
                          Weight     : in     Font_Weight_Type := FW_NORMAL;
                          Italics    : in     Boolean          := False;
                          Underline  : in     Boolean          := False;
                          Strike_Out : in     Boolean          := False;
                          Angle      : in     Integer          := 0;
                          Char_Set   : in     Integer          := ANSI_CHARSET
                         )
   is
      C_Text : constant GString_C := GWindows.GStrings.To_GString_C (Name);

      function CreateFont
        (nHeight            : Integer                 := Size;
         nWidth             : Integer                 := 0;
         nEscapement        : Integer                 := Angle;
         nOrientation       : Integer                 := Angle;
         fnWeight           : Font_Weight_Type        := Weight;
         fdwItalid          : Boolean                 := Italics;
         fdwUnderline       : Boolean                 := Underline;
         fdwStrikeOut       : Boolean                 := Strike_Out;
         fdwCharSet         : Integer                 := Char_Set;
         fdwOutputPrecision : Integer                 := 0;
         fdwClipPrecision   : Integer                 := 0;
         fdwQuality         : Integer                 := 0;
         fdwPitchAndFamily  : Integer                 := 0;
         lpszFace           : GString_C := C_Text)
        return GWindows.Types.Handle;
      pragma Import (StdCall, CreateFont,
                       "CreateFont" & Character_Mode_Identifier);
   begin
      Delete (Font);

      Handle (Font, CreateFont);
   end Create_Font;

   ----------------
   -- Create_Pen --
   ----------------

   procedure Create_Pen (Pen   : in out Pen_Type;
                         Style : in     Pen_Style_Type;
                         Width : in     Natural;
                         Color : in     GWindows.Colors.Color_Type)
   is
      Pen_Style : Integer;

      function CreatePen
        (fnPenStyle : Integer;
         nWidth     : Integer;
         crColor    : GWindows.Colors.Color_Type)
        return GWindows.Types.Handle;
      pragma Import (StdCall, CreatePen, "CreatePen");
   begin
      Delete (Pen);

      case Style is
         when Solid =>
            Pen_Style := PS_SOLID;
         when Inside_Frame =>
            Pen_Style := PS_INSIDEFRAME;
         when Dash =>
            Pen_Style := PS_DASH;
         when Dot =>
            Pen_Style := PS_DOT;
         when Dash_Dot =>
            Pen_Style := PS_DASHDOT;
         when Dash_Dot_Dot =>
            Pen_Style := PS_DASHDOTDOT;
         when Invisible =>
            Pen_Style := PS_NULL;
      end case;

      if (Style = Solid) or (Style = Inside_Frame) then
         Handle (Pen, CreatePen (Pen_Style, Width, Color));
      else
         Handle (Pen, CreatePen (Pen_Style, 1, Color));
      end if;
   end Create_Pen;

   ------------------------
   -- Create_Solid_Brush --
   ------------------------

   procedure Create_Solid_Brush (Brush : in out Brush_Type;
                                 Color : in     GWindows.Colors.Color_Type)
   is
      function CreateSolidBrush
        (crColor : GWindows.Colors.Color_Type := Color)
        return GWindows.Types.Handle;
      pragma Import (StdCall, CreateSolidBrush, "CreateSolidBrush");
   begin
      Delete (Brush);

      Handle (Brush, CreateSolidBrush);
   end Create_Solid_Brush;

   ------------------------
   -- Create_Hatch_Brush --
   ------------------------

   procedure Create_Hatch_Brush (Brush : in out Brush_Type;
                                 Style : in     Hatch_Style_Type;
                                 Color : in     GWindows.Colors.Color_Type)
   is
      Brush_Enum : Integer;

      function CreateHatchBrush
        (fnStyle : Integer;
         clrref  : GWindows.Colors.Color_Type)
        return GWindows.Types.Handle;
      pragma Import (StdCall, CreateHatchBrush, "CreateHatchBrush");
   begin
      Delete (Brush);

      case Style is
         when Horizontal =>
            Brush_Enum := HS_HORIZONTAL;
         when Vertical =>
            Brush_Enum := HS_VERTICAL;
         when Cross =>
            Brush_Enum := HS_CROSS;
         when Diagonal_Cross =>
            Brush_Enum := HS_DIAGCROSS;
         when Back_Diagonal =>
            Brush_Enum := HS_BDIAGONAL;
         when Forward_Diagonal =>
            Brush_Enum := HS_FDIAGONAL;
      end case;

      Handle (Brush, CreateHatchBrush (Brush_Enum, Color));
   end Create_Hatch_Brush;

   -------------------------------
   -- Create_System_Color_Brush --
   -------------------------------

   procedure Create_System_Color_Brush (Brush       : in out Brush_Type;
                                        Color_Const : in     Integer)
   is
      function GetSysColorBrush
        (crColor : Integer)
        return GWindows.Types.Handle;
      pragma Import (StdCall, GetSysColorBrush, "GetSysColorBrush");
   begin
      Delete (Brush);

      Handle (Brush, GetSysColorBrush (Color_Const));
   end Create_System_Color_Brush;

   -----------------------
   -- Load_Stock_Bitmap --
   -----------------------

   procedure Load_Stock_Bitmap (Bitmap       : in out Bitmap_Type;
                                Bitmap_Const :        Integer)
   is
      function LoadBitmap
        (hInst     : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         pszBitmap : Integer := Bitmap_Const)
        return GWindows.Types.Handle;
      pragma Import (StdCall, LoadBitmap,
                       "LoadBitmap" & Character_Mode_Identifier);
   begin
      Delete (Bitmap);

      Handle (Bitmap, LoadBitmap);
   end Load_Stock_Bitmap;

   -----------------------
   -- Load_Stock_Bitmap --
   -----------------------

   procedure Load_Stock_Bitmap
                (Bitmap       : in out Bitmap_Type;
                 Bitmap_Const :        Integer;
                 Conversion   : in     Bitmap_Conversion_Type;
                 X, Y         : in     Integer)
   is
      IMAGE_BITMAP       : constant := 0;
      LR_DEFAULTCOLOR    : constant := 16#0000#;
      LR_MONOCHROME      : constant := 16#0001#;
      LR_LOADTRANSPARENT : constant := 16#0020#;
      LR_LOADMAP3DCOLORS : constant := 16#1000#;

      function LoadImage
        (hInst     : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         lpszName  : Integer           := Bitmap_Const;
         uType     : Interfaces.C.int  := IMAGE_BITMAP;
         cxDesired : Interfaces.C.int  := Interfaces.C.int (X);
         cyDesired : Interfaces.C.int  := Interfaces.C.int (Y);
         fuLoad    : Interfaces.C.unsigned)
        return GWindows.Types.Handle;
      pragma Import (StdCall, LoadImage,
                       "LoadImage" & Character_Mode_Identifier);

      Flags : Interfaces.C.unsigned;
   begin
      Delete (Bitmap);

      case Conversion is
         when None =>
            Flags := LR_DEFAULTCOLOR;
         when Monochrome =>
            Flags := LR_MONOCHROME;
         when Transparent_Window =>
            Flags := LR_LOADTRANSPARENT;
         when Transparent_Button =>
            Flags := LR_LOADTRANSPARENT or LR_LOADMAP3DCOLORS;
         when Button_Conversion =>
            Flags := LR_LOADMAP3DCOLORS;
      end case;

      Handle (Bitmap, LoadImage (fuLoad => Flags));
   end Load_Stock_Bitmap;

   -----------------------
   -- Load_Stock_Bitmap --
   -----------------------

   procedure Load_Stock_Bitmap
                (Bitmap       : in out Bitmap_Type;
                 Bitmap_Const :        Integer;
                 Conversion   : in     Bitmap_Conversion_Type)
   is
      IMAGE_BITMAP       : constant := 0;
      LR_DEFAULTCOLOR    : constant := 16#0000#;
      LR_MONOCHROME      : constant := 16#0001#;
      LR_LOADTRANSPARENT : constant := 16#0020#;
      LR_DEFAULTSIZE     : constant := 16#0040#;
      LR_LOADMAP3DCOLORS : constant := 16#1000#;

      function LoadImage
        (hInst     : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         lpszName  : Integer           := Bitmap_Const;
         uType     : Interfaces.C.int  := IMAGE_BITMAP;
         cxDesired : Interfaces.C.int  := 0;
         cyDesired : Interfaces.C.int  := 0;
         fuLoad    : Interfaces.C.unsigned)
        return GWindows.Types.Handle;
      pragma Import (StdCall, LoadImage,
                       "LoadImage" & Character_Mode_Identifier);

      Flags : Interfaces.C.unsigned;
   begin
      Delete (Bitmap);

      case Conversion is
         when None =>
            Flags := LR_DEFAULTSIZE or LR_DEFAULTCOLOR;
         when Monochrome =>
            Flags := LR_DEFAULTSIZE or LR_MONOCHROME;
         when Transparent_Window =>
            Flags := LR_DEFAULTSIZE or LR_LOADTRANSPARENT;
         when Transparent_Button =>
            Flags := LR_DEFAULTSIZE or LR_LOADTRANSPARENT or
                     LR_LOADMAP3DCOLORS;
         when Button_Conversion =>
            Flags := LR_DEFAULTSIZE or LR_LOADMAP3DCOLORS;
      end case;

      Handle (Bitmap, LoadImage (fuLoad => Flags));
   end Load_Stock_Bitmap;

   -----------------
   -- Load_Bitmap --
   -----------------

   procedure Load_Bitmap (Bitmap     : in out Bitmap_Type;
                          Name       : in     GString;
                          Conversion : in     Bitmap_Conversion_Type := None)
   is
      C_Text : constant GString_C := GWindows.GStrings.To_GString_C (Name);

      IMAGE_BITMAP       : constant := 0;
      LR_DEFAULTCOLOR    : constant := 16#0000#;
      LR_MONOCHROME      : constant := 16#0001#;
      LR_LOADTRANSPARENT : constant := 16#0020#;
      LR_DEFAULTSIZE     : constant := 16#0040#;
      LR_LOADMAP3DCOLORS : constant := 16#1000#;

      function LoadImage
        (hInst     : GWindows.Types.Handle :=
           GWindows.Internal.Current_hInstance;
         lpszName  : GString_C         := C_Text;
         uType     : Interfaces.C.int  := IMAGE_BITMAP;
         cxDesired : Interfaces.C.int  := 0;
         cyDesired : Interfaces.C.int  := 0;
         fuLoad    : Interfaces.C.unsigned)
        return GWindows.Types.Handle;
      pragma Import (StdCall, LoadImage,
                       "LoadImage" & Character_Mode_Identifier);

      Flags : Interfaces.C.unsigned;
   begin
      Delete (Bitmap);

      case Conversion is
         when None =>
            Flags := LR_DEFAULTSIZE or LR_DEFAULTCOLOR;
         when Monochrome =>
            Flags := LR_DEFAULTSIZE or LR_MONOCHROME;
         when Transparent_Window =>
            Flags := LR_DEFAULTSIZE or LR_LOADTRANSPARENT;
         when Transparent_Button =>
            Flags := LR_DEFAULTSIZE or LR_LOADTRANSPARENT or
                     LR_LOADMAP3DCOLORS;
         when Button_Conversion =>
            Flags := LR_DEFAULTSIZE or LR_LOADMAP3DCOLORS;
      end case;

      Handle (Bitmap, LoadImage (fuLoad => Flags));
   end Load_Bitmap;

   ---------------------------
   -- Load_Bitmap_From_File --
   ---------------------------

   procedure Load_Bitmap_From_File
     (Bitmap     : in out Bitmap_Type;
      File_Name  : in     GString;
      Conversion : in     Bitmap_Conversion_Type := None)
   is
      C_Text : constant GString_C :=
        GWindows.GStrings.To_GString_C (File_Name);

      IMAGE_BITMAP       : constant := 0;
      LR_LOADFROMFILE    : constant := 16;
      LR_DEFAULTCOLOR    : constant := 16#0000#;
      LR_MONOCHROME      : constant := 16#0001#;
      LR_LOADTRANSPARENT : constant := 16#0020#;
      LR_DEFAULTSIZE     : constant := 16#0040#;
      LR_LOADMAP3DCOLORS : constant := 16#1000#;

      function LoadImage
        (hInst     : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         lpszName  : GString_C         := C_Text;
         uType     : Interfaces.C.int  := IMAGE_BITMAP;
         cxDesired : Interfaces.C.int  := 0;
         cyDesired : Interfaces.C.int  := 0;
         fuLoad    : Interfaces.C.unsigned)
        return GWindows.Types.Handle;
      pragma Import (StdCall, LoadImage,
                       "LoadImage" & Character_Mode_Identifier);

      Flags : Interfaces.C.unsigned;
   begin
      Delete (Bitmap);

      case Conversion is
         when None =>
            Flags := LR_DEFAULTSIZE or LR_DEFAULTCOLOR or LR_LOADFROMFILE;
         when Monochrome =>
            Flags := LR_DEFAULTSIZE or LR_MONOCHROME or LR_LOADFROMFILE;
         when Transparent_Window =>
            Flags := LR_DEFAULTSIZE or LR_LOADTRANSPARENT or LR_LOADFROMFILE;
         when Transparent_Button =>
            Flags := LR_DEFAULTSIZE or LR_LOADTRANSPARENT or
                     LR_LOADMAP3DCOLORS or LR_LOADFROMFILE;
         when Button_Conversion =>
            Flags := LR_DEFAULTSIZE or LR_LOADMAP3DCOLORS or LR_LOADFROMFILE;
      end case;

      Handle (Bitmap, LoadImage (fuLoad => Flags));
   end Load_Bitmap_From_File;

   -------------------
   -- Create_Bitmap --
   -------------------

   procedure Create_Bitmap (Bitmap         : in out Bitmap_Type;
                            Width          : in     Positive;
                            Height         : in     Positive;
                            Planes         : in     Positive;
                            Bits_Per_Pixel : in     Positive;
                            Bits           : in     Address)
   is
      function CreateBitmap
        (nWidth      : Integer := Width;
         nHeight     : Integer := Height;
         cPlanes     : Integer := Planes;
         cBitsPerPel : Integer := Bits_Per_Pixel;
         lpvBits     : Address := Bits)
        return GWindows.Types.Handle;
      pragma Import (StdCall, CreateBitmap, "CreateBitmap");
   begin
      Delete (Bitmap);
      Handle (Bitmap, CreateBitmap);
   end Create_Bitmap;

   ---------------------
   -- Load_Stock_Icon --
   ---------------------

   procedure Load_Stock_Icon (Icon       : in out Bitmap_Type;
                              Icon_Const : in     Integer)
   is
      function LoadIcon
        (hInst   : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         pszIcon : Integer           := Icon_Const)
        return GWindows.Types.Handle;
      pragma Import (StdCall, LoadIcon,
                       "LoadIcon" & Character_Mode_Identifier);
   begin
      Delete (Icon);

      Handle (Icon, LoadIcon);
   end Load_Stock_Icon;

   ---------------
   -- Load_Icon --
   ---------------

   procedure Load_Icon (Icon : in out Icon_Type;
                        Name : in     GString)
   is
      C_Text : constant GString_C := GWindows.GStrings.To_GString_C (Name);

      function LoadIcon
        (hInst   : GWindows.Types.Handle :=
           GWindows.Internal.Current_hInstance;
         pszIcon : GString_C         := C_Text)
        return GWindows.Types.Handle;
      pragma Import (StdCall, LoadIcon,
                       "LoadIcon" & Character_Mode_Identifier);
   begin
      Delete (Icon);

      Handle (Icon, LoadIcon);
   end Load_Icon;

   -------------------------
   -- Load_Icon_From_File --
   -------------------------

   procedure Load_Icon_From_File (Icon      : in out Icon_Type;
                                  File_Name : in     GString)
   is
      C_Text : constant GString_C :=
        GWindows.GStrings.To_GString_C (File_Name);

      IMAGE_ICON      : constant := 1;
      LR_LOADFROMFILE : constant := 16;

      function LoadImage
        (hInst     : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         lpszName  : GString_C         := C_Text;
         uType     : Interfaces.C.int  := IMAGE_ICON;
         cxDesired : Interfaces.C.int  := 0;
         cyDesired : Interfaces.C.int  := 0;
         fuLoad    : Interfaces.C.int  := LR_LOADFROMFILE)
        return GWindows.Types.Handle;
      pragma Import (StdCall, LoadImage,
                       "LoadImage" & Character_Mode_Identifier);
   begin
      Delete (Icon);

      Handle (Icon, LoadImage);
   end Load_Icon_From_File;

   ----------------------------
   -- Extract_Icon_From_File --
   ----------------------------

   procedure Extract_Icon_From_File (Icon      : in out Icon_Type;
                                     File_Name : in     GString;
                                     Index     : in     Integer   := 0)
   is
      C_Text : constant GString_C :=
        GWindows.GStrings.To_GString_C (File_Name);

      function ExtractAssociatedIcon
        (hInst    : GWindows.Types.Handle :=
           GWindows.Internal.Current_hInstance;
         lpszName : GString_C          := C_Text;
         lpiIcon  : Interfaces.C.short := Interfaces.C.short (Index))
        return GWindows.Types.Handle;
      pragma Import (StdCall, ExtractAssociatedIcon,
                       "ExtractAssociatedIcon" & Character_Mode_Identifier);
   begin
      Delete (Icon);

      Handle (Icon, ExtractAssociatedIcon);
   end Extract_Icon_From_File;

end GWindows.Drawing_Objects;
