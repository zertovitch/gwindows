with GWindows.Colors;               use GWindows.Colors;
with GWindows.Drawing;              use GWindows.Drawing;
with GWindows.Drawing_Objects;      use GWindows.Drawing_Objects;
with GWindows.Types;                use GWindows.Types;
with Interfaces.C;
with System;

package GWindows.DIBitmaps is

   Not_Valid_Error : exception;

   SOURCE_COPY      : constant  := 16#00CC0020#;
   SOURCE_PAINT     : constant  := 16#00EE0086#;
   SOURCE_AND       : constant  := 16#008800C6#;
   SOURCE_INVERT    : constant  := 16#00660046#;
   SOURCE_ERASE     : constant  := 16#00440328#;
   NOT_SOURCE_COPY  : constant  := 16#00330008#;
   NOT_SOURCE_ERASE : constant  := 16#001100A6#;
   MERGE_COPY       : constant  := 16#00C000CA#;
   MERGE_PAINT      : constant  := 16#00BB0226#;
   PATTERN_COPY     : constant  := 16#00F00021#;
   PATTERN_PAINT    : constant  := 16#00FB0A09#;
   PATTERN_INVERT   : constant  := 16#005A0049#;
   DEST_INVERT      : constant  := 16#00550009#;
   TRANSPARENT      : constant  := 16#00AA0029#;
   BLACKNESS        : constant  := 16#00000042#;
   WHITENESS        : constant  := 16#00FF0062#;

   type Color_Depth_Type is (Unknown, Mono, EGA, VGA, Triple, RGB);
   for Color_Depth_Type use
      (Unknown => 0, Mono => 1, EGA => 4, VGA => 8, Triple => 24, RGB => 32);
   for Color_Depth_Type'Size use 16;

   type DIB_Compression_Type is (BI_RGB, BI_RLE8, BI_RLE4, BI_BITFIELDS);
   for DIB_Compression_Type use
      (BI_RGB => 0, BI_RLE8 => 1, BI_RLE4 => 2, BI_BITFIELDS => 3);
   for DIB_Compression_Type'Size use 32;

   type DIB_Color_Meaning_Type is (DIB_RGB_COLORS, DIB_PAL_COLORS);
   for DIB_Color_Meaning_Type use (DIB_RGB_COLORS => 0, DIB_PAL_COLORS => 1);
   for DIB_Color_Meaning_Type'Size use Interfaces.C.unsigned'Size;

   type Pixel_Byte_Type is range 0 .. 255;
   for Pixel_Byte_Type'Size  use 8;
   type Pixel_Byte_Ptr_Type is access all Pixel_Byte_Type;

   type Windows_Info is
   record
      Header_Size : Integer range 40 .. 40 := 40;
      Width,
      Height      : Integer := 0;
      Planes      : Short_Integer range 1 .. 1 := 1;
      Color_Depth : Color_Depth_Type := Unknown;
      Compression : DIB_Compression_Type := BI_RGB;
      Image_Size  : Integer := 0;
   end record;

   for Windows_Info use
      record
         Header_Size at  0 range 0 .. 31;
         Width       at  4 range 0 .. 31;
         Height      at  8 range 0 .. 31;
         Planes      at 12 range 0 .. 15;
         Color_Depth at 14 range 0 .. 15;
         Compression at 16 range 0 .. 31;
         Image_Size  at 20 range 0 .. 31;
      end record;
   for Windows_Info'Size use 24 * 8;

   type DIB_Info_Header_Type is
      record
         Private_Info     : Windows_Info;
         --  These values set only by application program.
         X_Pixels_Per_Meter,
           Y_Pixels_Per_Meter,
           Colors_Used      : Integer := 0;  -- 0 => full size color table
         Colors_Important : Integer := 0;
      end record;
   for DIB_Info_Header_Type use
      record
         Private_Info       at  0 range 0 .. 24 * 8 - 1;
         X_Pixels_Per_Meter at 24 range 0 .. 31;
         Y_Pixels_Per_Meter at 28 range 0 .. 31;
         Colors_Used        at 32 range 0 .. 31;
         Colors_Important   at 36 range 0 .. 31;
      end record;
   for DIB_Info_Header_Type'Alignment use 4;
   for DIB_Info_Header_Type'Size use 40 * 8;
   type DIB_Info_Header_Ptr_Type is access all DIB_Info_Header_Type;

   type Root_DIBitmap_Type (Height, Width : Integer) is
      abstract tagged record
        Bitmap_Info_Header : aliased DIB_Info_Header_Type :=
          (Private_Info => (Header_Size => 40,
                            Width => Width,
                            Height => Height,
                            Planes => 1,
                            Color_Depth => VGA,
                            Compression => BI_RGB,
                            Image_Size => Width * Height),
           X_Pixels_Per_Meter => 0,
           Y_Pixels_Per_Meter => 0,
           Colors_Used        => 0,
           Colors_Important   => 0);
      end record;
   pragma Warnings (Off, Root_DIBitmap_Type);

--     for Root_DIBitmap_Type use
--        record
--           Height             at  4 range 0 .. 31;
--           Width              at  8 range 0 .. 31;
--           Bitmap_Info_Header at 12 range 0 .. 40 * 8 - 1;
--        end record;

   type Basic_DIBitmap_Type is abstract new Root_DIBitmap_Type with
     null record;

   type DIB_Color_Type is
      record
         Blue,
           Green,
           Red      : Integer range 0 .. 255;
         Reserved : Integer range 0 .. 0 := 0;
      end record;
   for DIB_Color_Type'Size use 32;
   for DIB_Color_Type use
      record
         Blue     at 0 range 0 .. 7;
         Green    at 1 range 0 .. 7;
         Red      at 2 range 0 .. 7;
         Reserved at 3 range 0 .. 7;
      end record;

   type Color_Type is
      record
         Blue,
         Green,
         Red      : Integer range 0 .. 255;
      end record;
   for Color_Type'Size use 24;
   for Color_Type use
      record
         Blue     at 0 range 0 .. 7;
         Green    at 1 range 0 .. 7;
         Red      at 2 range 0 .. 7;
      end record;
   type PDIB_Color_Type is access all Color_Type;

   type DIB_Color_List_Type is array (Natural range <>) of DIB_Color_Type;

   type VGA_Image_Type is array (Integer range <>, Integer range <>)
     of aliased Color_Range;
   for VGA_Image_Type'Alignment use 4;

   type VGA_Color_Index_Ptr_Type is access all Color_Range;

   type VGA_DIBitmap_Type (Height, Width : Integer) is
     new Basic_DIBitmap_Type (Height, Width) with
      record
         Color_Table : DIB_Color_List_Type (0 .. 255);
         Image       : VGA_Image_Type (1 .. Height, 1 .. Width);
      end record;
--     for VGA_DIBitmap_Type use record
--        Color_Table at 52 range 0 .. 256 * 32 - 1;
--     end record;
   type PVGA_DIBitmap_Type is access VGA_DIBitmap_Type;

   type Extended_Image_Type is array (Integer range <>, Integer range <>)
      of aliased Color_Type;

   type Extended_DIBitmap_Type (Height, Width : Integer) is
     new Basic_DIBitmap_Type (Height, Width) with
      record
         Image       : Extended_Image_Type (1 .. Height, 1 .. Width);
      end record;
   type Pextended_DIBitmap_Type is access Extended_DIBitmap_Type;

   function Get_Size (Bitmap : in VGA_DIBitmap_Type) return Size_Type;
   --  Return the size in pixel counts of the bitmap.

   procedure Set_Windows_Components (Bitmap : in out VGA_DIBitmap_Type);
   --
   --  Set the Windows components in Bitmap.
   --  This routine is intended to be used only by Claw.
   --
   procedure Create_DIB_Section
     (Canvas   : Canvas_Type;
      Bminfo   : DIB_Info_Header_Type;
      Bitmap   : in out GWindows.Drawing_Objects.Bitmap_Type;
      Pxls     : out System.Address);
   --  Create a bitmap that matches Canvas, but use data from Bminfo
   --  to create is. This creates a DIB that can be selected into
   --  a memory canvas

   procedure Get_Image_Ptr (Bitmap : in out VGA_DIBitmap_Type;
                            Result : out    Pixel_Byte_Ptr_Type);
   --
   --  Return a pointer to the bitmap bits.
   --  This routine is intended to be used only by Claw.
   --

   procedure Copy (Canvas : in     Canvas_Type'Class;
                   Target : in out Extended_DIBitmap_Type;
                   Source : in     Bitmap_Type'Class);
   --
   --  Retrieves the bits of the specified bitmap and copies them with 24bit
   --  colors in specified DIB.  (Note that the DIB's size was specified when
   --  it was created).

   procedure Copy (Canvas : in     Canvas_Type'Class;
                   Target : in out VGA_DIBitmap_Type;
                   Source : in     Bitmap_Type'Class);
   --
   --  Retrieves the bits of the specified bitmap and copies them into the
   --  specified DIB.  (Note that the DIB's size was specified when it
   --  was created).

   procedure Stretch_To_Canvas (Canvas :       in     Canvas_Type'Class;
                                Target_Point : in     Point_Type;
                                Target_Size :  in     Size_Type;
                                Source_Point : in     Point_Type;
                                Source_Size :  in     Size_Type;
                                DIBitmap :     in out VGA_DIBitmap_Type;
                                Raster_Oper :  in   Interfaces.C.unsigned_long
                                  := SOURCE_COPY);
   --
   --  Copies the color data for a rectangle of pixels in a
   --  device-independent bitmap (DIB) to the specified destination
   --  rectangle.  Smashes or stretches the image to fit.

end GWindows.DIBitmaps;
