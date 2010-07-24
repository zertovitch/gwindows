with Ada.Unchecked_Conversion;
with GWindows.Errors;               use GWindows.Errors;
with GWindows;                      use GWindows;

package body GWindows.DIBitmaps is

   function lpvBits is new Ada.Unchecked_Conversion
     (Source => VGA_Color_Index_Ptr_Type,
      Target => Pixel_Byte_Ptr_Type);
   pragma No_Strict_Aliasing (Pixel_Byte_Ptr_Type);

   function lpvBits is new Ada.Unchecked_Conversion
     (Source => PDIB_Color_Type,
      Target => Pixel_Byte_Ptr_Type);

   function Get_Size (Bitmap : in VGA_DIBitmap_Type)
     return Size_Type is
      --  Return the size in pixel counts of the bitmap.
   begin
      return
        (Width  => Bitmap.Bitmap_Info_Header.Private_Info.Width,
         Height => Bitmap.Bitmap_Info_Header.Private_Info.Height);
   end Get_Size;

   procedure Set_Windows_Components
     (Bitmap : in out Extended_DIBitmap_Type) is
      use type Interfaces.C.int;
   begin
      Bitmap.Bitmap_Info_Header.Private_Info.Header_Size := 40;
      if (Bitmap.Bitmap_Info_Header.Private_Info.Width + 3) / 4 /=
        Bitmap.Width / 4 then
         Bitmap.Bitmap_Info_Header.Private_Info.Width := Bitmap.Width;
      end if;
      Bitmap.Bitmap_Info_Header.Private_Info.Height      := Bitmap.Height;
      Bitmap.Bitmap_Info_Header.Private_Info.Planes      := 1;
      Bitmap.Bitmap_Info_Header.Private_Info.Image_Size  :=
        Bitmap.Width * Bitmap.Height * 3;
      Bitmap.Bitmap_Info_Header.Private_Info.Color_Depth := Triple;
      Bitmap.Bitmap_Info_Header.Private_Info.Compression := BI_RGB;
   end Set_Windows_Components;

   procedure Set_Windows_Components
     (Bitmap : in out VGA_DIBitmap_Type) is
      use type Interfaces.C.int;
   begin
      --  if Bitmap.Width mod 4 /= 0 then raise Claw.Not_Valid_Error;end if;
      Bitmap.Bitmap_Info_Header.Private_Info.Header_Size := 40;
      if (Bitmap.Bitmap_Info_Header.Private_Info.Width + 3) / 4 /=
        Bitmap.Width / 4 then
         Bitmap.Bitmap_Info_Header.Private_Info.Width := Bitmap.Width;
      end if;
      Bitmap.Bitmap_Info_Header.Private_Info.Height      := Bitmap.Height;
      Bitmap.Bitmap_Info_Header.Private_Info.Planes      := 1;
      Bitmap.Bitmap_Info_Header.Private_Info.Color_Depth := VGA;
      Bitmap.Bitmap_Info_Header.Private_Info.Compression := BI_RGB;
      Bitmap.Bitmap_Info_Header.Private_Info.Image_Size  :=
        Bitmap.Width * Bitmap.Height;
   end Set_Windows_Components;

   ------------------------
   -- Create_DIB_Section --
   ------------------------

   procedure Create_DIB_Section
     (Canvas   : Canvas_Type;
      Bminfo   : DIB_Info_Header_Type;
      Bitmap   : in out GWindows.Drawing_Objects.Bitmap_Type;
      Pxls     : out System.Address) is

      function CreateDIBSection
        (hdc      : GWindows.Types.Handle := GWindows.Drawing.Handle (Canvas);
         pbmi     : DIB_Info_Header_Type := Bminfo;
         iUsage   : DIB_Color_Meaning_Type := DIB_RGB_COLORS;
         ppvBits  : access System.Address := Pxls'Unrestricted_Access;
         hSection : GWindows.Types.Handle :=
           GWindows.Types.Null_Handle;
         dwOffset : Integer := 0) return GWindows.Types.Handle;
      pragma Import (Stdcall, CreateDIBSection, "CreateDIBSection");

   begin
      GWindows.Drawing_Objects.Handle (Bitmap, CreateDIBSection);
   end Create_DIB_Section;

   procedure Get_Image_Ptr (Bitmap : in out VGA_DIBitmap_Type;
                            Result : out    Pixel_Byte_Ptr_Type) is
      --
      --  Return a pointer to the bitmap bits.
      --  This routine is intended to be used only by Claw.
      --
      --
      Image_Ptr : VGA_Color_Index_Ptr_Type
        := Bitmap.Image (Bitmap.Image'First (1),
                         Bitmap.Image'First (2))'Unchecked_Access;
   begin
      Result := lpvBits (Image_Ptr);
   end Get_Image_Ptr;

   procedure Get_Image_Ptr (Bitmap : in out Extended_DIBitmap_Type;
                            Result : out    Pixel_Byte_Ptr_Type) is
      --
      --  Return a pointer to the bitmap bits.
      --  This routine is intended to be used only by Claw.
      --
      --
      Image_Ptr : PDIB_Color_Type
        := Bitmap.Image (Bitmap.Image'First (1),
                         Bitmap.Image'First (2))'Unchecked_Access;
   begin
      Result := lpvBits (Image_Ptr);
   end Get_Image_Ptr;

   function Get_DIBits (Hdc        : GWindows.Types.Handle;
                        HBitmap    : GWindows.Types.Handle;
                        Start_Scan : Interfaces.C.unsigned;
                        Scan_Lines : Interfaces.C.unsigned;
                        Bits       : Pixel_Byte_Ptr_Type;
                        BPI        : DIB_Info_Header_Ptr_Type;
                        Usage      : Interfaces.C.unsigned)
     return Interfaces.C.int;

   pragma Import (StdCall, Get_DIBits, "GetDIBits");

   procedure Copy (Canvas : in     Canvas_Type'Class;
                   Target : in out Extended_DIBitmap_Type;
                   Source : in     Bitmap_Type'Class) is
      use type Interfaces.C.long;
      use type Interfaces.C.int;
      --
      --  Retrieves the bits of the specified bitmap and copies them with 24bit
      --  colors in specified DIB. (Note that the DIB's size was specified when
      --  it was created).
      Result    : Interfaces.C.int;
      Info_Ptr  : DIB_Info_Header_Ptr_Type
        := Target.Bitmap_Info_Header'Unchecked_Access;
     Image_Ptr : Pixel_Byte_Ptr_Type;
   begin
      if GWindows.Drawing.Handle (Canvas) = GWindows.Types.Null_Handle or else
         GWindows.Drawing_Objects.Handle (Source) = GWindows.Types.Null_Handle
      then
         raise Not_Valid_Error;
      end if;
      Set_Windows_Components (Target);      --  Dispatches
      Get_Image_Ptr (Target, Image_Ptr);    --  Dispatches

      Result := Get_DIBits (GWindows.Drawing.Handle (Canvas),
                            GWindows.Drawing_Objects.Handle (Source),
                            0, Interfaces.C.unsigned (Target.Height),
                            Image_Ptr,
                            Info_Ptr,
                            DIB_Color_Meaning_Type'Pos (DIB_RGB_COLORS));
      if Result = 0 then
         Error_Check (3);
      end if;
   end Copy;

   procedure Copy (Canvas : in     Canvas_Type'Class;
                   Target : in out VGA_DIBitmap_Type;
                   Source : in     Bitmap_Type'Class) is
      use type Interfaces.C.long;
      use type Interfaces.C.int;
      --
      --  Retrieves the bits of the specified bitmap and copies them into the
      --  specified DIB.  (Note that the DIB's size was specified when it
      --  was created).
      Result    : Interfaces.C.int;
      Info_Ptr  : DIB_Info_Header_Ptr_Type
        := Target.Bitmap_Info_Header'Unchecked_Access;
      Image_Ptr : Pixel_Byte_Ptr_Type;
   begin
      if GWindows.Drawing.Handle (Canvas) = GWindows.Types.Null_Handle or else
         GWindows.Drawing_Objects.Handle (Source) = GWindows.Types.Null_Handle
      then
         raise Not_Valid_Error;
      end if;
      Set_Windows_Components (Target);      --  Dispatches
      Get_Image_Ptr (Target, Image_Ptr);    --  Dispatches

      Result := Get_DIBits (GWindows.Drawing.Handle (Canvas),
                            GWindows.Drawing_Objects.Handle (Source),
                            0, Interfaces.C.unsigned (Target.Height),
                            Image_Ptr,
                            Info_Ptr,
                            DIB_Color_Meaning_Type'Pos (DIB_RGB_COLORS));
      if Result = 0 then
         Error_Check (3);
      end if;
   end Copy;

   function Stretch_DIBits (The_Hdc     : GWindows.Types.Handle;
                            X_Dest      : Interfaces.C.int;
                            Y_Dest      : Interfaces.C.int;
                            Dest_Width  : Interfaces.C.int;
                            Dest_Height : Interfaces.C.int;
                            X_Src       : Interfaces.C.int;
                            Y_Src       : Interfaces.C.int;
                            Src_Width   : Interfaces.C.int;
                            Src_Height  : Interfaces.C.int;
                            Bits        : Pixel_Byte_Ptr_Type;
                            Bits_Info   : DIB_Info_Header_Ptr_Type;
                            Usage       : Interfaces.C.unsigned;
                            Rop         : Interfaces.C.unsigned_long)
     return Interfaces.C.int;
   pragma Import (StdCall, Stretch_DIBits, "StretchDIBits");

   procedure Stretch_To_Canvas (Canvas :       in     Canvas_Type'Class;
                                Target_Point : in     Point_Type;
                                Target_Size :  in     Size_Type;
                                Source_Point : in     Point_Type;
                                Source_Size :  in     Size_Type;
                                DIBitmap :     in out VGA_DIBitmap_Type;
                                Raster_Oper :  in   Interfaces.C.unsigned_long
                                  := SOURCE_COPY) is
      --
      --  Copies the color data for a rectangle of pixels in a
      --  device-independent bitmap (DIB) to the specified destination
      --  rectangle.  Smashes or stretches the image to fit.
      use type Interfaces.C.long;
      use type Interfaces.C.int;
      Result    : Interfaces.C.int;
      Info_Ptr  : DIB_Info_Header_Ptr_Type
        := DIBitmap.Bitmap_Info_Header'Unchecked_Access;
      Image_Ptr : Pixel_Byte_Ptr_Type;
   begin
      if GWindows.Drawing.Handle (Canvas) = GWindows.Types.Null_Handle then
         raise Not_Valid_Error;
      end if;
      Set_Windows_Components (DIBitmap);      --  Dispatches
      Get_Image_Ptr (DIBitmap, Image_Ptr);    --  Dispatches

      Result := Stretch_DIBits (GWindows.Drawing.Handle (Canvas),
                                Interfaces.C.int (Target_Point.X),
                                Interfaces.C.int (Target_Point.Y),
                                Interfaces.C.int (Target_Size.Width),
                                Interfaces.C.int (Target_Size.Height),
                                Interfaces.C.int (Source_Point.X),
                                Interfaces.C.int (Source_Point.Y),
                                Interfaces.C.int (Source_Size.Width),
                                Interfaces.C.int (Source_Size.Height),
                                Image_Ptr,
                                Info_Ptr,
                                DIB_Color_Meaning_Type'Pos (DIB_RGB_COLORS),
                                Raster_Oper);
      if Result = 0 then
         Error_Check (3);
      end if;
   end Stretch_To_Canvas;

end GWindows.DIBitmaps;
