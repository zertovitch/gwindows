package body GWindows.Drawing.Extended is

   -----------------------------------------------------------------------
   -- Paint_Transparent_Bitmap - with image stretching and transparency --
   -----------------------------------------------------------------------

   procedure Paint_Transparent_Bitmap
     (Canvas                : in out Canvas_Type;
      Bitmap                : in out GWindows.Drawing_Objects.Bitmap_Type;
      X, Y, Width, Height   : in     Integer;
      Source_Width,
      Source_Height         : in     Integer;
      Blending              : in     Blend_Function :=  Default_Blend_Function)
   is
      MDC : Memory_Canvas_Type;
      HALFTONE : constant := 4;
      procedure GDI_SetStretchBltMode
        (hdc          : GWindows.Types.Handle := Handle (Canvas);
         iStretchMode : Integer               := HALFTONE);
      pragma Import (StdCall, GDI_SetStretchBltMode, "SetStretchBltMode");
   begin
      Create_Memory_Canvas (MDC, Canvas);
      Select_Object (MDC, Bitmap);
      GDI_SetStretchBltMode;
      AlphaBlend (Canvas, X, Y, Width, Height, MDC, 0, 0,
         Source_Width, Source_Height, Blending);
   end Paint_Transparent_Bitmap;

   procedure AlphaBlend
     (Destination_Canvas                    : in out Canvas_Type;
      Destination_X, Destination_Y          : in     Integer;
      Destination_Width, Destination_Height : in     Integer;
      Source_Canvas                         : in     Canvas_Type'Class;
      Source_X, Source_Y                    : in     Integer;
      Source_Width, Source_Height           : in     Integer;
      Blending
        : in     Blend_Function :=  Default_Blend_Function)
   is
      procedure GDI_AlphaBlend
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
         BLENDFUNC  : Blend_Function        := Blending);
      pragma Import (StdCall, GDI_AlphaBlend, "GdiAlphaBlend");
   begin
      GDI_AlphaBlend;
   end AlphaBlend;

end GWindows.Drawing.Extended;