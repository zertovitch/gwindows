--  The following methods were introduced in GWindows.Drawing in 2014 (rev. 242) but
--  since then have led to chronic issues with some versions of the GCC linker
--  - linker error on GdiAlphaBlend.
--
--  Now they are (rev. 359, 2018-09-01) in a new extension of Drawing.

package GWindows.Drawing.Extended is

   ---------------------------------------------
   --  StretchBlt with transparency blending  --
   ---------------------------------------------

   AC_SRC_OVER : constant := 0;
   --  The source bitmap is placed over the destination bitmap
   --  based on the alpha values of the source pixels.
   --
   --  Currently, the only blend operation that has been defined is AC_SRC_OVER
   --  MSDN 18-Oct-2014

   SCA_Opaque : constant := 255;
   --  Set the SourceConstantAlpha value SCA_Opaque when
   --  you only want to use per-pixel alpha values.

   AC_SRC_ALPHA : constant := 1;

   type Blend_Function is record
      BlendOp             : Interfaces.C.unsigned_char;
      BlendFlags          : Interfaces.C.unsigned_char;
      SourceConstantAlpha : Interfaces.C.unsigned_char;
      AlphaFormat         : Interfaces.C.unsigned_char;
   end record;

   Default_Blend_Function : constant Blend_Function :=
     (BlendOp             => AC_SRC_OVER,
      BlendFlags          => 0,            -- Must be zero.
      SourceConstantAlpha => SCA_Opaque,
      AlphaFormat         => AC_SRC_ALPHA
     );

   procedure AlphaBlend
     (Destination_Canvas                    : in out Canvas_Type;
      Destination_X, Destination_Y          : in     Integer;
      Destination_Width, Destination_Height : in     Integer;
      Source_Canvas                         : in     Canvas_Type'Class;
      Source_X, Source_Y                    : in     Integer;
      Source_Width, Source_Height           : in     Integer;
      Blending
        : in     Blend_Function :=  Default_Blend_Function);

   procedure Paint_Transparent_Bitmap
     (Canvas                : in out Canvas_Type;
      Bitmap                : in out GWindows.Drawing_Objects.Bitmap_Type;
      X, Y, Width, Height   : in     Integer;
      Source_Width,
      Source_Height         : in     Integer;
      Blending              :
        in     Blend_Function :=  Default_Blend_Function);
   --  Paint bitmap on a rectangle determined by X, Y, Width, Height on Canvas,
   --  taking Source_Width, Source_Height from bitmap;
   --  use transparency from image

end GWindows.Drawing.Extended;