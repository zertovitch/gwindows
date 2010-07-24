------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--        G W I N D O W S . D R A W I N G . C A P A B I L I T I E S         --
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

with Interfaces.C;

package GWindows.Drawing.Capabilities is

   function Get_Capability (Canvas : in Information_Canvas_Type'Class;
                            Index  : in Integer)
                           return Interfaces.C.unsigned;

   --  Returns a capability of the device associated with canvas
   --  Use one of the capability indexes below. The return value
   --  may also be one of the capability flags listed below.

   --  Capability Indexes

   DRIVERVERSION                   : constant := 0;
   --  GetDeviceCaps: Version number
   TECHNOLOGY                      : constant := 2;
   --  GetDeviceCaps: Device type returns DT_PLOTTER, DT_RASDISPLAY,
   --  DT_RASPRINTER, DT_RASCAMERA, DT_CHARSTREAM, DT_METAFILE, or DT_DISPFILE
   HORZSIZE                        : constant := 4;
   --  GetDeviceCaps: Width of display in millimeters
   VERTSIZE                        : constant := 6;
   --  GetDeviceCaps: Height of display in millimeters
   HORZRES                         : constant := 8;
   --  GetDeviceCaps: Width of display in pixels
   VERTRES                         : constant := 10;
   --  GetDeviceCaps: Height of display in raster lines
   BITSPIXEL                       : constant := 12;
   --  GetDeviceCaps: Color bits per pixel
   PLANES                          : constant := 14;
   --  GetDeviceCaps: Color planes
   NUMBRUSHES                      : constant := 16;
   --  GetDeviceCaps: Number of device-specific brushes
   NUMPENS                         : constant := 18;
   --  GetDeviceCaps: Number of device-specific pens
   NUMMARKERS                      : constant := 20;
   --  GetDeviceCaps: Number of device-specific markers
   NUMFONTS                        : constant := 22;
   --  GetDeviceCaps: Number of device-specific fonts
   NUMCOLORS                       : constant := 24;
   --  GetDeviceCaps: Entries in device color table
   PDEVICESIZE                     : constant := 26;
   --  GetDeviceCaps: Size of PDEVICE structure in bytes
   CURVECAPS                       : constant := 28;
   --  GetDeviceCaps: Curve capability flags - CC_NONE, CC_CIRCLES, CC_PIE,
   --  CC_CHORD, CC_ELLIPSES, CC_WIDE, CC_STYLED, CC_WIDESTYLED, CC_INTERIORS,
   --  CC_ROUNDRECT
   LINECAPS                        : constant := 30;
   --  GetDeviceCaps: Line capability flags - LC_NONE, LC_POLYLINE, LC_MARKER,
   --  LC_POLYMARKER, LC_WIDE, LC_STYLED, LC_WIDESTYLED, LC_INTERIORS
   POLYGONALCAPS                   : constant := 32;
   --  GetDeviceCaps: Polygon capability flags - PC_NONE, PC_POLYGON,
   --  PC_RECTANGLE, PC_WINDPOLYGON, PC_SCANLINE, PC_WIDE, PC_STYLED,
   --  PC_WIDESTYLED, PC_INTERIORS
   TEXTCAPS                        : constant := 34;
   --  GetDeviceCaps: Text capability flags - TC_OP_STROKE, TC_CR_90,
   --  TC_CR_ANY, TC_IA_ABLE, TC_UA_ABLE, TC_SO_ABLE, TC_RA_ABLE, TC_VA_ABLE,
   --  Etc.
   CLIPCAPS                        : constant := 36;
   --  GetDeviceCaps: Clipping capability flags - CP_NONE, CP_RECTANGLE,
   --  CP_REGION
   RASTERCAPS                      : constant := 38;
   --  GetDeviceCaps: Raster capability flags - RC_BITBLT, RC_SCALING,
   --  RC_SAVEBITMAP, RC_DI_BITMAP, RC_PALETTE, RC_STRETCHBLT, RC_FLOODFILL,
   --  RC_STRETCHDIB, Etc.
   ASPECTX                         : constant := 40;
   --  GetDeviceCaps: X Aspect
   ASPECTY                         : constant := 42;
   --  GetDeviceCaps: Y Aspect
   ASPECTXY                        : constant := 44;
   --  GetDeviceCaps: X/Y Aspect
   LOGPIXELSX                      : constant := 88;
   --  GetDeviceCaps: X pixels per logical inch
   LOGPIXELSY                      : constant := 90;
   --  GetDeviceCaps: Y pixels per logical inch
   SIZEPALETTE                     : constant := 104;
   --  GetDeviceCaps: Entries in system palette
   NUMRESERVED                     : constant := 106;
   --  GetDeviceCaps: Reserved entries in system palette
   COLORRES                        : constant := 108;
   --  GetDeviceCaps: Color resolution in bits per pixel
   PHYSICALWIDTH                   : constant := 110;
   PHYSICALHEIGHT                  : constant := 111;
   PHYSICALOFFSETX                 : constant := 112;
   PHYSICALOFFSETY                 : constant := 113;
   SCALINGFACTORX                  : constant := 114;
   SCALINGFACTORY                  : constant := 115;
   VREFRESH                        : constant := 116;
   DESKTOPVERTRES                  : constant := 117;
   DESKTOPHORZRES                  : constant := 118;
   BLTALIGNMENT                    : constant := 119;

   --  Capability Flags

   DT_PLOTTER                      : constant := 0;
   --  GetDeviceCaps: TECHNOLOGY flag
   DT_RASDISPLAY                   : constant := 1;
   --  GetDeviceCaps: TECHNOLOGY flag
   DT_RASPRINTER                   : constant := 2;
   --  GetDeviceCaps: TECHNOLOGY flag
   DT_RASCAMERA                    : constant := 3;
   --  GetDeviceCaps: TECHNOLOGY flag
   DT_CHARSTREAM                   : constant := 4;
   --  GetDeviceCaps: TECHNOLOGY flag
   DT_METAFILE                     : constant := 5;
   --  GetDeviceCaps: TECHNOLOGY flag
   DT_DISPFILE                     : constant := 6;
   --  GetDeviceCaps: TECHNOLOGY flag
   CC_NONE                         : constant := 0;
   --  GetDeviceCaps: CURVECAPS flag
   CC_CIRCLES                      : constant := 1;
   --  GetDeviceCaps: CURVECAPS flag
   CC_PIE                          : constant := 2;
   --  GetDeviceCaps: CURVECAPS flag
   CC_CHORD                        : constant := 4;
   --  GetDeviceCaps: CURVECAPS flag
   CC_ELLIPSES                     : constant := 8;
   --  GetDeviceCaps: CURVECAPS flag
   CC_WIDE                         : constant := 16;
   --  GetDeviceCaps: CURVECAPS flag
   CC_STYLED                       : constant := 32;
   --  GetDeviceCaps: CURVECAPS flag
   CC_WIDESTYLED                   : constant := 64;
   --  GetDeviceCaps: CURVECAPS flag
   CC_INTERIORS                    : constant := 128;
   --  GetDeviceCaps: CURVECAPS flag
   CC_ROUNDRECT                    : constant := 256;
   --  GetDeviceCaps: CURVECAPS flag
   LC_NONE                         : constant := 0;
   --  GetDeviceCaps: LINECAPS flag
   LC_POLYLINE                     : constant := 2;
   --  GetDeviceCaps: LINECAPS flag
   LC_MARKER                       : constant := 4;
   --  GetDeviceCaps: LINECAPS flag
   LC_POLYMARKER                   : constant := 8;
   --  GetDeviceCaps: LINECAPS flag
   LC_WIDE                         : constant := 16;
   --  GetDeviceCaps: LINECAPS flag
   LC_STYLED                       : constant := 32;
   --  GetDeviceCaps: LINECAPS flag
   LC_WIDESTYLED                   : constant := 64;
   --  GetDeviceCaps: LINECAPS flag
   LC_INTERIORS                    : constant := 128;
   --  GetDeviceCaps: LINECAPS flag
   PC_NONE                         : constant := 0;
   --  GetDeviceCaps: POLYCONALCAPS flag
   PC_POLYGON                      : constant := 1;
   --  GetDeviceCaps: POLYCONALCAPS flag
   PC_RECTANGLE                    : constant := 2;
   --  GetDeviceCaps: POLYCONALCAPS flag
   PC_WINDPOLYGON                  : constant := 4;
   --  GetDeviceCaps: POLYCONALCAPS flag
   PC_SCANLINE                     : constant := 8;
   --  GetDeviceCaps: POLYCONALCAPS flag
   PC_WIDE                         : constant := 16;
   --  GetDeviceCaps: POLYCONALCAPS flag
   PC_STYLED                       : constant := 32;
   --  GetDeviceCaps: POLYCONALCAPS flag
   PC_WIDESTYLED                   : constant := 64;
   --  GetDeviceCaps: POLYCONALCAPS flag
   PC_INTERIORS                    : constant := 128;
   --  GetDeviceCaps: POLYCONALCAPS flag
   PC_POLYPOLYGON                  : constant := 256;
   PC_PATHS                        : constant := 512;
   CP_NONE                         : constant := 0;
   --  GetDeviceCaps: CLIPCAPS flag
   CP_RECTANGLE                    : constant := 1;
   --  GetDeviceCaps: CLIPCAPS flag
   CP_REGION                       : constant := 2;
   --  GetDeviceCaps: CLIPCAPS flag
   TC_OP_CHARACTER                 : constant := 1;
   --  GetDeviceCaps: TEXTCAPS flag
   TC_OP_STROKE                    : constant := 2;
   --  GetDeviceCaps: TEXTCAPS flag
   TC_CP_STROKE                    : constant := 4;
   --  GetDeviceCaps: TEXTCAPS flag
   TC_CR_90                        : constant := 8;
   --  GetDeviceCaps: TEXTCAPS flag
   TC_CR_ANY                       : constant := 16;
   --  GetDeviceCaps: TEXTCAPS flag
   TC_SF_X_YINDEP                  : constant := 32;
   --  GetDeviceCaps: TEXTCAPS flag
   TC_SA_DOUBLE                    : constant := 64;
   --  GetDeviceCaps: TEXTCAPS flag
   TC_SA_INTEGER                   : constant := 128;
   --  GetDeviceCaps: TEXTCAPS flag
   TC_SA_CONTIN                    : constant := 256;
   --  GetDeviceCaps: TEXTCAPS flag
   TC_EA_DOUBLE                    : constant := 512;
   --  GetDeviceCaps: TEXTCAPS flag
   TC_IA_ABLE                      : constant := 1024;
   --  GetDeviceCaps: TEXTCAPS flag
   TC_UA_ABLE                      : constant := 2048;
   --  GetDeviceCaps: TEXTCAPS flag
   TC_SO_ABLE                      : constant := 4096;
   --  GetDeviceCaps: TEXTCAPS flag
   TC_RA_ABLE                      : constant := 8192;
   --  GetDeviceCaps: TEXTCAPS flag
   TC_VA_ABLE                      : constant := 16384;
   --  GetDeviceCaps: TEXTCAPS flag
   TC_RESERVED                     : constant := 32768;
   TC_SCROLLBLT                    : constant := 65536;
   RC_BITBLT                       : constant := 1;
   --  GetDeviceCaps: RASTERCAPS flag
   RC_BANDING                      : constant := 2;
   --  GetDeviceCaps: RASTERCAPS flag
   RC_SCALING                      : constant := 4;
   --  GetDeviceCaps: RASTERCAPS flag
   RC_BITMAP64                     : constant := 8;
   --  GetDeviceCaps: RASTERCAPS flag
   RC_GDI20_OUTPUT                 : constant := 16;
   --  GetDeviceCaps: RASTERCAPS flag
   RC_GDI20_STATE                  : constant := 32;
   --  GetDeviceCaps: RASTERCAPS flag
   RC_SAVEBITMAP                   : constant := 64;
   --  GetDeviceCaps: RASTERCAPS flag
   RC_DI_BITMAP                    : constant := 128;
   --  GetDeviceCaps: RASTERCAPS flag
   RC_PALETTE                      : constant := 256;
   --  GetDeviceCaps: RASTERCAPS flag
   RC_DIBTODEV                     : constant := 512;
   --  GetDeviceCaps: RASTERCAPS flag
   RC_BIGFONT                      : constant := 1024;
   --  GetDeviceCaps: RASTERCAPS flag
   RC_STRETCHBLT                   : constant := 2048;
   --  GetDeviceCaps: RASTERCAPS flag
   RC_FLOODFILL                    : constant := 4096;
   --  GetDeviceCaps: RASTERCAPS flag
   RC_STRETCHDIB                   : constant := 8192;
   --  GetDeviceCaps: RASTERCAPS flag
   RC_OP_DX_OUTPUT                 : constant := 16384;
   --  GetDeviceCaps: RASTERCAPS flag
   RC_DEVBITS                      : constant := 32768;
   --  GetDeviceCaps: RASTERCAPS flag
   CAPS1                           : constant := 94;
   --  GetDeviceCaps: Other capability flags - C1_TRANSPARENT
   C1_TRANSPARENT                  : constant := 1;
   --  GetDeviceCaps: CAPS1 transparency flag

end GWindows.Drawing.Capabilities;
