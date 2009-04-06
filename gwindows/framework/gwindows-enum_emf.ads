with GWindows.Colors;      use GWindows.Colors;
with GWindows.Drawing;     use GWindows.Drawing;
with GWindows.Drawing_EMF; use GWindows.Drawing_EMF;
with GWindows.Types;       use GWindows.Types;
with Interfaces;           use Interfaces;
with Interfaces.C;         use Interfaces.C;

package GWindows.Enum_EMF is

   EMR_HEADER : constant := 1;             --  wingdi.h:2527
   EMR_POLYBEZIER : constant := 2;             --  wingdi.h:2528
   EMR_POLYGON : constant := 3;             --  wingdi.h:2529
   EMR_POLYLINE : constant := 4;             --  wingdi.h:2530
   EMR_POLYBEZIERTO : constant := 5;             --  wingdi.h:2531
   EMR_POLYLINETO : constant := 6;             --  wingdi.h:2532
   EMR_POLYPOLYLINE : constant := 7;             --  wingdi.h:2533
   EMR_POLYPOLYGON : constant := 8;             --  wingdi.h:2534
   EMR_SETWINDOWEXTEX : constant := 9;             --  wingdi.h:2535
   EMR_SETWINDOWORGEX : constant := 10;            --  wingdi.h:2536
   EMR_SETVIEWPORTEXTEX : constant := 11;            --  wingdi.h:2537
   EMR_SETVIEWPORTORGEX : constant := 12;            --  wingdi.h:2538
   EMR_SETBRUSHORGEX : constant := 13;            --  wingdi.h:2539
   EMR_EOF : constant := 14;            --  wingdi.h:2540
   EMR_SETPIXELV : constant := 15;            --  wingdi.h:2541
   EMR_SETMAPPERFLAGS : constant := 16;            --  wingdi.h:2542
   EMR_SETMAPMODE : constant := 17;            --  wingdi.h:2543
   EMR_SETBKMODE : constant := 18;            --  wingdi.h:2544
   EMR_SETPOLYFILLMODE : constant := 19;            --  wingdi.h:2545
   EMR_SETROP2 : constant := 20;            --  wingdi.h:2546
   EMR_SETSTRETCHBLTMODE : constant := 21;            --  wingdi.h:2547
   EMR_SETTEXTALIGN : constant := 22;            --  wingdi.h:2548
   EMR_SETCOLORADJUSTMENT : constant := 23;            --  wingdi.h:2549
   EMR_SETTEXTCOLOR : constant := 24;            --  wingdi.h:2550
   EMR_SETBKCOLOR : constant := 25;            --  wingdi.h:2551
   EMR_OFFSETCLIPRGN : constant := 26;            --  wingdi.h:2552
   EMR_MOVETOEX : constant := 27;            --  wingdi.h:2553
   EMR_SETMETARGN : constant := 28;            --  wingdi.h:2554
   EMR_EXCLUDECLIPRECT : constant := 29;            --  wingdi.h:2555
   EMR_INTERSECTCLIPRECT : constant := 30;            --  wingdi.h:2556
   EMR_SCALEVIEWPORTEXTEX : constant := 31;            --  wingdi.h:2557
   EMR_SCALEWINDOWEXTEX : constant := 32;            --  wingdi.h:2558
   EMR_SAVEDC : constant := 33;            --  wingdi.h:2559
   EMR_RESTOREDC : constant := 34;            --  wingdi.h:2560
   EMR_SETWORLDTRANSFORM : constant := 35;            --  wingdi.h:2561
   EMR_MODIFYWORLDTRANSFORM : constant := 36;            --  wingdi.h:2562
   EMR_SELECTOBJECT : constant := 37;            --  wingdi.h:2563
   EMR_CREATEPEN : constant := 38;            --  wingdi.h:2564
   EMR_CREATEBRUSHINDIRECT : constant := 39;            --  wingdi.h:2565
   EMR_DELETEOBJECT : constant := 40;            --  wingdi.h:2566
   EMR_ANGLEARC : constant := 41;            --  wingdi.h:2567
   EMR_ELLIPSE : constant := 42;            --  wingdi.h:2568
   EMR_RECTANGLE : constant := 43;            --  wingdi.h:2569
   EMR_ROUNDRECT : constant := 44;            --  wingdi.h:2570
   EMR_ARC : constant := 45;            --  wingdi.h:2571
   EMR_CHORD : constant := 46;            --  wingdi.h:2572
   EMR_PIE : constant := 47;            --  wingdi.h:2573
   EMR_SELECTPALETTE : constant := 48;            --  wingdi.h:2574
   EMR_CREATEPALETTE : constant := 49;            --  wingdi.h:2575
   EMR_SETPALETTEENTRIES : constant := 50;            --  wingdi.h:2576
   EMR_RESIZEPALETTE : constant := 51;            --  wingdi.h:2577
   EMR_REALIZEPALETTE : constant := 52;            --  wingdi.h:2578
   EMR_EXTFLOODFILL : constant := 53;            --  wingdi.h:2579
   EMR_LINETO : constant := 54;            --  wingdi.h:2580
   EMR_ARCTO : constant := 55;            --  wingdi.h:2581
   EMR_POLYDRAW : constant := 56;            --  wingdi.h:2582
   EMR_SETARCDIRECTION : constant := 57;            --  wingdi.h:2583
   EMR_SETMITERLIMIT : constant := 58;            --  wingdi.h:2584
   EMR_BEGINPATH : constant := 59;            --  wingdi.h:2585
   EMR_ENDPATH : constant := 60;            --  wingdi.h:2586
   EMR_CLOSEFIGURE : constant := 61;            --  wingdi.h:2587
   EMR_FILLPATH : constant := 62;            --  wingdi.h:2588
   EMR_STROKEANDFILLPATH : constant := 63;            --  wingdi.h:2589
   EMR_STROKEPATH : constant := 64;            --  wingdi.h:2590
   EMR_FLATTENPATH : constant := 65;            --  wingdi.h:2591
   EMR_WIDENPATH : constant := 66;            --  wingdi.h:2592
   EMR_SELECTCLIPPATH : constant := 67;            --  wingdi.h:2593
   EMR_ABORTPATH : constant := 68;            --  wingdi.h:2594
   EMR_GDICOMMENT : constant := 70;            --  wingdi.h:2596
   EMR_FILLRGN : constant := 71;            --  wingdi.h:2597
   EMR_FRAMERGN : constant := 72;            --  wingdi.h:2598
   EMR_INVERTRGN : constant := 73;            --  wingdi.h:2599
   EMR_PAINTRGN : constant := 74;            --  wingdi.h:2600
   EMR_EXTSELECTCLIPRGN : constant := 75;            --  wingdi.h:2601
   EMR_BITBLT : constant := 76;            --  wingdi.h:2602
   EMR_STRETCHBLT : constant := 77;            --  wingdi.h:2603
   EMR_MASKBLT : constant := 78;            --  wingdi.h:2604
   EMR_PLGBLT : constant := 79;            --  wingdi.h:2605
   EMR_SETDIBITSTODEVICE : constant := 80;            --  wingdi.h:2606
   EMR_STRETCHDIBITS : constant := 81;            --  wingdi.h:2607
   EMR_EXTCREATEFONTINDIRECTW : constant := 82;            --  wingdi.h:2608
   EMR_EXTTEXTOUTA : constant := 83;            --  wingdi.h:2609
   EMR_EXTTEXTOUTW : constant := 84;            --  wingdi.h:2610
   EMR_POLYBEZIER16 : constant := 85;            --  wingdi.h:2611
   EMR_POLYGON16 : constant := 86;            --  wingdi.h:2612
   EMR_POLYLINE16 : constant := 87;            --  wingdi.h:2613
   EMR_POLYBEZIERTO16 : constant := 88;            --  wingdi.h:2614
   EMR_POLYLINETO16 : constant := 89;            --  wingdi.h:2615
   EMR_POLYPOLYLINE16 : constant := 90;            --  wingdi.h:2616
   EMR_POLYPOLYGON16 : constant := 91;            --  wingdi.h:2617
   EMR_POLYDRAW16 : constant := 92;            --  wingdi.h:2618
   EMR_CREATEMONOBRUSH : constant := 93;            --  wingdi.h:2619
   EMR_CREATEDIBPATTERNBRUSHPT : constant := 94;            --  wingdi.h:2620
   EMR_EXTCREATEPEN : constant := 95;            --  wingdi.h:2621
   EMR_POLYTEXTOUTA : constant := 96;            --  wingdi.h:2622
   EMR_POLYTEXTOUTW : constant := 97;            --  wingdi.h:2623

   MWT_IDENTITY : constant := 1;             --  wingdi.h:336
   MWT_LEFTMULTIPLY : constant := 2;             --  wingdi.h:337
   MWT_RIGHTMULTIPLY : constant := 3;             --  wingdi.h:338

   type Point_16 is
      record
         X, Y : Interfaces.C.short;
      end record;
   type Point_16_Array is array (Positive range <>) of Point_16;
   pragma Convention (C, Point_16_Array);
   type SIZEL is
      record
         cx : Interfaces.C.long;
         cy : Interfaces.C.long;
      end record;
   pragma Convention (C_Pass_By_Copy, SIZEL);

   type LOGPEN is
      record
         lopnStyle : Interfaces.C.int;
         lopnWidth : Point_Type;
         lopnColor : Interfaces.C.long;
      end record;
   pragma Convention (C_Pass_By_Copy, LOGPEN);

   type Integer_Array is array (Integer range <>) of Integer;
   type EXTLOGPEN is
      record
         elpPenStyle   : Interfaces.C.long;
         elpWidth      : Interfaces.C.long;
         elpBrushStyle : Interfaces.C.int;
         elpColor      : Interfaces.C.long;
         elpHatch      : Interfaces.C.long;
         elpNumEntries : Interfaces.C.long;
         elpStyleEntry : Integer_Array (Integer range 0 .. 1);
      end record;
   pragma Convention (C_Pass_By_Copy, EXTLOGPEN);

   type EMR_Type (iType : Integer) is
      record
         nSize : Integer;  --  size of the record, in bytes
         case iType is
            when EMR_HEADER |
                 EMR_EXTTEXTOUTA | EMR_EXTTEXTOUTW | EMR_POLYLINE16 |
                 EMR_POLYBEZIER16 | EMR_POLYGON16 | EMR_POLYBEZIERTO16 |
                 EMR_POLYLINETO16 | EMR_ELLIPSE =>
               rclBounds : GWindows.Types.Rectangle_Type;
               case iType is
                  when EMR_HEADER =>
                     rclFrame       : Rectangle_Type;
                     dSignature     : Integer;
                     nVersion       : Integer;
                     nBytes         : Integer;
                     nRecords       : Integer;
                     nHandles       : Integer_16;
                     sReserved      : Integer_16;
                     nDescription   : Integer;
                     offDescription : Integer;
                     nPalEntries    : Integer;
                     szlDevice      : SIZEL;
                     szlMillimeters : SIZEL;
                     cbPixelFormat  : Integer;
                     offPixelFormat : Integer;
                     bOpenGL        : Integer;
                     szlMicrometers : SIZEL;
                  when EMR_EXTTEXTOUTA | EMR_EXTTEXTOUTW =>
                     iGraphicsMode : Integer;
                     exScale       : Float;
                     eyScale       : Float;
                     ptlReference  : GWindows.Types.Point_Type;
                     nChars        : Integer;
                     offString     : Integer;
                     fOptions      : Integer;
                     rcl           : GWindows.Types.Rectangle_Type;
                     offDx         : Integer;
                     case iType is
                        when EMR_EXTTEXTOUTA =>
                           str : String (1 .. 1_000_000);
                        when EMR_EXTTEXTOUTW =>
                           gstr : GString (1 .. 1_000_000);
                        when others =>
                           null;
                     end case;
                  when EMR_POLYLINE16 | EMR_POLYBEZIER16 | EMR_POLYGON16 |
                       EMR_POLYBEZIERTO16 | EMR_POLYLINETO16 =>
                     cpts : Integer;
                     apts : Point_16_Array (1 .. 1_000);
                  when others =>
                     null;
               end case;
            when EMR_SETWORLDTRANSFORM | EMR_MODIFYWORLDTRANSFORM =>
               --  Transformation of coordinates (x, y) in world space
               --  to coordinates in page space (x', y')
               --  x' = x * eM11 + y * eM21 + eDx
               --  y' = x * eM12 + y * eM22 + eDy
               Transform : GWindows.Drawing.XFORM;
               case iType is
                  when EMR_MODIFYWORLDTRANSFORM =>
                     iMode : Integer;
                  when others =>
                     null;
               end case;
            when EMR_SETTEXTCOLOR =>
               crColor : Color_Type;
            when EMR_CREATEPEN | EMR_EXTCREATEPEN =>
               ihPen : Integer;
               case iType is
                  when EMR_CREATEPEN =>
                     lopn  : LOGPEN;
                  when EMR_EXTCREATEPEN =>
                     offBmi  : Integer;
                     cbBmi   : Integer;
                     offBits : Integer;
                     cbBits  : Integer;
                     elp     : EXTLOGPEN;
                  when others =>
                     null;
               end case;
            when EMR_SELECTOBJECT =>
               ihObject : Integer;
            when others =>
               null;
         end case;
      end record;
   type PEMR_Type is access EMR_Type;

   type Handle_Table is array (Integer range <>) of GWindows.Types.Handle;
   type Handle_Table_Access is access Handle_Table;

   type EMF_Enum_Proc is access procedure (Emr          : PEMR_Type;
                                           Handle_Table : Handle_Table_Access;
                                           Handle_Count : Integer);

   --  Enumerate all records of an EMF file
   --  Returns True if the enumeration was successful
   function Enumerate_EMF (Canvas    : GWindows.Drawing.Canvas_Type'Class;
                           Emf       : EMF_Type;
                           Enum_Proc : EMF_Enum_Proc;
                           Rect      : GWindows.Types.Rectangle_Type)
                           return Boolean;

   function EMR_Name (iType : Integer) return String;

   function Text (Emr : PEMR_Type) return GString;

end GWindows.Enum_EMF;
