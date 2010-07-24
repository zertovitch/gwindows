with GWindows.GStrings; use GWindows.GStrings;

package body GWindows.Enum_EMF is

   type ENHMFENUMPROC is access
      function (hDC      : GWindows.Types.Handle;
                lpHTable : Handle_Table_Access;
                lpEMFR   : PEMR_Type;
                nObj     : Integer;
                lpData   : EMF_Enum_Proc) return Integer;
   pragma Convention (Stdcall, ENHMFENUMPROC);
   type Prectangle_Type is access GWindows.Types.Rectangle_Type;

   function EnumEnhMetaFile (hdc           : GWindows.Types.Handle;
                             HENHMETAFILE  : GWindows.Types.Handle;
                             lpEnhMetaFunc : ENHMFENUMPROC;
                             lpData        : EMF_Enum_Proc;
                             lpRect        : Prectangle_Type) return Integer;
   pragma Import (StdCall, EnumEnhMetaFile, "EnumEnhMetaFile");

   function Enum_Func (hDC      : GWindows.Types.Handle;
                       lpHTable : Handle_Table_Access;
                       lpEMFR   : PEMR_Type;
                       nObj     : Integer;
                       lpData   : EMF_Enum_Proc) return Integer;
   pragma Convention (Stdcall, Enum_Func);
   function Enum_Func (hDC      : GWindows.Types.Handle;
                       lpHTable : Handle_Table_Access;
                       lpEMFR   : PEMR_Type;
                       nObj     : Integer;
                       lpData   : EMF_Enum_Proc) return Integer is
      pragma Unreferenced (hDC);
   begin
      lpData (lpEMFR, lpHTable, nObj);
      return 1;
   end Enum_Func;

   function Enumerate_EMF (Canvas    : GWindows.Drawing.Canvas_Type'Class;
                           Emf       : EMF_Type;
                           Enum_Proc : EMF_Enum_Proc;
                           Rect      : GWindows.Types.Rectangle_Type)
                           return Boolean is
   begin
      return EnumEnhMetaFile (GWindows.Drawing.Handle (Canvas),
                              GWindows.Drawing_EMF.Handle (Emf),
                              Enum_Func'Access, Enum_Proc,
                              Rect'Unrestricted_Access) /= 0;
   end Enumerate_EMF;

   function EMR_Name (iType : Integer) return String is
      type EMR_Kind is
         (EMR_NULL,
          EMR_HEADER,
          EMR_POLYBEZIER,
          EMR_POLYGON,
          EMR_POLYLINE,
          EMR_POLYBEZIERTO,
          EMR_POLYLINETO,
          EMR_POLYPOLYLINE,
          EMR_POLYPOLYGON,
          EMR_SETWINDOWEXTEX,
          EMR_SETWINDOWORGEX,
          EMR_SETVIEWPORTEXTEX,
          EMR_SETVIEWPORTORGEX,
          EMR_SETBRUSHORGEX,
          EMR_EOF,
          EMR_SETPIXELV,
          EMR_SETMAPPERFLAGS,
          EMR_SETMAPMODE,
          EMR_SETBKMODE,
          EMR_SETPOLYFILLMODE,
          EMR_SETROP2,
          EMR_SETSTRETCHBLTMODE,
          EMR_SETTEXTALIGN,
          EMR_SETCOLORADJUSTMENT,
          EMR_SETTEXTCOLOR,
          EMR_SETBKCOLOR,
          EMR_OFFSETCLIPRGN,
          EMR_MOVETOEX,
          EMR_SETMETARGN,
          EMR_EXCLUDECLIPRECT,
          EMR_INTERSECTCLIPRECT,
          EMR_SCALEVIEWPORTEXTEX,
          EMR_SCALEWINDOWEXTEX,
          EMR_SAVEDC,
          EMR_RESTOREDC,
          EMR_SETWORLDTRANSFORM,
          EMR_MODIFYWORLDTRANSFORM,
          EMR_SELECTOBJECT,
          EMR_CREATEPEN,
          EMR_CREATEBRUSHINDIRECT,
          EMR_DELETEOBJECT,
          EMR_ANGLEARC,
          EMR_ELLIPSE,
          EMR_RECTANGLE,
          EMR_ROUNDRECT,
          EMR_ARC,
          EMR_CHORD,
          EMR_PIE,
          EMR_SELECTPALETTE,
          EMR_CREATEPALETTE,
          EMR_SETPALETTEENTRIES,
          EMR_RESIZEPALETTE,
          EMR_REALIZEPALETTE,
          EMR_EXTFLOODFILL,
          EMR_LINETO,
          EMR_ARCTO,
          EMR_POLYDRAW,
          EMR_SETARCDIRECTION,
          EMR_SETMITERLIMIT,
          EMR_BEGINPATH,
          EMR_ENDPATH,
          EMR_CLOSEFIGURE,
          EMR_FILLPATH,
          EMR_STROKEANDFILLPATH,
          EMR_STROKEPATH,
          EMR_FLATTENPATH,
          EMR_WIDENPATH,
          EMR_SELECTCLIPPATH,
          EMR_ABORTPATH,
          EMR_69,
          EMR_GDICOMMENT,
          EMR_FILLRGN,
          EMR_FRAMERGN,
          EMR_INVERTRGN,
          EMR_PAINTRGN,
          EMR_EXTSELECTCLIPRGN,
          EMR_BITBLT,
          EMR_STRETCHBLT,
          EMR_MASKBLT,
          EMR_PLGBLT,
          EMR_SETDIBITSTODEVICE,
          EMR_STRETCHDIBITS,
          EMR_EXTCREATEFONTINDIRECTW,
          EMR_EXTTEXTOUTA,
          EMR_EXTTEXTOUTW,
          EMR_POLYBEZIER16,
          EMR_POLYGON16,
          EMR_POLYLINE16,
          EMR_POLYBEZIERTO16,
          EMR_POLYLINETO16,
          EMR_POLYPOLYLINE16,
          EMR_POLYPOLYGON16,
          EMR_POLYDRAW16,
          EMR_CREATEMONOBRUSH,
          EMR_CREATEDIBPATTERNBRUSHPT,
          EMR_EXTCREATEPEN,
          EMR_POLYTEXTOUTA,
          EMR_POLYTEXTOUTW,
          EMR_SETICMMODE,
          EMR_CREATECOLORSPACE,
          EMR_SETCOLORSPACE,
          EMR_DELETECOLORSPACE,
          EMR_GLSRECORD,
          EMR_GLSBOUNDEDRECORD,
          EMR_PIXELFORMAT);
   begin
      if iType in
         EMR_Kind'Pos (EMR_Kind'First) .. EMR_Kind'Pos (EMR_Kind'Last)
      then
         return EMR_Kind'Val (iType)'Img;
      else
         return iType'Img;
      end if;
   end EMR_Name;

   function Text (Emr : PEMR_Type) return GString is
      First : Integer;
   begin
      case Emr.iType is
         when EMR_EXTTEXTOUTA =>
            if Emr.offString >= 76 and
               (Emr.offString + Emr.nChars) <= Emr.nSize then
               First := Emr.offString - 76 + 1;
               return To_GString_From_String
                 (Emr.str (First .. First + Emr.nChars - 1));
            end if;
         when EMR_EXTTEXTOUTW =>
            if Emr.offString >= 76 and
               (Emr.offString + Emr.nChars * 2) <= Emr.nSize then
               First := (Emr.offString - 76) / 2 + 1;
               return Emr.gstr (First .. First + Emr.nChars - 1);
            end if;
         when others =>
            null;
      end case;
      return "";
   end Text;

end GWindows.Enum_EMF;
