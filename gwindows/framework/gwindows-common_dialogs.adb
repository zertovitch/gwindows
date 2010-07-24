------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--       G W I N D O W S . W I N D O W S . C O M M O N _ D I A L O G S      --
--                                                                          --
--                                 B o d y                                  --
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

with Ada.Unchecked_Conversion;
with Interfaces.C;
with GWindows.Application;
with GWindows.GStrings;
with GWindows.GStrings.Unbounded;
with GNATCOM.Types;

package body GWindows.Common_Dialogs is
   pragma Linker_Options ("-lcomdlg32");

   use type Interfaces.C.unsigned;

   -------------------------------------------------------------------------
   --  Operating System Imports
   ------------------------------------------------------------------------

   CC_RGBINIT              : constant := 1;
--     CC_FULLOPEN             : constant := 2;
--     CC_PREVENTFULLOPEN      : constant := 4;
--     CC_SHOWHELP             : constant := 8;
--     CC_ENABLEHOOK           : constant := 16;
--     CC_ENABLETEMPLATE       : constant := 32;
--     CC_ENABLETEMPLATEHANDLE : constant := 64;
--     CC_SOLIDCOLOR           : constant := 128;
   CC_ANYCOLOR             : constant := 256;
--     OFN_READONLY             : constant := 1;
--     OFN_OVERWRITEPROMPT      : constant := 2;
   OFN_HIDEREADONLY         : constant := 4;
--     OFN_NOCHANGEDIR          : constant := 8;
--     OFN_SHOWHELP             : constant := 16;
   OFN_ENABLEHOOK           : constant := 32;                       --  * AnSp
   OFN_ENABLETEMPLATE       : constant := 64;                       --  * AnSp
--     OFN_ENABLETEMPLATEHANDLE : constant := 128;
--     OFN_NOVALIDATE           : constant := 256;
--     OFN_ALLOWMULTISELECT     : constant := 512;
--     OFN_EXTENSIONDIFFERENT   : constant := 1024;
--     OFN_PATHMUSTEXIST        : constant := 2048;
--     OFN_FILEMUSTEXIST        : constant := 4096;
--     OFN_CREATEPROMPT         : constant := 8192;
--     OFN_SHAREAWARE           : constant := 16384;
--     OFN_NOREADONLYRETURN     : constant := 32768;
--     OFN_NOTESTFILECREATE     : constant := 65536;
--     OFN_NONETWORKBUTTON      : constant := 131072;
--     OFN_NOLONGNAMES          : constant := 262144;
   OFN_EXPLORER             : constant := 524288;                   --  * AnSp
--     OFN_NODEREFERENCELINKS   : constant := 1048576;
--     OFN_LONGNAMES            : constant := 2097152;

   type TCHOOSECOLOR is
      record
         lStructSize    : Interfaces.C.long := 36;
         hwndOwner      : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         hInstance      : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         rgbResult      : GWindows.Colors.Color_Type := 0;
         lpCustColors   : Pointer_To_Color_Array;
         flags          : Interfaces.C.unsigned  := CC_ANYCOLOR or CC_RGBINIT;
         lCustData      : Interfaces.C.long := 0;
         lpfnHook       : Interfaces.C.long := 0;
         lpTemplateName : Interfaces.C.long := 0;
      end record;

   function ChooseColor
     (lpcc : in TCHOOSECOLOR)
     return Integer;
   pragma Import (StdCall, ChooseColor, "ChooseColorA");

   type LPSTR is access all Interfaces.C.char;

   type OFNHookProcStdcall is access
      function (hWnd    : GWindows.Types.Handle;
                uiMsg   : Interfaces.C.unsigned;
                wParam  : GWindows.Types.Wparam;
                lParam  : GWindows.Types.Lparam) return Interfaces.C.long;
   pragma Convention (Stdcall, OFNHookProcStdcall);

   type OPENFILENAME is
      record
         lStructSize     : Integer := OPENFILENAME'Size / 8;
         hwndOwner       : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         hInstance       : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         lpstrFilter       : LPSTR;
         lpstrCustomFilter : LPSTR;
         nMaxCustFilter    : Interfaces.C.long := 0;
         nFilterIndex      : Interfaces.C.long := 0;
         lpstrFile         : LPSTR;
         nMaxFile          : Interfaces.C.long;
         lpstrFileTitle    : LPSTR;
         nMaxFileTitle     : Interfaces.C.long := 0;
         lpstrInitialDir   : LPSTR;
         lpstrTitle        : LPSTR;
         flags             : Interfaces.C.long := OFN_HIDEREADONLY;
         nFileOffset       : Interfaces.C.short := 0;
         nFileExtension    : Interfaces.C.short := 0;
         lpstrDefExt       : LPSTR;
         lCustData         : GWindows.Types.Lparam := 0;
         lpfnHook          : OFNHookProcStdcall;
         lpTemplateName    : LPSTR;
      end record;

   function GetOpenFileName
     (lpOFN : in OPENFILENAME)
     return Integer;
   pragma Import (StdCall, GetOpenFileName, "GetOpenFileNameA");

   function GetSaveFileName
     (lpOFN : in OPENFILENAME)
     return Integer;
   pragma Import (StdCall, GetSaveFileName, "GetSaveFileNameA");

   type Face_Name_Type is new Interfaces.C.char_array (0 .. 32);
   type Pointer_To_Face_Name_Type is access all Face_Name_Type;

   type LOGFONT is
      record
         lfHeight         : Interfaces.C.long := 0;
         lfWidth          : Interfaces.C.long := 0;
         lfEscapement     : Interfaces.C.long := 0;
         lfOrientation    : Interfaces.C.long := 0;
         lfWeight         : Interfaces.C.long := 0;
         lfItalic         : Interfaces.C.unsigned_char := 0;
         lfUnderline      : Interfaces.C.unsigned_char := 0;
         lfStrikeOut      : Interfaces.C.unsigned_char := 0;
         lfCharSet        : Interfaces.C.unsigned_char := 0;
         lfOutPrecision   : Interfaces.C.unsigned_char := 0;
         lfClipPrecision  : Interfaces.C.unsigned_char := 0;
         lfQuality        : Interfaces.C.unsigned_char := 0;
         lfPitchAndFamily : Interfaces.C.unsigned_char := 0;
         lfFaceName       : Pointer_To_Face_Name_Type := null;
      end record;
   type Pointer_To_LOGFONT is access all LOGFONT;

--     CF_SCREENFONTS          : constant := 1;
--     CF_PRINTERFONTS         : constant := 2;
   CF_BOTH                 : constant := 3;
--     CF_SHOWHELP             : constant := 4;
--     CF_FONTSHOWHELP         : constant := 4;
--     CF_ENABLEHOOK           : constant := 8;
--     CF_ENABLETEMPLATE       : constant := 16;
--     CF_ENABLETEMPLATEHANDLE : constant := 32;
   CF_INITTOLOGFONTSTRUCT  : constant := 64;
--     CF_USESTYLE             : constant := 128;
   CF_EFFECTS              : constant := 256;
--     CF_APPLY                : constant := 512;
--     CF_ANSIONLY             : constant := 1024;
--     CF_SCRIPTSONLY          : constant := 1024;
--     CF_NOVECTORFONTS        : constant := 2048;
--     CF_NOOEMFONTS           : constant := 2048;
--     CF_NOSIMULATIONS        : constant := 4096;
   CF_LIMITSIZE            : constant := 8192;
--     CF_FIXEDPITCHONLY       : constant := 16384;
--     CF_WYSIWYG              : constant := 32768;
--     CF_FORCEFONTEXIST       : constant := 65536;
--     CF_SCALABLEONLY         : constant := 131072;
--     CF_TTONLY               : constant := 262144;
--     CF_NOFACESEL            : constant := 524288;
--     CF_NOSTYLESEL           : constant := 1048576;
--     CF_NOSIZESEL            : constant := 2097152;
--     CF_SELECTSCRIPT         : constant := 4194304;
--     CF_NOSCRIPTSEL          : constant := 8388608;
--     CF_NOVERTFONTS          : constant := 16777216;

--     SIMULATED_FONTTYPE : constant := 32768;
--     PRINTER_FONTTYPE   : constant := 16384;
--     SCREEN_FONTTYPE    : constant := 8192;
--     BOLD_FONTTYPE      : constant := 256;
--     ITALIC_FONTTYPE    : constant := 512;
--     REGULAR_FONTTYPE   : constant := 1024;

   type Pointer_To_DEVMODE is access all DEVMODE;

   type TPRINTDLG is
      record
         lStructSize         : Interfaces.C.long      := 66;
         hwndOwner       : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         hDevMode            : Pointer_To_DEVMODE     := null;
         hDevNames           : Integer                := 0;
         hDC             : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         flags               : Interfaces.C.unsigned  := 0;
         nFromPage           : Interfaces.C.short     := 0;
         nToPage             : Interfaces.C.short     := 0;
         nMinPage            : Interfaces.C.short     := 0;
         nMaxPage            : Interfaces.C.short     := 0;
         nCopies             : Interfaces.C.short     := 0;
         hInstance           : Interfaces.C.long      := 0;
         lCustData           : Interfaces.C.long      := 0;
         lpfnPrintHook       : Interfaces.C.long      := 0;
         lpfnSetupHook       : Interfaces.C.long      := 0;
         lpPrintTemplateName : Interfaces.C.long      := 0;
         lpSetupTemplateName : Interfaces.C.long      := 0;
         hPrintTemplate      : Interfaces.C.long      := 0;
         hSetupTemplate      : Interfaces.C.long      := 0;
      end record;

   PD_RETURNDC                   : constant := 256;
--     PD_RETURNIC                   : constant := 512;
   PD_RETURNDEFAULT              : constant := 1024;

   type BROWSEINFO is
      record
         hwndOwner      : GWindows.Types.Handle := GWindows.Types.Null_Handle;
         pidlRoot       : Interfaces.C.long := 0;
         pszDisplayName : GNATCOM.Types.LPSTR;
         lpszTitle      : GNATCOM.Types.LPSTR;
         ulFlags        : Interfaces.C.unsigned := 0;
         lpfn           : Interfaces.C.long := 0;
         lParam         : Interfaces.C.long := 0;
         iImage         : Interfaces.C.int;
      end record;

   function SHBrowseForFolder
     (lpbi : BROWSEINFO)
     return Interfaces.C.long;
   pragma Import (StdCall, SHBrowseForFolder, "SHBrowseForFolder");

   procedure  SHGetPathFromIDList
     (pidl    : Interfaces.C.long;
      pszpath : Interfaces.C.char_array);
   pragma Import (StdCall, SHGetPathFromIDList, "SHGetPathFromIDList");

   -------------------------------------------------------------------------
   --  Package Body
   -------------------------------------------------------------------------

   ------------------
   -- Choose_Color --
   ------------------

   procedure Choose_Color
     (Window  : in     GWindows.Base.Base_Window_Type'Class;
      Color   : in out GWindows.Colors.Color_Type;
      Success :    out Boolean;
      Custom  : in     Pointer_To_Color_Array          :=
        Global_Color_Array'Access)
   is
      CC     : TCHOOSECOLOR;
      Result : Integer;
   begin
      CC.hwndOwner    := GWindows.Base.Handle (Window);
      CC.lpCustColors := Custom;
      CC.rgbResult    := Color;

      Result := ChooseColor (CC);

      Success := Result /= 0;

      if Success then
         Color := CC.rgbResult;
      end if;
   end Choose_Color;

   Hook : OFNHookProc;

   function Stdcallhook (hWnd   : GWindows.Types.Handle;
                         uiMsg  : Interfaces.C.unsigned;
                         wParam : GWindows.Types.Wparam;
                         lParam : GWindows.Types.Lparam)
                         return Interfaces.C.long;
   pragma Convention (Stdcall, Stdcallhook);

   function Stdcallhook (hWnd   : GWindows.Types.Handle;
                         uiMsg  : Interfaces.C.unsigned;
                         wParam : GWindows.Types.Wparam;
                         lParam : GWindows.Types.Lparam)
                         return Interfaces.C.long is
   begin
      return Hook (hWnd, uiMsg, wParam, lParam);
   end Stdcallhook;

   function MAKEINTRESOURCE (Id : Integer) return LPSTR is
      type Int is range -(2 ** (Standard'Address_Size - 1)) ..
                         (2 ** (Standard'Address_Size - 1) - 1);
      function To_LPSTR is new Ada.Unchecked_Conversion (Int, LPSTR);
   begin
      return To_LPSTR (Int (Id));
   end MAKEINTRESOURCE;

   ---------------
   -- Open_File --
   ---------------

   procedure Open_File
     (Window            : in     GWindows.Base.Base_Window_Type'Class;
      Dialog_Title      : in     GString;
      File_Name         : in out GString_Unbounded;
      Filters           : in     Filter_Array;
      Default_Extension : in     GString;
      File_Title        :    out GString_Unbounded;
      Success           :    out Boolean;
      TemplateId        : in     Integer := 0;                      --  * AnSp
      UserProc          : in     OFNHookProc := null)               --  * AnSp
   is
      use Interfaces.C;
      use GWindows.GStrings;
      use GWindows.GStrings.Unbounded;

      OFN         : OPENFILENAME;
      Max_Size    : constant := 5120;
      C_File_Name : char_array (0 .. Max_Size) :=
        (others => nul);
      C_File_Title  : char_array (0 .. Max_Size) :=
        (others => nul);
      Result      : Integer;
      C_Default_Extension : char_array :=
        To_C (GWindows.GStrings.To_String (Default_Extension));
      C_Dialog_Title : char_array :=
        To_C (GWindows.GStrings.To_String (Dialog_Title));
      Filter_List : GString_Unbounded;
   begin
      for N in Filters'Range loop
         Filter_List := Filter_List &
           To_GString_Unbounded (To_GString_From_Unbounded
                                   (Filters (N).Name) &
                                 GCharacter'Val (0) &
                                 To_GString_From_Unbounded
                                   (Filters (N).Filter) &
                                 GCharacter'Val (0));
      end loop;

      Filter_List := Filter_List & To_GString_Unbounded (GCharacter'Val (0) &
                                                         GCharacter'Val (0));

      if File_Name /= "" then
         declare
            SFile_Name  : constant char_array :=
              To_C (To_String (To_GString_From_Unbounded (File_Name)));
         begin
            C_File_Name (SFile_Name'Range) := SFile_Name;
         end;
      end if;

      declare
         C_Filter_List : char_array :=
           To_C (To_String (To_GString_From_Unbounded (Filter_List)));
      begin
         OFN.hwndOwner := GWindows.Base.Handle (Window);
         OFN.lpstrFile := C_File_Name (0)'Unchecked_Access;
         OFN.nMaxFile := Max_Size;
         OFN.lpstrFileTitle := C_File_Title (0)'Unchecked_Access;
         OFN.nMaxFileTitle := Max_Size;
         OFN.lpstrFilter := C_Filter_List (0)'Unchecked_Access;
         OFN.nFilterIndex := 1;
         OFN.lpstrDefExt := C_Default_Extension (0)'Unchecked_Access;
         OFN.lpstrTitle := C_Dialog_Title (0)'Unchecked_Access;
         --  * AnSp: next if statements are new
         if TemplateId /= 0 or UserProc /= null then
            --  Need to set explorer style with user template or procedure
            OFN.flags := OFN.flags + Interfaces.C.long (OFN_EXPLORER);
         end if;
         if TemplateId /= 0 then
            OFN.lpTemplateName := MAKEINTRESOURCE (TemplateId);
            OFN.hInstance := GWindows.Application.hInstance;
            OFN.flags := OFN.flags + Interfaces.C.long (OFN_ENABLETEMPLATE);
         end if;
         if UserProc /= null then
            Hook := UserProc;
            OFN.lpfnHook := Stdcallhook'Access;
            OFN.flags := OFN.flags + Interfaces.C.long (OFN_ENABLEHOOK);
         end if;
         Result := GetOpenFileName (OFN);
      end;
      Success := Result /= 0;

      if Success then
         File_Name :=
           To_GString_Unbounded
           (To_GString_From_String (To_Ada (C_File_Name)));
         File_Title :=
           To_GString_Unbounded
           (To_GString_From_String (To_Ada (C_File_Title)));
      else
         File_Name := To_GString_Unbounded ("");
         File_Title := To_GString_Unbounded ("");
      end if;
   end Open_File;

   ---------------
   -- Save_File --
   ---------------

   procedure Save_File
     (Window            : in     GWindows.Base.Base_Window_Type'Class;
      Dialog_Title      : in     GString;
      File_Name         : in out GString_Unbounded;
      Filters           : in     Filter_Array;
      Default_Extension : in     GString;
      File_Title        :    out GString_Unbounded;
      Success           :    out Boolean;
      TemplateId        : in     Integer := 0;                      --  * AnSp
      UserProc          : in     OFNHookProc := null)               --  * AnSp
   is
      use Interfaces.C;
      use GWindows.GStrings;
      use GWindows.GStrings.Unbounded;

      OFN         : OPENFILENAME;
      Max_Size    : constant := 5120;
      C_File_Name : char_array (0 .. Max_Size) :=
        (others => nul);
      C_File_Title  : char_array (0 .. Max_Size) :=
        (others => nul);
      Result      : Integer;
      C_Default_Extension : char_array :=
        To_C (GWindows.GStrings.To_String (Default_Extension));
      C_Dialog_Title : char_array :=
        To_C (GWindows.GStrings.To_String (Dialog_Title));
      Filter_List : GString_Unbounded;
   begin
      for N in Filters'Range loop
         Filter_List := Filter_List &
           To_GString_Unbounded (To_GString_From_Unbounded
                                   (Filters (N).Name) &
                                 GCharacter'Val (0) &
                                 To_GString_From_Unbounded
                                   (Filters (N).Filter) &
                                 GCharacter'Val (0));
      end loop;

      Filter_List := Filter_List & To_GString_Unbounded (GCharacter'Val (0) &
                                                         GCharacter'Val (0));

      if File_Name /= "" then
         declare
            SFile_Name  : constant char_array :=
              To_C (To_String (To_GString_From_Unbounded (File_Name)));
         begin
            C_File_Name (SFile_Name'Range) := SFile_Name;
         end;
      end if;

      declare
         C_Filter_List : char_array :=
           To_C (To_String (To_GString_From_Unbounded (Filter_List)));
      begin
         OFN.hwndOwner := GWindows.Base.Handle (Window);
         OFN.lpstrFile := C_File_Name (0)'Unchecked_Access;
         OFN.nMaxFile := Max_Size;
         OFN.lpstrFileTitle := C_File_Title (0)'Unchecked_Access;
         OFN.nMaxFileTitle := Max_Size;
         OFN.lpstrFilter := C_Filter_List (0)'Unchecked_Access;
         OFN.nFilterIndex := 1;
         OFN.lpstrDefExt := C_Default_Extension (0)'Unchecked_Access;
         OFN.lpstrTitle := C_Dialog_Title (0)'Unchecked_Access;
         --  * AnSp: next if statements are new
         if TemplateId /= 0 or UserProc /= null then
            --  Need to set explorer style with user template or procedure
            OFN.flags := OFN.flags + Interfaces.C.long (OFN_EXPLORER);
         end if;
         if TemplateId /= 0 then
            OFN.lpTemplateName := MAKEINTRESOURCE (TemplateId);
            OFN.hInstance := GWindows.Application.hInstance;
            OFN.flags := OFN.flags + Interfaces.C.long (OFN_ENABLETEMPLATE);
         end if;
         if UserProc /= null then
            Hook := UserProc;
            OFN.lpfnHook := Stdcallhook'Access;
            OFN.flags := OFN.flags + Interfaces.C.long (OFN_ENABLEHOOK);
         end if;
         --  * AnSp: added code up to here
         Result := GetSaveFileName (OFN);
      end;
      Success := Result /= 0;

      if Success then
         File_Name :=
           To_GString_Unbounded
           (To_GString_From_String (To_Ada (C_File_Name)));
         File_Title :=
           To_GString_Unbounded
           (To_GString_From_String (To_Ada (C_File_Title)));
      else
         File_Name := To_GString_Unbounded ("");
         File_Title := To_GString_Unbounded ("");
      end if;
   end Save_File;

   ------------------------------
   -- Choose_Font_With_Effects --
   ------------------------------

   procedure Choose_Font_With_Effects
     (Window   : in     GWindows.Base.Base_Window_Type'Class;
      Canvas   : in     GWindows.Drawing.Canvas_Type'Class;
      Font     : in out GWindows.Drawing_Objects.Font_Type'Class;
      Color    : in out GWindows.Colors.Color_Type;
      Min_Size : in     Integer                                  := 8;
      Max_Size : in     Integer                                  := 72)
   is
      use GWindows.Base;
      use GWindows.Drawing_Objects;
      use GWindows.Drawing;
      use GWindows.Colors;
      use type Interfaces.C.unsigned_long;

      FName : aliased Face_Name_Type;
      LFont : aliased LOGFONT;

      type TCHOOSEFONT is
         record
            lStructSize            : Interfaces.C.long  := 60;
            hwndOwner              : GWindows.Types.Handle  := Handle (Window);
            hDC                    : GWindows.Types.Handle  := Handle (Canvas);
            lpLogFont              : Pointer_To_LOGFONT
              := LFont'Unchecked_Access;
            iPointSize             : Interfaces.C.int   := 0;
            flags                  : Interfaces.C.unsigned_long
              := CF_BOTH or CF_INITTOLOGFONTSTRUCT or CF_LIMITSIZE;
            rgbColors              : Color_Type         := 0;
            lCustData              : Interfaces.C.long  := 0;
            lpfnHook               : Interfaces.C.long  := 0;
            lpTemplateName         : Interfaces.C.long  := 0;
            hInstance              : Interfaces.C.long  := 0;
            lpszStyle              : Interfaces.C.long  := 0;
            nFontType              : Interfaces.C.short := 0;
            uu_MISSING_ALIGNMENT_u : Interfaces.C.short := 0;
            nSizeMin               : Integer            := Min_Size;
            nSizeMax               : Integer            := Max_Size;
         end record;

      CFont : TCHOOSEFONT;

      procedure GetObject
        (Object : GWindows.Types.Handle := Handle (Font);
         SizeOf : Integer := 60;
         Font   : in out LOGFONT);
      pragma Import (StdCall, GetObject, "GetObjectA");

      procedure ChooseFont (CF : TCHOOSEFONT := CFont);
      pragma Import (StdCall, ChooseFont, "ChooseFontA");

      function CreateFontIndirect (LF : Pointer_To_LOGFONT)
                                  return GWindows.Types.Handle;
      pragma Import (StdCall, CreateFontIndirect, "CreateFontIndirectA");
   begin

      if Color < 16#FFFFFFFF# then
         CFont.flags := CFont.flags or CF_EFFECTS;
         CFont.rgbColors := Color;
      end if;

      LFont.lfFaceName := FName'Unchecked_Access;

      GetObject (Font => LFont);

      ChooseFont;

      Delete (Font);

      Handle (Font, CreateFontIndirect (CFont.lpLogFont));

      Color := CFont.rgbColors;
   end Choose_Font_With_Effects;

   -----------------
   -- Choose_Font --
   -----------------

   procedure Choose_Font
     (Window   : in     GWindows.Base.Base_Window_Type'Class;
      Canvas   : in     GWindows.Drawing.Canvas_Type'Class;
      Font     : in out GWindows.Drawing_Objects.Font_Type'Class;
      Min_Size : in     Integer                                  := 8;
      Max_Size : in     Integer                                  := 72)
   is
      Color : GWindows.Colors.Color_Type := 16#FFFFFFFF#;
   begin
      Choose_Font_With_Effects
        (Window, Canvas, Font, Color, Min_Size, Max_Size);
   end Choose_Font;

   --------------------
   -- Choose_Printer --
   --------------------

   procedure Choose_Printer
     (Window    : in     GWindows.Base.Base_Window_Type'Class;
      Canvas    :    out GWindows.Drawing.Printer_Canvas_Type'Class;
      Settings  :    out DEVMODE;
      Flags     : in out Interfaces.C.unsigned;
      From_Page : in out Natural;
      To_Page   : in out Natural;
      Min_Page  : in     Natural;
      Max_Page  : in     Natural;
      Copies    : in out Natural;
      Success   :    out Boolean)
   is
      use Interfaces.C;

      PD : TPRINTDLG;

      function PrintDlg
        (lppd : TPRINTDLG := PD)
        return Integer;
      pragma Import (StdCall, PrintDlg, "PrintDlgA");

      procedure GlobalFree (handle : Integer);
      procedure GlobalFree (handle : Pointer_To_DEVMODE);
      pragma Import (StdCall, GlobalFree, "GlobalFree");
   begin
      PD.hwndOwner := GWindows.Base.Handle (Window);
      PD.flags     := PD_RETURNDC or Flags;
      PD.nFromPage := short (From_Page);
      PD.nToPage := short (To_Page);
      PD.nMinPage := short (Min_Page);
      PD.nMaxPage := short (Max_Page);
      PD.nCopies := short (Copies);

      Success := PrintDlg /= 0;

      if Success then
         GWindows.Drawing.Capture (Canvas, GWindows.Types.Null_Handle, PD.hDC);

         Flags := PD.flags;
         From_Page := Integer (PD.nFromPage);
         To_Page := Integer (PD.nToPage);
         Copies := Integer (PD.nCopies);

         Settings := PD.hDevMode.all;

         GlobalFree (PD.hDevNames);
         GlobalFree (PD.hDevMode);
      end if;

   end Choose_Printer;

   ----------------------------
   -- Choose_Default_Printer --
   ----------------------------

   procedure Choose_Default_Printer
     (Canvas   : out GWindows.Drawing.Printer_Canvas_Type'Class;
      Settings : out DEVMODE;
      Success  : out Boolean)
   is
      PD : TPRINTDLG;

      function PrintDlg
        (lppd : TPRINTDLG := PD)
        return Integer;
      pragma Import (StdCall, PrintDlg, "PrintDlgA");

      procedure GlobalFree (handle : Integer);
      procedure GlobalFree (handle : Pointer_To_DEVMODE);
      pragma Import (StdCall, GlobalFree, "GlobalFree");
   begin
      PD.flags     := PD_RETURNDC or PD_RETURNDEFAULT;

      Success := PrintDlg /= 0;

      if Success then
         GWindows.Drawing.Capture (Canvas, GWindows.Types.Null_Handle, PD.hDC);

         Settings := PD.hDevMode.all;

         GlobalFree (PD.hDevNames);
         GlobalFree (PD.hDevMode);
      end if;
   end Choose_Default_Printer;

   function Get_Directory
     (Window       : in GWindows.Base.Base_Window_Type'Class;
      Dialog_Title : in GString)
     return GWindows.GString
   is
      Result1 : GWindows.GString_Unbounded;
      Result2 : GWindows.GString_Unbounded;
   begin
      Get_Directory (Window, Dialog_Title, Result1, Result2);
      return GWindows.GStrings.To_GString_From_Unbounded (Result2);
   end Get_Directory;

   function Get_Directoy
     (Dialog_Title : in GString)
     return GWindows.GString
   is
      Result1 : GWindows.GString_Unbounded;
      Result2 : GWindows.GString_Unbounded;
   begin
      Get_Directory (Dialog_Title, Result1, Result2);
      return GWindows.GStrings.To_GString_From_Unbounded (Result2);
   end Get_Directoy;

   procedure Get_Directory
     (Window       : in GWindows.Base.Base_Window_Type'Class;
      Dialog_Title : in GString;
      Directory_Display_Name : out GString_Unbounded;
      Directory_Path : out GString_Unbounded)
   is
      use type Interfaces.C.long;

      Directory : Interfaces.C.char_array (1 .. 1024);
      Title     : Interfaces.C.char_array :=
        Interfaces.C.To_C (GWindows.GStrings.To_String (Dialog_Title));
      BInfo     : BROWSEINFO;
      Pidl      : Interfaces.C.long;
   begin
      BInfo.hwndOwner := GWindows.Base.Handle (Window);
      BInfo.pszDisplayName := Directory (Directory'First)'Unchecked_Access;
      BInfo.lpszTitle := Title (Title'First)'Unchecked_Access;

      Pidl :=  SHBrowseForFolder (BInfo);

      if Pidl /= 0 then
         Directory_Display_Name := GWindows.GStrings.To_GString_Unbounded
           (GWindows.GStrings.To_GString_From_String
            (Interfaces.C.To_Ada (Directory)));

         SHGetPathFromIDList (Pidl, Directory);
         Directory_Path := GWindows.GStrings.To_GString_Unbounded
           (GWindows.GStrings.To_GString_From_String
            (Interfaces.C.To_Ada (Directory)));
      else
         Directory_Display_Name := GWindows.GStrings.To_GString_Unbounded ("");
         Directory_Path := GWindows.GStrings.To_GString_Unbounded ("");
      end if;
   end Get_Directory;

   procedure Get_Directory
     (Dialog_Title : in GString;
      Directory_Display_Name : out GString_Unbounded;
      Directory_Path : out GString_Unbounded)
   is
      Temp : GWindows.Base.Base_Window_Type;
   begin
      Get_Directory (Temp,
                     Dialog_Title,
                     Directory_Display_Name,
                     Directory_Path);
   end Get_Directory;

end GWindows.Common_Dialogs;
