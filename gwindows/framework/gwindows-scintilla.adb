------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                   G W I N D O W S . S C I N T I L L A                    --
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

--  Bindings to Scintilla Edit Control for Windows

with Ada.Unchecked_Conversion;

with GWindows.GStrings;
with GWindows.Types; use GWindows.Types;

pragma Elaborate_All (GWindows.GStrings);

package body GWindows.Scintilla is

--   SCI_START                      : constant := 16#07D0#;
--   SCI_OPTIONAL_START             : constant := 16#0BB8#;
--   SCI_LEXER_START                : constant := 16#0FA0#;
   SCI_ADDTEXT                    : constant := 16#07D1#;
   SCI_ADDSTYLEDTEXT              : constant := 16#07D2#;
   SCI_INSERTTEXT                 : constant := 16#07D3#;
   SCI_CLEARALL                   : constant := 16#07D4#;
   SCI_CLEARDOCUMENTSTYLE         : constant := 16#07D5#;
   SCI_GETLENGTH                  : constant := 16#07D6#;
   SCI_GETCHARAT                  : constant := 16#07D7#;
   SCI_GETCURRENTPOS              : constant := 16#07D8#;
   SCI_GETANCHOR                  : constant := 16#07D9#;
   SCI_GETSTYLEAT                 : constant := 16#07DA#;
   SCI_REDO                       : constant := 16#07DB#;
   SCI_SETUNDOCOLLECTION          : constant := 16#07DC#;
   SCI_SELECTALL                  : constant := 16#07DD#;
   SCI_SETSAVEPOINT               : constant := 16#07DE#;
   SCI_GETSTYLEDTEXT              : constant := 16#07DF#;
   SCI_CANREDO                    : constant := 16#07E0#;
   SCI_MARKERLINEFROMHANDLE       : constant := 16#07E1#;
   SCI_MARKERDELETEHANDLE         : constant := 16#07E2#;
   SCI_GETUNDOCOLLECTION          : constant := 16#07E3#;
   SCI_GETVIEWWS                  : constant := 16#07E4#;
   SCI_SETVIEWWS                  : constant := 16#07E5#;
   SCI_POSITIONFROMPOINT          : constant := 16#07E6#;
   SCI_POSITIONFROMPOINTCLOSE     : constant := 16#07E7#;
   SCI_GOTOLINE                   : constant := 16#07E8#;
   SCI_GOTOPOS                    : constant := 16#07E9#;
   SCI_SETANCHOR                  : constant := 16#07EA#;
   SCI_GETCURLINE                 : constant := 16#07EB#;
   SCI_GETENDSTYLED               : constant := 16#07EC#;
   SCI_CONVERTEOLS                : constant := 16#07ED#;
   SCI_GETEOLMODE                 : constant := 16#07EE#;
   SCI_SETEOLMODE                 : constant := 16#07EF#;
   SCI_STARTSTYLING               : constant := 16#07F0#;
   SCI_SETSTYLING                 : constant := 16#07F1#;
   SCI_GETBUFFEREDDRAW            : constant := 16#07F2#;
   SCI_SETBUFFEREDDRAW            : constant := 16#07F3#;
   SCI_SETTABWIDTH                : constant := 16#07F4#;
   SCI_GETTABWIDTH                : constant := 16#0849#;
   SCI_SETCODEPAGE                : constant := 16#07F5#;
   SCI_SETUSEPALETTE              : constant := 16#07F7#;
   SCI_MARKERDEFINE               : constant := 16#07F8#;
   SCI_MARKERSETFORE              : constant := 16#07F9#;
   SCI_MARKERSETBACK              : constant := 16#07FA#;
   SCI_MARKERADD                  : constant := 16#07FB#;
   SCI_MARKERDELETE               : constant := 16#07FC#;
   SCI_MARKERDELETEALL            : constant := 16#07FD#;
   SCI_MARKERGET                  : constant := 16#07FE#;
   SCI_MARKERNEXT                 : constant := 16#07FF#;
   SCI_MARKERPREVIOUS             : constant := 16#0800#;
   SCI_SETMARGINTYPEN             : constant := 16#08C0#;
   SCI_GETMARGINTYPEN             : constant := 16#08C1#;
   SCI_SETMARGINWIDTHN            : constant := 16#08C2#;
   SCI_GETMARGINWIDTHN            : constant := 16#08C3#;
   SCI_SETMARGINMASKN             : constant := 16#08C4#;
   SCI_GETMARGINMASKN             : constant := 16#08C5#;
   SCI_SETMARGINSENSITIVEN        : constant := 16#08C6#;
   SCI_GETMARGINSENSITIVEN        : constant := 16#08C7#;
   SCI_STYLECLEARALL              : constant := 16#0802#;
   SCI_STYLESETFORE               : constant := 16#0803#;
   SCI_STYLESETBACK               : constant := 16#0804#;
   SCI_STYLESETBOLD               : constant := 16#0805#;
   SCI_STYLESETITALIC             : constant := 16#0806#;
   SCI_STYLESETSIZE               : constant := 16#0807#;
   SCI_STYLESETFONT               : constant := 16#0808#;
   SCI_STYLESETEOLFILLED          : constant := 16#0809#;
   SCI_STYLERESETDEFAULT          : constant := 16#080A#;
   SCI_STYLESETUNDERLINE          : constant := 16#080B#;
   SCI_STYLESETCASE               : constant := 16#080C#;
   SCI_STYLESETCHARACTERSET       : constant := 16#0812#;
   SCI_SETSELFORE                 : constant := 16#0813#;
   SCI_SETSELBACK                 : constant := 16#0814#;
   SCI_SETCARETFORE               : constant := 16#0815#;
   SCI_ASSIGNCMDKEY               : constant := 16#0816#;
   SCI_CLEARCMDKEY                : constant := 16#0817#;
   SCI_CLEARALLCMDKEYS            : constant := 16#0818#;
   SCI_SETSTYLINGEX               : constant := 16#0819#;
   SCI_STYLESETVISIBLE            : constant := 16#081A#;
   SCI_GETCARETPERIOD             : constant := 16#081B#;
   SCI_SETCARETPERIOD             : constant := 16#081C#;
   SCI_SETWORDCHARS               : constant := 16#081D#;
   SCI_BEGINUNDOACTION            : constant := 16#081E#;
   SCI_ENDUNDOACTION              : constant := 16#081F#;
   SCI_INDICSETSTYLE              : constant := 16#0820#;
   SCI_INDICGETSTYLE              : constant := 16#0821#;
   SCI_INDICSETFORE               : constant := 16#0822#;
   SCI_INDICGETFORE               : constant := 16#0823#;
   SCI_SETSTYLEBITS               : constant := 16#082A#;
   SCI_GETSTYLEBITS               : constant := 16#082B#;
   SCI_SETLINESTATE               : constant := 16#082C#;
   SCI_GETLINESTATE               : constant := 16#082D#;
   SCI_GETMAXLINESTATE            : constant := 16#082E#;
   SCI_GETCARETLINEVISIBLE        : constant := 16#082F#;
   SCI_SETCARETLINEVISIBLE        : constant := 16#0830#;
   SCI_GETCARETLINEBACK           : constant := 16#0831#;
   SCI_SETCARETLINEBACK           : constant := 16#0832#;
   SCI_STYLESETCHANGEABLE         : constant := 16#0833#;
   SCI_AUTOCSHOW                  : constant := 16#0834#;
   SCI_AUTOCCANCEL                : constant := 16#0835#;
   SCI_AUTOCACTIVE                : constant := 16#0836#;
   SCI_AUTOCPOSSTART              : constant := 16#0837#;
   SCI_AUTOCCOMPLETE              : constant := 16#0838#;
   SCI_AUTOCSTOPS                 : constant := 16#0839#;
   SCI_AUTOCSETSEPARATOR          : constant := 16#083A#;
   SCI_AUTOCGETSEPARATOR          : constant := 16#083B#;
   SCI_AUTOCSELECT                : constant := 16#083C#;
   SCI_AUTOCSETCANCELATSTART      : constant := 16#083E#;
   SCI_AUTOCGETCANCELATSTART      : constant := 16#083F#;
   SCI_AUTOCSETFILLUPS            : constant := 16#0840#;
   SCI_AUTOCSETCHOOSESINGLE       : constant := 16#0841#;
   SCI_AUTOCGETCHOOSESINGLE       : constant := 16#0842#;
   SCI_AUTOCSETIGNORECASE         : constant := 16#0843#;
   SCI_AUTOCGETIGNORECASE         : constant := 16#0844#;
   SCI_USERLISTSHOW               : constant := 16#0845#;
   SCI_AUTOCSETAUTOHIDE           : constant := 16#0846#;
   SCI_AUTOCGETAUTOHIDE           : constant := 16#0847#;
   SCI_AUTOCSETDROPRESTOFWORD     : constant := 16#08DE#;
   SCI_AUTOCGETDROPRESTOFWORD     : constant := 16#08DF#;
   SCI_SETINDENT                  : constant := 16#084A#;
   SCI_GETINDENT                  : constant := 16#084B#;
   SCI_SETUSETABS                 : constant := 16#084C#;
   SCI_GETUSETABS                 : constant := 16#084D#;
   SCI_SETLINEINDENTATION         : constant := 16#084E#;
   SCI_GETLINEINDENTATION         : constant := 16#084F#;
   SCI_GETLINEINDENTPOSITION      : constant := 16#0850#;
   SCI_GETCOLUMN                  : constant := 16#0851#;
   SCI_SETHSCROLLBAR              : constant := 16#0852#;
   SCI_GETHSCROLLBAR              : constant := 16#0853#;
   SCI_SETINDENTATIONGUIDES       : constant := 16#0854#;
   SCI_GETINDENTATIONGUIDES       : constant := 16#0855#;
   SCI_SETHIGHLIGHTGUIDE          : constant := 16#0856#;
   SCI_GETHIGHLIGHTGUIDE          : constant := 16#0857#;
   SCI_GETLINEENDPOSITION         : constant := 16#0858#;
   SCI_GETCODEPAGE                : constant := 16#0859#;
   SCI_GETCARETFORE               : constant := 16#085A#;
   SCI_GETUSEPALETTE              : constant := 16#085B#;
   SCI_GETREADONLY                : constant := 16#085C#;
   SCI_SETCURRENTPOS              : constant := 16#085D#;
   SCI_SETSELECTIONSTART          : constant := 16#085E#;
   SCI_GETSELECTIONSTART          : constant := 16#085F#;
   SCI_SETSELECTIONEND            : constant := 16#0860#;
   SCI_GETSELECTIONEND            : constant := 16#0861#;
   SCI_SETPRINTMAGNIFICATION      : constant := 16#0862#;
   SCI_GETPRINTMAGNIFICATION      : constant := 16#0863#;
   SCI_SETPRINTCOLORMODE         : constant := 16#0864#;
   SCI_GETPRINTCOLORMODE         : constant := 16#0865#;
   SCI_FINDTEXT                   : constant := 16#0866#;
   SCI_FORMATRANGE                : constant := 16#0867#;
   SCI_GETFIRSTVISIBLELINE        : constant := 16#0868#;
   SCI_GETLINE                    : constant := 16#0869#;
   SCI_GETLINECOUNT               : constant := 16#086A#;
   SCI_SETMARGINLEFT              : constant := 16#086B#;
   SCI_GETMARGINLEFT              : constant := 16#086C#;
   SCI_SETMARGINRIGHT             : constant := 16#086D#;
   SCI_GETMARGINRIGHT             : constant := 16#086E#;
   SCI_GETMODIFY                  : constant := 16#086F#;
   SCI_SETSEL                     : constant := 16#0870#;
   SCI_GETSELTEXT                 : constant := 16#0871#;
   SCI_GETTEXTRANGE               : constant := 16#0872#;
   SCI_HIDESELECTION              : constant := 16#0873#;
   SCI_POINTXFROMPOSITION         : constant := 16#0874#;
   SCI_POINTYFROMPOSITION         : constant := 16#0875#;
   SCI_LINEFROMPOSITION           : constant := 16#0876#;
   SCI_POSITIONFROMLINE           : constant := 16#0877#;
   SCI_LINESCROLL                 : constant := 16#0878#;
   SCI_SCROLLCARET                : constant := 16#0879#;
   SCI_REPLACESEL                 : constant := 16#087A#;
   SCI_SETREADONLY                : constant := 16#087B#;
--   SCI_NULL                       : constant := 16#087C#;
   SCI_CANPASTE                   : constant := 16#087D#;
   SCI_CANUNDO                    : constant := 16#087E#;
   SCI_EMPTYUNDOBUFFER            : constant := 16#087F#;
   SCI_UNDO                       : constant := 16#0880#;
   SCI_CUT                        : constant := 16#0881#;
   SCI_COPY                       : constant := 16#0882#;
   SCI_PASTE                      : constant := 16#0883#;
   SCI_CLEAR                      : constant := 16#0884#;
   SCI_SETTEXT                    : constant := 16#0885#;
   SCI_GETTEXT                    : constant := 16#0886#;
   SCI_GETTEXTLENGTH              : constant := 16#0887#;
--   SCI_GETDIRECTFUNCTION          : constant := 16#0888#;
--   SCI_GETDIRECTPOINTER           : constant := 16#0889#;
   SCI_SETOVERTYPE                : constant := 16#088A#;
   SCI_GETOVERTYPE                : constant := 16#088B#;
   SCI_SETCARETWIDTH              : constant := 16#088C#;
   SCI_GETCARETWIDTH              : constant := 16#088D#;
   SCI_SETTARGETSTART             : constant := 16#088E#;
   SCI_GETTARGETSTART             : constant := 16#088F#;
   SCI_SETTARGETEND               : constant := 16#0890#;
   SCI_GETTARGETEND               : constant := 16#0891#;
   SCI_REPLACETARGET              : constant := 16#0892#;
   SCI_REPLACETARGETRE            : constant := 16#0893#;
   SCI_SEARCHINTARGET             : constant := 16#0895#;
   SCI_SETSEARCHFLAGS             : constant := 16#0896#;
   SCI_GETSEARCHFLAGS             : constant := 16#0897#;
   SCI_CALLTIPSHOW                : constant := 16#0898#;
   SCI_CALLTIPCANCEL              : constant := 16#0899#;
   SCI_CALLTIPACTIVE              : constant := 16#089A#;
   SCI_CALLTIPPOSSTART            : constant := 16#089B#;
   SCI_CALLTIPSETHLT              : constant := 16#089C#;
   SCI_CALLTIPSETBACK             : constant := 16#089D#;
   SCI_VISIBLEFROMDOCLINE         : constant := 16#08AC#;
   SCI_DOCLINEFROMVISIBLE         : constant := 16#08AD#;
   SCI_SETFOLDLEVEL               : constant := 16#08AE#;
   SCI_GETFOLDLEVEL               : constant := 16#08AF#;
   SCI_GETLASTCHILD               : constant := 16#08B0#;
   SCI_GETFOLDPARENT              : constant := 16#08B1#;
   SCI_SHOWLINES                  : constant := 16#08B2#;
   SCI_HIDELINES                  : constant := 16#08B3#;
   SCI_GETLINEVISIBLE             : constant := 16#08B4#;
   SCI_SETFOLDEXPANDED            : constant := 16#08B5#;
   SCI_GETFOLDEXPANDED            : constant := 16#08B6#;
   SCI_TOGGLEFOLD                 : constant := 16#08B7#;
   SCI_ENSUREVISIBLE              : constant := 16#08B8#;
   SCI_SETFOLDFLAGS               : constant := 16#08B9#;
   SCI_ENSUREVISIBLEENFORCEPOLICY : constant := 16#08BA#;
   SCI_SETTABINDENTS              : constant := 16#08D4#;
   SCI_GETTABINDENTS              : constant := 16#08D5#;
   SCI_SETBACKSPACEUNINDENTS      : constant := 16#08D6#;
   SCI_GETBACKSPACEUNINDENTS      : constant := 16#08D7#;
   SCI_SETMOUSEDWELLTIME          : constant := 16#08D8#;
   SCI_GETMOUSEDWELLTIME          : constant := 16#08D9#;
   SCI_WORDSTARTPOSITION          : constant := 16#08DA#;
   SCI_WORDENDPOSITION            : constant := 16#08DB#;
   SCI_SETWRAPMODE                : constant := 16#08DC#;
   SCI_GETWRAPMODE                : constant := 16#08DD#;
   SCI_SETLAYOUTCACHE             : constant := 16#08E0#;
   SCI_GETLAYOUTCACHE             : constant := 16#08E1#;
   SCI_SETSCROLLWIDTH             : constant := 16#08E2#;
   SCI_GETSCROLLWIDTH             : constant := 16#08E3#;
   SCI_TEXTWIDTH                  : constant := 16#08E4#;
   SCI_SETENDATLASTLINE           : constant := 16#08E5#;
   SCI_GETENDATLASTLINE           : constant := 16#08E6#;
   SCI_LINEDOWN                   : constant := 16#08FC#;
   SCI_LINEDOWNEXTEND             : constant := 16#08FD#;
   SCI_LINEUP                     : constant := 16#08FE#;
   SCI_LINEUPEXTEND               : constant := 16#08FF#;
   SCI_CHARLEFT                   : constant := 16#0900#;
   SCI_CHARLEFTEXTEND             : constant := 16#0901#;
   SCI_CHARRIGHT                  : constant := 16#0902#;
   SCI_CHARRIGHTEXTEND            : constant := 16#0903#;
   SCI_WORDLEFT                   : constant := 16#0904#;
   SCI_WORDLEFTEXTEND             : constant := 16#0905#;
   SCI_WORDRIGHT                  : constant := 16#0906#;
   SCI_WORDRIGHTEXTEND            : constant := 16#0907#;
   SCI_HOME                       : constant := 16#0908#;
   SCI_HOMEEXTEND                 : constant := 16#0909#;
   SCI_LINEEND                    : constant := 16#090A#;
   SCI_LINEENDEXTEND              : constant := 16#090B#;
   SCI_DOCUMENTSTART              : constant := 16#090C#;
   SCI_DOCUMENTSTARTEXTEND        : constant := 16#090D#;
   SCI_DOCUMENTEND                : constant := 16#090E#;
   SCI_DOCUMENTENDEXTEND          : constant := 16#090F#;
   SCI_PAGEUP                     : constant := 16#0910#;
   SCI_PAGEUPEXTEND               : constant := 16#0911#;
   SCI_PAGEDOWN                   : constant := 16#0912#;
   SCI_PAGEDOWNEXTEND             : constant := 16#0913#;
   SCI_EDITTOGGLEOVERTYPE         : constant := 16#0914#;
   SCI_CANCEL                     : constant := 16#0915#;
   SCI_DELETEBACK                 : constant := 16#0916#;
   SCI_TAB                        : constant := 16#0917#;
   SCI_BACKTAB                    : constant := 16#0918#;
   SCI_NEWLINE                    : constant := 16#0919#;
   SCI_FORMFEED                   : constant := 16#091A#;
   SCI_VCHOME                     : constant := 16#091B#;
   SCI_VCHOMEEXTEND               : constant := 16#091C#;
   SCI_ZOOMIN                     : constant := 16#091D#;
   SCI_ZOOMOUT                    : constant := 16#091E#;
   SCI_DELWORDLEFT                : constant := 16#091F#;
   SCI_DELWORDRIGHT               : constant := 16#0920#;
   SCI_LINECUT                    : constant := 16#0921#;
   SCI_LINEDELETE                 : constant := 16#0922#;
   SCI_LINETRANSPOSE              : constant := 16#0923#;
   SCI_LOWERCASE                  : constant := 16#0924#;
   SCI_UPPERCASE                  : constant := 16#0925#;
   SCI_LINESCROLLDOWN             : constant := 16#0926#;
   SCI_LINESCROLLUP               : constant := 16#0927#;
   SCI_DELETEBACKNOTLINE          : constant := 16#0928#;
   SCI_MOVECARETINSIDEVIEW        : constant := 16#0961#;
   SCI_LINELENGTH                 : constant := 16#092E#;
   SCI_BRACEHIGHLIGHT             : constant := 16#092F#;
   SCI_BRACEBADLIGHT              : constant := 16#0930#;
   SCI_BRACEMATCH                 : constant := 16#0931#;
   SCI_GETVIEWEOL                 : constant := 16#0933#;
   SCI_SETVIEWEOL                 : constant := 16#0934#;
   SCI_GETDOCPOINTER              : constant := 16#0935#;
   SCI_SETDOCPOINTER              : constant := 16#0936#;
   SCI_SETMODEVENTMASK            : constant := 16#0937#;
   SCI_GETEDGECOLUMN              : constant := 16#0938#;
   SCI_SETEDGECOLUMN              : constant := 16#0939#;
   SCI_GETEDGEMODE                : constant := 16#093A#;
   SCI_SETEDGEMODE                : constant := 16#093B#;
   SCI_GETEDGECOLOR               : constant := 16#093C#;
   SCI_SETEDGECOLOR               : constant := 16#093D#;
   SCI_SEARCHANCHOR               : constant := 16#093E#;
   SCI_SEARCHNEXT                 : constant := 16#093F#;
   SCI_SEARCHPREV                 : constant := 16#0940#;
   SCI_SETCARETPOLICY             : constant := 16#0941#;
   SCI_LINESONSCREEN              : constant := 16#0942#;
   SCI_USEPOPUP                   : constant := 16#0943#;
   SCI_SELECTIONISRECTANGLE       : constant := 16#0944#;
   SCI_SETZOOM                    : constant := 16#0945#;
   SCI_GETZOOM                    : constant := 16#0946#;
   SCI_CREATEDOCUMENT             : constant := 16#0947#;
   SCI_ADDREFDOCUMENT             : constant := 16#0948#;
   SCI_RELEASEDOCUMENT            : constant := 16#0949#;
   SCI_GETMODEVENTMASK            : constant := 16#094A#;
   SCI_SETFOCUS                   : constant := 16#094C#;
   SCI_GETFOCUS                   : constant := 16#094D#;
   SCI_SETSTATUS                  : constant := 16#094E#;
   SCI_GETSTATUS                  : constant := 16#094F#;
   SCI_SETMOUSEDOWNCAPTURES       : constant := 16#0950#;
   SCI_GETMOUSEDOWNCAPTURES       : constant := 16#0951#;
   SCI_SETCURSOR                  : constant := 16#0952#;
   SCI_GETCURSOR                  : constant := 16#0953#;
   SCI_SETCONTROLCHARSYMBOL       : constant := 16#0954#;
   SCI_GETCONTROLCHARSYMBOL       : constant := 16#0955#;
   SCI_WORDPARTLEFT               : constant := 16#0956#;
   SCI_WORDPARTLEFTEXTEND         : constant := 16#0957#;
   SCI_WORDPARTRIGHT              : constant := 16#0958#;
   SCI_WORDPARTRIGHTEXTEND        : constant := 16#0959#;
   SCI_SETVISIBLEPOLICY           : constant := 16#095A#;
   SCI_DELLINELEFT                : constant := 16#095B#;
   SCI_DELLINERIGHT               : constant := 16#095C#;
   SCI_SETXOFFSET                 : constant := 16#095D#;
   SCI_GETXOFFSET                 : constant := 16#095E#;
--   SCI_GRABFOCUS                  : constant := 16#0960#;
   SCI_STARTRECORD                : constant := 16#0BB9#;
   SCI_STOPRECORD                 : constant := 16#0BBA#;
   SCI_SETLEXER                   : constant := 16#0FA1#;
   SCI_GETLEXER                   : constant := 16#0FA2#;
   SCI_COLORISE                   : constant := 16#0FA3#;
   SCI_SETPROPERTY                : constant := 16#0FA4#;
   SCI_SETKEYWORDS                : constant := 16#0FA5#;
   SCI_SETLEXERLANGUAGE           : constant := 16#0FA6#;

   procedure LoadLibrary (LibName : GString_C);
   pragma Import (StdCall, LoadLibrary, "LoadLibrary"
                  & Character_Mode_Identifier);

   WS_TABSTOP                 : constant := 65536;

   --------------------
   -- AddRefDocument --
   --------------------

   procedure AddRefDocument
     (Control : in out Scintilla_Type;
      doc : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_ADDREFDOCUMENT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := To_Lparam (doc));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end AddRefDocument;

   -------------------
   -- AddStyledText --
   -------------------

   procedure AddStyledText
     (Control : in out Scintilla_Type;
      c       : Cell_Array_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_ADDSTYLEDTEXT;
         wParam : GWindows.Types.Wparam := c'Length;
         lParam : System.Address        := c (c'First)'Address);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end AddStyledText;

   -------------
   -- AddText --
   -------------

   procedure AddText
     (Control : in out Scintilla_Type;
      text    : in     GString)
   is
      S_Text : String :=
        GWindows.GStrings.To_String (text) & Character'Val (0);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_ADDTEXT;
         wParam : GWindows.Types.Wparam := text'Length;
         lParam : System.Address        := S_Text (S_Text'First)'Address);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end AddText;

   ------------------
   -- AssignCmdKey --
   ------------------

   procedure AssignCmdKey
     (Control : in out Scintilla_Type;
      km : Key_Mod;
      msg : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_ASSIGNCMDKEY;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (km);
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (msg));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end AssignCmdKey;

   -----------------
   -- AutoCActive --
   -----------------

   function AutoCActive (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_AUTOCACTIVE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end AutoCActive;

   -----------------
   -- AutoCCancel --
   -----------------

   procedure AutoCCancel (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_AUTOCCANCEL;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end AutoCCancel;

   -------------------
   -- AutoCComplete --
   -------------------

   procedure AutoCComplete (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_AUTOCCOMPLETE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end AutoCComplete;

   ----------------------
   -- AutoCGetAutoHide --
   ----------------------

   function AutoCGetAutoHide (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_AUTOCGETAUTOHIDE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end AutoCGetAutoHide;

   ---------------------------
   -- AutoCGetCancelAtStart --
   ---------------------------

   function AutoCGetCancelAtStart
     (Control : Scintilla_Type)
      return Boolean
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_AUTOCGETCANCELATSTART;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end AutoCGetCancelAtStart;

   --------------------------
   -- AutoCGetChooseSingle --
   --------------------------

   function AutoCGetChooseSingle (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_AUTOCGETCHOOSESINGLE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end AutoCGetChooseSingle;

   ----------------------------
   -- AutoCGetDropRestOfWord --
   ----------------------------

   function AutoCGetDropRestOfWord
     (Control : Scintilla_Type)
      return Boolean
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_AUTOCGETDROPRESTOFWORD;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end AutoCGetDropRestOfWord;

   ------------------------
   -- AutoCGetIgnoreCase --
   ------------------------

   function AutoCGetIgnoreCase (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_AUTOCGETIGNORECASE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end AutoCGetIgnoreCase;

   -----------------------
   -- AutoCGetSeparator --
   -----------------------

   function AutoCGetSeparator (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_AUTOCGETSEPARATOR;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return To_Integer (SendMessage);
   end AutoCGetSeparator;

   -------------------
   -- AutoCPosStart --
   -------------------

   function AutoCPosStart (Control : Scintilla_Type) return Position is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_AUTOCPOSSTART;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return To_Integer (SendMessage);
   end AutoCPosStart;

   -----------------
   -- AutoCSelect --
   -----------------

   procedure AutoCSelect (Control : in out Scintilla_Type; text : GString) is
      S_Text : String :=
        GWindows.GStrings.To_String (text) & Character'Val (0);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_AUTOCSELECT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : System.Address        := S_Text (S_Text'First)'Address);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end AutoCSelect;

   ----------------------
   -- AutoCSetAutoHide --
   ----------------------

   procedure AutoCSetAutoHide
     (Control : in out Scintilla_Type;
      autoHide : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_AUTOCSETAUTOHIDE;
         wParam : GWindows.Types.Wparam := Boolean'Pos (autoHide);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end AutoCSetAutoHide;

   ---------------------------
   -- AutoCSetCancelAtStart --
   ---------------------------

   procedure AutoCSetCancelAtStart
     (Control : in out Scintilla_Type;
      cancel : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_AUTOCSETCANCELATSTART;
         wParam : GWindows.Types.Wparam := Boolean'Pos (cancel);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end AutoCSetCancelAtStart;

   --------------------------
   -- AutoCSetChooseSingle --
   --------------------------

   procedure AutoCSetChooseSingle
     (Control : in out Scintilla_Type;
      chooseSingle : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_AUTOCSETCHOOSESINGLE;
         wParam : GWindows.Types.Wparam := Boolean'Pos (chooseSingle);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end AutoCSetChooseSingle;

   ----------------------------
   -- AutoCSetDropRestOfWord --
   ----------------------------

   procedure AutoCSetDropRestOfWord
     (Control : in out Scintilla_Type;
      dropRestOfWord : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_AUTOCSETDROPRESTOFWORD;
         wParam : GWindows.Types.Wparam := Boolean'Pos (dropRestOfWord);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end AutoCSetDropRestOfWord;

   ---------------------
   -- AutoCSetFillUps --
   ---------------------

   procedure AutoCSetFillUps
     (Control : in out Scintilla_Type;
      characterSet : GString)
   is
      S_Text : String :=
        GWindows.GStrings.To_String (characterSet) & Character'Val (0);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_AUTOCSETFILLUPS;
         wParam : GWindows.Types.Wparam := 0;
         lParam : System.Address    := S_Text (S_Text'First)'Address);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end AutoCSetFillUps;

   ------------------------
   -- AutoCSetIgnoreCase --
   ------------------------

   procedure AutoCSetIgnoreCase
     (Control : in out Scintilla_Type;
      ignoreCase : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_AUTOCSETIGNORECASE;
         wParam : GWindows.Types.Wparam := Boolean'Pos (ignoreCase);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end AutoCSetIgnoreCase;

   -----------------------
   -- AutoCSetSeparator --
   -----------------------

   procedure AutoCSetSeparator
     (Control : in out Scintilla_Type;
      separatorCharacter : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_AUTOCSETSEPARATOR;
         wParam : GWindows.Types.Wparam := To_Wparam (separatorCharacter);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end AutoCSetSeparator;

   ---------------
   -- AutoCShow --
   ---------------

   procedure AutoCShow
     (Control    : in out Scintilla_Type;
      lenEntered : in     Integer;
      itemList   : in     GString)
   is
      S_Text : String :=
        GWindows.GStrings.To_String (itemList) & Character'Val (0);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_AUTOCSHOW;
         wParam : GWindows.Types.Wparam := To_Wparam (lenEntered);
         lParam : System.Address        := S_Text (S_Text'First)'Address);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end AutoCShow;

   ----------------
   -- AutoCStops --
   ----------------

   procedure AutoCStops
     (Control : in out Scintilla_Type;
      characterSet : GString)
   is
      S_Text : String :=
        GWindows.GStrings.To_String (characterSet) & Character'Val (0);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_AUTOCSTOPS;
         wParam : GWindows.Types.Wparam := 0;
         lParam : System.Address        := S_Text (S_Text'First)'Address);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end AutoCStops;

   -------------
   -- BackTab --
   -------------

   procedure BackTab (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_BACKTAB;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end BackTab;

   ---------------------
   -- BeginUndoAction --
   ---------------------

   procedure BeginUndoAction (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_BEGINUNDOACTION;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end BeginUndoAction;

   -------------------
   -- BraceBadLight --
   -------------------

   procedure BraceBadLight
     (Control : in out Scintilla_Type;
      pos : Position)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_BRACEBADLIGHT;
         wParam : GWindows.Types.Wparam := To_Wparam (pos);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end BraceBadLight;

   --------------------
   -- BraceHighlight --
   --------------------

   procedure BraceHighlight
     (Control : in out Scintilla_Type;
      pos1 : Position;
      ppos2 : Position)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_BRACEHIGHLIGHT;
         wParam : GWindows.Types.Wparam := To_Wparam (pos1);
         lParam : GWindows.Types.Lparam := To_Lparam (ppos2));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end BraceHighlight;

   ----------------
   -- BraceMatch --
   ----------------

   function BraceMatch
     (Control : Scintilla_Type;
      pos : Position)
      return Position
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_BRACEMATCH;
         wParam : GWindows.Types.Wparam := To_Wparam (pos);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return To_Integer (SendMessage);
   end BraceMatch;

   -------------------
   -- CallTipActive --
   -------------------

   function CallTipActive (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CALLTIPACTIVE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end CallTipActive;

   -------------------
   -- CallTipCancel --
   -------------------

   procedure CallTipCancel (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CALLTIPCANCEL;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end CallTipCancel;

   ---------------------
   -- CallTipPosStart --
   ---------------------

   function CallTipPosStart (Control : Scintilla_Type) return Position is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CALLTIPPOSSTART;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return To_Integer (SendMessage);
   end CallTipPosStart;

   --------------------
   -- CallTipSetBack --
   --------------------

   procedure CallTipSetBack
     (Control : in out Scintilla_Type;
      back : GWindows.Colors.Color_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CALLTIPSETBACK;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (back);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end CallTipSetBack;

   -------------------
   -- CallTipSetHlt --
   -------------------

   procedure CallTipSetHlt
     (Control : in out Scintilla_Type;
      start : Integer;
      endp : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CALLTIPSETHLT;
         wParam : GWindows.Types.Wparam := To_Wparam (start);
         lParam : GWindows.Types.Lparam := To_Lparam (endp));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end CallTipSetHlt;

   -----------------
   -- CallTipShow --
   -----------------

   procedure CallTipShow
     (Control : in out Scintilla_Type;
      pos : Position;
      definition : GString)
   is
      S_Text : String :=
        GWindows.GStrings.To_String (definition) & Character'Val (0);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CALLTIPSHOW;
         wParam : GWindows.Types.Wparam := To_Wparam (pos);
         lParam : System.Address        := S_Text (S_Text'First)'Address);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end CallTipShow;

   ------------
   -- Cancel --
   ------------

   procedure Cancel (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CANCEL;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Cancel;

   --------------
   -- CanPaste --
   --------------

   function CanPaste (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CANPASTE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end CanPaste;

   -------------
   -- CanRedo --
   -------------

   function CanRedo (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CANREDO;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end CanRedo;

   -------------
   -- CanUndo --
   -------------

   function CanUndo (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CANUNDO;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end CanUndo;

   --------------
   -- CharLeft --
   --------------

   procedure CharLeft (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CHARLEFT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end CharLeft;

   --------------------
   -- CharLeftExtend --
   --------------------

   procedure CharLeftExtend (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CHARLEFTEXTEND;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end CharLeftExtend;

   ---------------
   -- CharRight --
   ---------------

   procedure CharRight (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CHARRIGHT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end CharRight;

   ---------------------
   -- CharRightExtend --
   ---------------------

   procedure CharRightExtend (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CHARRIGHTEXTEND;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end CharRightExtend;

   -----------
   -- Clear --
   -----------

   procedure Clear (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CLEAR;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Clear;

   --------------
   -- ClearAll --
   --------------

   procedure ClearAll (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CLEARALL;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end ClearAll;

   ---------------------
   -- ClearAllCmdKeys --
   ---------------------

   procedure ClearAllCmdKeys (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CLEARALLCMDKEYS;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end ClearAllCmdKeys;

   -----------------
   -- ClearCmdKey --
   -----------------

   procedure ClearCmdKey (Control : in out Scintilla_Type; km : Key_Mod) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CLEARCMDKEY;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (km);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end ClearCmdKey;

   ------------------------
   -- ClearDocumentStyle --
   ------------------------

   procedure ClearDocumentStyle (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CLEARDOCUMENTSTYLE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end ClearDocumentStyle;

   --------------
   -- Colorise --
   --------------

   procedure Colorise
     (Control : in out Scintilla_Type;
      start : Position;
      endp : Position)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_COLORISE;
         wParam : GWindows.Types.Wparam := To_Wparam (start);
         lParam : GWindows.Types.Lparam := To_Lparam (endp));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Colorise;

   -----------------
   -- ConvertEOLs --
   -----------------

   procedure ConvertEOLs
     (Control : in out Scintilla_Type;
      eolMode : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CONVERTEOLS;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (eolMode);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end ConvertEOLs;

   ----------
   -- Copy --
   ----------

   procedure Copy (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_COPY;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Copy;

   ------------
   -- Create --
   ------------

   procedure Create
     (Scintilla  : in out Scintilla_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
      Styles : constant Interfaces.C.unsigned := WS_TABSTOP;
   begin
      Create_Control (Scintilla,
                      Parent,
                      "Scintilla",
                      "",
                      Left,
                      Top,
                      Width,
                      Height,
                      ID,
                      Styles,
                      Is_Dynamic => Is_Dynamic);

      Border (Scintilla);

      if Show then
         GWindows.Scintilla.Show (Scintilla);
      end if;
   end Create;

   --------------------
   -- CreateDocument --
   --------------------

   function CreateDocument (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CREATEDOCUMENT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return To_Integer (SendMessage);
   end CreateDocument;

   ---------
   -- Cut --
   ---------

   procedure Cut (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_CUT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Cut;

   ----------------
   -- DeleteBack --
   ----------------

   procedure DeleteBack (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_DELETEBACK;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end DeleteBack;

   -----------------------
   -- DeleteBackNotLine --
   -----------------------

   procedure DeleteBackNotLine (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_DELETEBACKNOTLINE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end DeleteBackNotLine;

   -----------------
   -- DelLineLeft --
   -----------------

   procedure DelLineLeft (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_DELLINELEFT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end DelLineLeft;

   ------------------
   -- DelLineRight --
   ------------------

   procedure DelLineRight (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_DELLINERIGHT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end DelLineRight;

   -----------------
   -- DelWordLeft --
   -----------------

   procedure DelWordLeft (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_DELWORDLEFT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end DelWordLeft;

   ------------------
   -- DelWordRight --
   ------------------

   procedure DelWordRight (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_DELWORDRIGHT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end DelWordRight;

   ------------------------
   -- DocLineFromVisible --
   ------------------------

   function DocLineFromVisible
     (Control : Scintilla_Type;
      lineDisplay : Integer)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_DOCLINEFROMVISIBLE;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (lineDisplay);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return To_Integer (SendMessage);
   end DocLineFromVisible;

   -----------------
   -- DocumentEnd --
   -----------------

   procedure DocumentEnd (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_DOCUMENTEND;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end DocumentEnd;

   -----------------------
   -- DocumentEndExtend --
   -----------------------

   procedure DocumentEndExtend (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_DOCUMENTENDEXTEND;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end DocumentEndExtend;

   -------------------
   -- DocumentStart --
   -------------------

   procedure DocumentStart (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_DOCUMENTSTART;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end DocumentStart;

   -------------------------
   -- DocumentStartExtend --
   -------------------------

   procedure DocumentStartExtend (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_DOCUMENTSTARTEXTEND;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end DocumentStartExtend;

   ------------------------
   -- EditToggleOvertype --
   ------------------------

   procedure EditToggleOvertype (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_EDITTOGGLEOVERTYPE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end EditToggleOvertype;

   ---------------------
   -- EmptyUndoBuffer --
   ---------------------

   procedure EmptyUndoBuffer (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_EMPTYUNDOBUFFER;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end EmptyUndoBuffer;

   -------------------
   -- EndUndoAction --
   -------------------

   procedure EndUndoAction (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_ENDUNDOACTION;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end EndUndoAction;

   -------------------
   -- EnsureVisible --
   -------------------

   procedure EnsureVisible
     (Control : in out Scintilla_Type;
      line : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_ENSUREVISIBLE;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (line);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end EnsureVisible;

   --------------------------------
   -- EnsureVisibleEnforcePolicy --
   --------------------------------

   procedure EnsureVisibleEnforcePolicy
     (Control : in out Scintilla_Type;
      line : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_ENSUREVISIBLEENFORCEPOLICY;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (line);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end EnsureVisibleEnforcePolicy;

   --------------
   -- FindText --
   --------------

   function FindText
     (Control : Scintilla_Type;
      flags   : Integer;
      ft      : Find_Text_Access)
     return Position
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_FINDTEXT;
         wParam : GWindows.Types.Wparam := To_Wparam (flags);
         lParam : Find_Text_Access      := ft)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return To_Integer (SendMessage);
   end FindText;

   -----------------
   -- FormatRange --
   -----------------

   procedure FormatRange
     (Control : in out Scintilla_Type;
      draw : Boolean;
      fr : Text_Range_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_FORMATRANGE;
         wParam : GWindows.Types.Wparam := Boolean'Pos (draw);
         lParam : Text_Range_Type       := fr);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end FormatRange;

   --------------
   -- FormFeed --
   --------------

   procedure FormFeed (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_FORMFEED;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end FormFeed;

   ---------------
   -- GetAnchor --
   ---------------

   function GetAnchor (Control : Scintilla_Type) return Position is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETANCHOR;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetAnchor;

   ---------------------------
   -- GetBackSpaceUnIndents --
   ---------------------------

   function GetBackSpaceUnIndents (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETBACKSPACEUNINDENTS;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end GetBackSpaceUnIndents;

   ---------------------
   -- GetBufferedDraw --
   ---------------------

   function GetBufferedDraw (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETBUFFEREDDRAW;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end GetBufferedDraw;

   ------------------
   -- GetCaretFore --
   ------------------

   function GetCaretFore
     (Control : Scintilla_Type)
      return GWindows.Colors.Color_Type
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETCARETFORE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Colors.Color_Type;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end GetCaretFore;

   ----------------------
   -- GetCaretLineBack --
   ----------------------

   function GetCaretLineBack
     (Control : Scintilla_Type)
      return GWindows.Colors.Color_Type
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETCARETLINEBACK;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Colors.Color_Type;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end GetCaretLineBack;

   -------------------------
   -- GetCaretLineVisible --
   -------------------------

   function GetCaretLineVisible (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETCARETLINEVISIBLE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end GetCaretLineVisible;

   --------------------
   -- GetCaretPeriod --
   --------------------

   procedure GetCaretPeriod (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETCARETPERIOD;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end GetCaretPeriod;

   -------------------
   -- GetCaretWidth --
   -------------------

   function GetCaretWidth (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETCARETWIDTH;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetCaretWidth;

   ---------------
   -- GetCharAt --
   ---------------

   function GetCharAt
     (Control : Scintilla_Type;
      pos : Position)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETCHARAT;
         wParam : GWindows.Types.Wparam := To_Wparam (pos);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetCharAt;

   -----------------
   -- GetCodePage --
   -----------------

   function GetCodePage (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETCODEPAGE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetCodePage;

   ---------------
   -- GetColumn --
   ---------------

   function GetColumn
     (Control : Scintilla_Type;
      pos : Position)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETCOLUMN;
         wParam : GWindows.Types.Wparam := To_Wparam (pos);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetColumn;

   --------------------------
   -- GetControlCharSymbol --
   --------------------------

   function GetControlCharSymbol (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETCONTROLCHARSYMBOL;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetControlCharSymbol;

   ----------------
   -- GetCurLine --
   ----------------

   procedure GetCurLine
     (Control        : in     Scintilla_Type;
      text           :    out GString;
      Caret_Position :    out Integer)
   is
      S : String (text'First .. text'Last + 1);

      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETCURLINE;
         wParam : GWindows.Types.Wparam := S'Length;
         lParam : System.Address        := S (S'First)'Address)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      Caret_Position := GWindows.Types.To_Integer (SendMessage);
      text := GWindows.GStrings.To_GString_From_String
        (S (S'First .. S'Last - 1));
   end GetCurLine;

   -------------------
   -- GetCurrentPos --
   -------------------

   function GetCurrentPos (Control : Scintilla_Type) return Position is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETCURRENTPOS;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetCurrentPos;

   ---------------
   -- GetCursor --
   ---------------

   function GetCursor (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETCURSOR;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetCursor;

   -------------------
   -- GetDocPointer --
   -------------------

   function GetDocPointer (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETDOCPOINTER;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetDocPointer;

   ------------------
   -- GetEdgeColor --
   ------------------

   function GetEdgeColor
     (Control : Scintilla_Type)
      return GWindows.Colors.Color_Type
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETEDGECOLOR;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Colors.Color_Type;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end GetEdgeColor;

   -------------------
   -- GetEdgeColumn --
   -------------------

   function GetEdgeColumn (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETEDGECOLUMN;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetEdgeColumn;

   -----------------
   -- GetEdgeMode --
   -----------------

   function GetEdgeMode (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETEDGEMODE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetEdgeMode;

   ----------------------
   -- GetEndAtLastLine --
   ----------------------

   function GetEndAtLastLine (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETENDATLASTLINE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetEndAtLastLine;

   ------------------
   -- GetEndStyled --
   ------------------

   function GetEndStyled (Control : Scintilla_Type) return Position is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETENDSTYLED;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetEndStyled;

   ----------------
   -- GetEOLMode --
   ----------------

   function GetEOLMode (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETEOLMODE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetEOLMode;

   -------------------------
   -- GetFirstVisibleLine --
   -------------------------

   function GetFirstVisibleLine (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETFIRSTVISIBLELINE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetFirstVisibleLine;

   --------------
   -- GetFocus --
   --------------

   function GetFocus (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETFOCUS;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end GetFocus;

   ---------------------
   -- GetFoldExpanded --
   ---------------------

   function GetFoldExpanded
     (Control : Scintilla_Type;
      line : Integer)
      return Boolean
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETFOLDEXPANDED;
         wParam : GWindows.Types.Wparam := To_Wparam (line);
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end GetFoldExpanded;

   ------------------
   -- GetFoldLevel --
   ------------------

   function GetFoldLevel
     (Control : Scintilla_Type;
      line : Integer)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETFOLDLEVEL;
         wParam : GWindows.Types.Wparam := To_Wparam (line);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetFoldLevel;

   -------------------
   -- GetFoldParent --
   -------------------

   function GetFoldParent
     (Control : Scintilla_Type;
      line : Integer)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETFOLDPARENT;
         wParam : GWindows.Types.Wparam := To_Wparam (line);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetFoldParent;

   -----------------------
   -- GetHighlightGuide --
   -----------------------

   function GetHighlightGuide (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETHIGHLIGHTGUIDE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetHighlightGuide;

   -------------------
   -- GetHScrollBar --
   -------------------

   function GetHScrollBar (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETHSCROLLBAR;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end GetHScrollBar;

   ---------------
   -- GetIndent --
   ---------------

   function GetIndent (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETINDENT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetIndent;

   --------------------------
   -- GetIndentationGuides --
   --------------------------

   function GetIndentationGuides (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETINDENTATIONGUIDES;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end GetIndentationGuides;

   ------------------
   -- GetLastChild --
   ------------------

   function GetLastChild
     (Control : Scintilla_Type;
      line : Integer;
      level : Integer)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETLASTCHILD;
         wParam : GWindows.Types.Wparam := To_Wparam (line);
         lParam : GWindows.Types.Lparam := To_Lparam (level))
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetLastChild;

   --------------------
   -- GetLayoutCache --
   --------------------

   function GetLayoutCache (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETLAYOUTCACHE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetLayoutCache;

   ---------------
   -- GetLength --
   ---------------

   function GetLength (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETLENGTH;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetLength;

   --------------
   -- GetLexer --
   --------------

   function GetLexer (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETLEXER;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetLexer;

   -------------
   -- GetLine --
   -------------

   function GetLine
     (Control : in     Scintilla_Type;
      line    : in     Integer)
     return GString
   is
      Length : Integer := LineLength (Control, line);
      S      : String (1 .. Length + 1);

      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETLINE;
         wParam : GWindows.Types.Wparam := To_Wparam (line);
         lParam : System.Address        := S (S'First)'Address)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      Length := GWindows.Types.To_Integer (SendMessage);
      return GWindows.GStrings.To_GString_From_String
        (S (S'First .. S'Last - 1));
   end GetLine;

   ------------------
   -- GetLineCount --
   ------------------

   function GetLineCount (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETLINECOUNT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetLineCount;

   ------------------------
   -- GetLineEndPosition --
   ------------------------

   function GetLineEndPosition
     (Control : Scintilla_Type;
      line : Integer)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETLINEENDPOSITION;
         wParam : GWindows.Types.Wparam := To_Wparam (line);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetLineEndPosition;

   ------------------------
   -- GetLineIndentation --
   ------------------------

   function GetLineIndentation
     (Control : Scintilla_Type;
      line : Integer)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETLINEINDENTATION;
         wParam : GWindows.Types.Wparam := To_Wparam (line);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetLineIndentation;

   ---------------------------
   -- GetLineIndentPosition --
   ---------------------------

   function GetLineIndentPosition
     (Control : Scintilla_Type;
      line : Integer)
      return Position
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETLINEINDENTPOSITION;
         wParam : GWindows.Types.Wparam := To_Wparam (line);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetLineIndentPosition;

   ------------------
   -- GetLineState --
   ------------------

   function GetLineState
     (Control : Scintilla_Type;
      line : Integer)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETLINESTATE;
         wParam : GWindows.Types.Wparam := To_Wparam (line);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetLineState;

   --------------------
   -- GetLineVisible --
   --------------------

   function GetLineVisible
     (Control : Scintilla_Type;
      line : Integer)
      return Boolean
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETLINEVISIBLE;
         wParam : GWindows.Types.Wparam := To_Wparam (line);
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end GetLineVisible;

   -------------------
   -- GetMarginLeft --
   -------------------

   function GetMarginLeft (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETMARGINLEFT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetMarginLeft;

   --------------------
   -- GetMarginMaskN --
   --------------------

   function GetMarginMaskN
     (Control : Scintilla_Type;
      margin : Integer)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETMARGINMASKN;
         wParam : GWindows.Types.Wparam := To_Wparam (margin);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetMarginMaskN;

   --------------------
   -- GetMarginRight --
   --------------------

   function GetMarginRight (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETMARGINRIGHT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetMarginRight;

   -------------------------
   -- GetMarginSensitiveN --
   -------------------------

   function GetMarginSensitiveN
     (Control : Scintilla_Type;
      margin : Integer)
      return Boolean
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETMARGINSENSITIVEN;
         wParam : GWindows.Types.Wparam := To_Wparam (margin);
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end GetMarginSensitiveN;

   --------------------
   -- GetMarginTypeN --
   --------------------

   function GetMarginTypeN
     (Control : Scintilla_Type;
      margin : Integer)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETMARGINTYPEN;
         wParam : GWindows.Types.Wparam := To_Wparam (margin);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetMarginTypeN;

   ---------------------
   -- GetMarginWidthN --
   ---------------------

   function GetMarginWidthN
     (Control : Scintilla_Type;
      margin : Integer)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETMARGINWIDTHN;
         wParam : GWindows.Types.Wparam := To_Wparam (margin);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetMarginWidthN;

   ---------------------
   -- GetMaxLineState --
   ---------------------

   function GetMaxLineState (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETMAXLINESTATE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetMaxLineState;

   ---------------------
   -- GetModEventMask --
   ---------------------

   function GetModEventMask (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETMODEVENTMASK;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetModEventMask;

   ---------------
   -- GetModify --
   ---------------

   function GetModify (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETMODIFY;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end GetModify;

   --------------------------
   -- GetMouseDownCaptures --
   --------------------------

   function GetMouseDownCaptures (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETMOUSEDOWNCAPTURES;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end GetMouseDownCaptures;

   -----------------------
   -- GetMouseDwellTime --
   -----------------------

   function GetMouseDwellTime (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETMOUSEDWELLTIME;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetMouseDwellTime;

   -----------------
   -- GetOvertype --
   -----------------

   function GetOvertype (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETOVERTYPE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end GetOvertype;

   -----------------------
   -- GetPrintColorMode --
   -----------------------

   function GetPrintColorMode (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETPRINTCOLORMODE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetPrintColorMode;

   ---------------------------
   -- GetPrintMagnification --
   ---------------------------

   function GetPrintMagnification
     (Control : Scintilla_Type)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETPRINTMAGNIFICATION;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetPrintMagnification;

   -----------------
   -- GetReadOnly --
   -----------------

   function GetReadOnly (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETREADONLY;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end GetReadOnly;

   --------------------
   -- GetScrollWidth --
   --------------------

   function GetScrollWidth (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETSCROLLWIDTH;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetScrollWidth;

   --------------------
   -- GetSearchFlags --
   --------------------

   function GetSearchFlags (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETSEARCHFLAGS;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetSearchFlags;

   ---------------------
   -- GetSelectionEnd --
   ---------------------

   function GetSelectionEnd (Control : Scintilla_Type) return Position is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETSELECTIONEND;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetSelectionEnd;

   -----------------------
   -- GetSelectionStart --
   -----------------------

   function GetSelectionStart (Control : Scintilla_Type) return Position is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETSELECTIONSTART;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetSelectionStart;

   ----------------
   -- GetSelText --
   ----------------

   procedure GetSelText
     (Control : in     Scintilla_Type;
      text    :    out GString;
      length  :    out Integer)
   is
      S : String (text'First .. text'Last + 1);

      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETSELTEXT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : System.Address        := S (S'First)'Address)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      length := GWindows.Types.To_Integer (SendMessage);
      text := GWindows.GStrings.To_GString_From_String
        (S (S'First .. S'Last - 1));
   end GetSelText;

   ---------------
   -- GetStatus --
   ---------------

   function GetStatus (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETSTATUS;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetStatus;

   ----------------
   -- GetStyleAt --
   ----------------

   function GetStyleAt
     (Control : Scintilla_Type;
      pos : Position)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETSTYLEAT;
         wParam : GWindows.Types.Wparam := To_Wparam (pos);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetStyleAt;

   ------------------
   -- GetStyleBits --
   ------------------

   function GetStyleBits (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETSTYLEBITS;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetStyleBits;

   -------------------
   -- GetStyledText --
   -------------------

   function GetStyledText
     (Control : Scintilla_Type;
      tr : Text_Range_Type)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETSTYLEDTEXT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : Text_Range_Type       := tr)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetStyledText;

   -------------------
   -- GetTabIndents --
   -------------------

   function GetTabIndents (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETTABINDENTS;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end GetTabIndents;

   -----------------
   -- GetTabWidth --
   -----------------

   function GetTabWidth (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETTABWIDTH;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetTabWidth;

   ------------------
   -- GetTargetEnd --
   ------------------

   function GetTargetEnd (Control : Scintilla_Type) return Position is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETTARGETEND;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetTargetEnd;

   --------------------
   -- GetTargetStart --
   --------------------

   function GetTargetStart (Control : Scintilla_Type) return Position is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETTARGETSTART;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetTargetStart;

   -------------
   -- GetText --
   -------------

   procedure GetText
     (Control : in     Scintilla_Type;
      text    :    out GString;
      length  :    out Integer)
   is
      S : String (text'First .. text'Last + 1);

      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETTEXT;
         wParam : GWindows.Types.Wparam := S'Length;
         lParam : System.Address        := S (S'First)'Address)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      length := GWindows.Types.To_Integer (SendMessage);
      text := GWindows.GStrings.To_GString_From_String
        (S (S'First .. S'Last - 1));
   end GetText;

   -------------------
   -- GetTextLength --
   -------------------

   function GetTextLength (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETTEXTLENGTH;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetTextLength;

   ------------------
   -- GetTextRange --
   ------------------

   function GetTextRange
     (Control : Scintilla_Type;
      Min     : Integer;
      Max     : Integer)
     return GString
   is
      Buffer : String (1 .. Max - Min);
      TR     : constant Text_Range_Type :=
        (Min, Max, Buffer (Buffer'First)'Address);
      Length : Integer;
      pragma Warnings (Off, Length);
   begin
      Length := GetTextRange (Control, TR);
      return GWindows.GStrings.To_GString_From_String (Buffer);
   end GetTextRange;

   function GetTextRange
     (Control : Scintilla_Type;
      tr : Text_Range_Type)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETTEXTRANGE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : Text_Range_Type   := tr)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetTextRange;

   -----------------------
   -- GetUndoCollection --
   -----------------------

   function GetUndoCollection (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETUNDOCOLLECTION;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end GetUndoCollection;

   -------------------
   -- GetUsePalette --
   -------------------

   function GetUsePalette (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETUSEPALETTE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end GetUsePalette;

   ----------------
   -- GetUseTabs --
   ----------------

   function GetUseTabs (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETUSETABS;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end GetUseTabs;

   ----------------
   -- GetViewEOL --
   ----------------

   function GetViewEOL (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETVIEWEOL;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end GetViewEOL;

   ---------------
   -- GetViewWS --
   ---------------

   function GetViewWS (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETVIEWWS;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetViewWS;

   -----------------
   -- GetWrapMode --
   -----------------

   function GetWrapMode (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETWRAPMODE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetWrapMode;

   ----------------
   -- GetXOffset --
   ----------------

   function GetXOffset (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETXOFFSET;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetXOffset;

   -------------
   -- GetZoom --
   -------------

   function GetZoom (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GETZOOM;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end GetZoom;

   --------------
   -- GotoLine --
   --------------

   procedure GotoLine
     (Control : in out Scintilla_Type;
      line : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GOTOLINE;
         wParam : GWindows.Types.Wparam := To_Wparam (line);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end GotoLine;

   -------------
   -- GotoPos --
   -------------

   procedure GotoPos (Control : in out Scintilla_Type; pos : Position) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_GOTOPOS;
         wParam : GWindows.Types.Wparam := To_Wparam (pos);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end GotoPos;

   ---------------
   -- HideLines --
   ---------------

   procedure HideLines
     (Control : in out Scintilla_Type;
      lineStart : Integer;
      lineEnd : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_HIDELINES;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (lineStart);
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (lineEnd));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end HideLines;

   -------------------
   -- HideSelection --
   -------------------

   procedure HideSelection
     (Control : in out Scintilla_Type;
      normal : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_HIDESELECTION;
         wParam : GWindows.Types.Wparam := Boolean'Pos (normal);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end HideSelection;

   ----------
   -- Home --
   ----------

   procedure Home (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_HOME;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Home;

   ----------------
   -- HomeExtend --
   ----------------

   procedure HomeExtend (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_HOMEEXTEND;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end HomeExtend;

   ------------------
   -- IndicGetFore --
   ------------------

   function IndicGetFore
     (Control : Scintilla_Type;
      indic : Integer)
      return GWindows.Colors.Color_Type
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_INDICGETFORE;
         wParam : GWindows.Types.Wparam := To_Wparam (indic);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Colors.Color_Type;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;

   end IndicGetFore;

   -------------------
   -- IndicGetStyle --
   -------------------

   procedure IndicGetStyle
     (Control : in out Scintilla_Type;
      indic : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_INDICGETSTYLE;
         wParam : GWindows.Types.Wparam := To_Wparam (indic);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end IndicGetStyle;

   ------------------
   -- IndicSetFore --
   ------------------

   procedure IndicSetFore
     (Control : in out Scintilla_Type;
      indic   : in     Integer;
      fore    : in     GWindows.Colors.Color_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_INDICSETFORE;
         wParam : GWindows.Types.Wparam := To_Wparam (indic);
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (fore));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end IndicSetFore;

   -------------------
   -- IndicSetStyle --
   -------------------

   procedure IndicSetStyle
     (Control : in out Scintilla_Type;
      indic : Integer;
      style : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_INDICSETSTYLE;
         wParam : GWindows.Types.Wparam := To_Wparam (indic);
         lParam : GWindows.Types.Lparam := To_Lparam (style));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end IndicSetStyle;

   ----------------
   -- InsertText --
   ----------------

   procedure InsertText
     (Control : in out Scintilla_Type;
      pos : Position;
      text : GString)
   is
      S : String :=
        GWindows.GStrings.To_String (text) & Character'Val (0);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_INSERTTEXT;
         wParam : GWindows.Types.Wparam := To_Wparam (pos);
         lParam : System.Address        := S (S'First)'Address);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end InsertText;

   -------------
   -- LineCut --
   -------------

   procedure LineCut (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_LINECUT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end LineCut;

   ----------------
   -- LineDelete --
   ----------------

   procedure LineDelete (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_LINEDELETE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end LineDelete;

   --------------
   -- LineDown --
   --------------

   procedure LineDown (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_LINEDOWN;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end LineDown;

   --------------------
   -- LineDownExtend --
   --------------------

   procedure LineDownExtend (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_LINEDOWNEXTEND;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end LineDownExtend;

   -------------
   -- LineEnd --
   -------------

   procedure LineEnd (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_LINEEND;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end LineEnd;

   -------------------
   -- LineEndExtend --
   -------------------

   procedure LineEndExtend (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_LINEENDEXTEND;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end LineEndExtend;

   ----------------------
   -- LineFromPosition --
   ----------------------

   function LineFromPosition
     (Control : Scintilla_Type;
      pos : Position)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_LINEFROMPOSITION;
         wParam : GWindows.Types.Wparam := To_Wparam (pos);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end LineFromPosition;

   ----------------
   -- LineLength --
   ----------------

   function LineLength
     (Control : Scintilla_Type;
      line : Integer)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_LINELENGTH;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (line);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end LineLength;

   ----------------
   -- LineScroll --
   ----------------

   procedure LineScroll
     (Control : in out Scintilla_Type;
      columns : Integer;
      lines : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_LINESCROLL;
         wParam : GWindows.Types.Wparam := To_Wparam (columns);
         lParam : GWindows.Types.Lparam := To_Lparam (lines));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end LineScroll;

   --------------------
   -- LineScrollDown --
   --------------------

   procedure LineScrollDown (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_LINESCROLLDOWN;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end LineScrollDown;

   ------------------
   -- LineScrollUp --
   ------------------

   procedure LineScrollUp (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_LINESCROLLUP;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end LineScrollUp;

   -------------------
   -- LinesOnScreen --
   -------------------

   function LinesOnScreen (Control : Scintilla_Type) return Integer is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_LINESONSCREEN;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end LinesOnScreen;

   -------------------
   -- LineTranspose --
   -------------------

   procedure LineTranspose (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_LINETRANSPOSE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end LineTranspose;

   ------------
   -- LineUp --
   ------------

   procedure LineUp (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_LINEUP;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end LineUp;

   ------------------
   -- LineUpExtend --
   ------------------

   procedure LineUpExtend (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_LINEUPEXTEND;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end LineUpExtend;

   ---------------
   -- LowerCase --
   ---------------

   procedure LowerCase (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_LOWERCASE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end LowerCase;

   ---------------
   -- MarkerAdd --
   ---------------

   function MarkerAdd
     (Control : Scintilla_Type;
      line : Integer;
      markerNumber : Integer)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_MARKERADD;
         wParam : GWindows.Types.Wparam := To_Wparam (line);
         lParam : GWindows.Types.Lparam := To_Lparam (markerNumber))
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end MarkerAdd;

   ------------------
   -- MarkerDefine --
   ------------------

   procedure MarkerDefine
     (Control : in out Scintilla_Type;
      markerNumber : in Integer;
      markerSymbol : in Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_MARKERDEFINE;
         wParam : GWindows.Types.Wparam := To_Wparam (markerNumber);
         lParam : GWindows.Types.Lparam := To_Lparam (markerSymbol));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end MarkerDefine;

   ------------------
   -- MarkerDelete --
   ------------------

   procedure MarkerDelete
     (Control : in out Scintilla_Type;
      line : Integer;
      markerNumber : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_MARKERDELETE;
         wParam : GWindows.Types.Wparam := To_Wparam (line);
         lParam : GWindows.Types.Lparam := To_Lparam (markerNumber));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end MarkerDelete;

   ---------------------
   -- MarkerDeleteAll --
   ---------------------

   procedure MarkerDeleteAll
     (Control : in out Scintilla_Type;
      markerNumber : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_MARKERDELETEALL;
         wParam : GWindows.Types.Wparam := To_Wparam (markerNumber);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end MarkerDeleteAll;

   ------------------------
   -- MarkerDeleteHandle --
   ------------------------

   procedure MarkerDeleteHandle
     (Control : in out Scintilla_Type;
      mhandle : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_MARKERDELETEHANDLE;
         wParam : GWindows.Types.Wparam := To_Wparam (mhandle);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end MarkerDeleteHandle;

   ---------------
   -- MarkerGet --
   ---------------

   function MarkerGet
     (Control : Scintilla_Type;
      line : Integer)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_MARKERGET;
         wParam : GWindows.Types.Wparam := To_Wparam (line);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end MarkerGet;

   --------------------------
   -- MarkerLineFromHandle --
   --------------------------

   function MarkerLineFromHandle
     (Control : Scintilla_Type;
      mhandle : Integer)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_MARKERLINEFROMHANDLE;
         wParam : GWindows.Types.Wparam := To_Wparam (mhandle);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end MarkerLineFromHandle;

   ----------------
   -- MarkerNext --
   ----------------

   function MarkerNext
     (Control : Scintilla_Type;
      lineStart : Integer;
      markerMask : Integer)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_MARKERNEXT;
         wParam : GWindows.Types.Wparam := To_Wparam (lineStart);
         lParam : GWindows.Types.Lparam := To_Lparam (markerMask))
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end MarkerNext;

   --------------------
   -- MarkerPrevious --
   --------------------

   function MarkerPrevious
     (Control : Scintilla_Type;
      lineStart : Integer;
      markerMask : Integer)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_MARKERPREVIOUS;
         wParam : GWindows.Types.Wparam := To_Wparam (lineStart);
         lParam : GWindows.Types.Lparam := To_Lparam (markerMask))
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end MarkerPrevious;

   -------------------
   -- MarkerSetBack --
   -------------------

   procedure MarkerSetBack
     (Control      : in out Scintilla_Type;
      markerNumber : in     Integer;
      back         : in     GWindows.Colors.Color_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_MARKERSETBACK;
         wParam : GWindows.Types.Wparam := To_Wparam (markerNumber);
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (back));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end MarkerSetBack;

   -------------------
   -- MarkerSetFore --
   -------------------

   procedure MarkerSetFore
     (Control      : in out Scintilla_Type;
      markerNumber : in     Integer;
      fore         : in     GWindows.Colors.Color_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_MARKERSETFORE;
         wParam : GWindows.Types.Wparam := To_Wparam (markerNumber);
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (fore));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end MarkerSetFore;

   -------------------------
   -- MoveCaretInsideView --
   -------------------------

   procedure MoveCaretInsideView (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_MOVECARETINSIDEVIEW;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end MoveCaretInsideView;

   -------------
   -- NewLine --
   -------------

   procedure NewLine (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_NEWLINE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end NewLine;

   ----------------
   -- On_Command --
   ----------------

   procedure On_Command
     (Window  : in out Scintilla_Type;
      Code    : in     Integer;
      ID      : in     Integer;
      Control : in     GWindows.Base.Pointer_To_Base_Window_Class)
   is
      SCEN_CHANGE                    : constant := 16#0300#;
      SCEN_SETFOCUS                  : constant := 16#0200#;
      SCEN_KILLFOCUS                 : constant := 16#0100#;

      pragma Warnings (Off, ID);
      pragma Warnings (Off, Control);
   begin
      case Code is
         when SCEN_SETFOCUS =>
            On_Focus (Scintilla_Type'Class (Window));
         when SCEN_KILLFOCUS =>
            On_Lost_Focus (Scintilla_Type'Class (Window));
         when SCEN_CHANGE =>
            On_Change (Scintilla_Type'Class (Window));
         when others =>
            null;
      end case;
   end On_Command;

   ---------------
   -- On_Notify --
   ---------------

   procedure On_Notify
     (Window       : in out Scintilla_Type;
      Message      : in     GWindows.Base.Pointer_To_Notification;
      Control      : in     GWindows.Base.Pointer_To_Base_Window_Class;
      Return_Value : in out GWindows.Types.Lresult)
   is
      pragma Warnings (Off, Control);
      pragma Warnings (Off, Return_Value);

      type STRUCT_SCNOTIFICATION is
         record
            Handle           : GWindows.Types.Handle;
            ID               : Integer;
            Code             : Integer;
            Pos              : Position;
            ch               : Integer;
            modifiers        : Integer;
            modificationType : Integer;
            text             : System.Address;
            length           : Integer;
            linesAdded       : Integer;
            message          : Integer;
            wParam           : Integer;
            lParam           : Integer;
            line             : Integer;
            foldLevelNow     : Integer;
            foldLevelPrev    : Integer;
            margin           : Integer;
            listType         : Integer;
            x                : Integer;
            y                : Integer;
         end record;
      pragma Convention (C,  STRUCT_SCNOTIFICATION);
      type PSTRUCT_SCNOTIFICATION is access all STRUCT_SCNOTIFICATION;

      function To_PSTRUCT_SCNOTIFICATION is new
        Ada.Unchecked_Conversion (GWindows.Base.Pointer_To_Notification,
                                  PSTRUCT_SCNOTIFICATION);

      NM : constant PSTRUCT_SCNOTIFICATION :=
        To_PSTRUCT_SCNOTIFICATION (Message);

      SCN_STYLENEEDED                : constant := 16#07D0#;
      SCN_CHARADDED                  : constant := 16#07D1#;
      SCN_SAVEPOINTREACHED           : constant := 16#07D2#;
      SCN_SAVEPOINTLEFT              : constant := 16#07D3#;
      SCN_MODIFYATTEMPTRO            : constant := 16#07D4#;
      SCN_DOUBLECLICK                : constant := 16#07D6#;
      SCN_UPDATEUI                   : constant := 16#07D7#;
      SCN_MODIFIED                   : constant := 16#07D8#;
      SCN_MACRORECORD                : constant := 16#07D9#;
      SCN_MARGINCLICK                : constant := 16#07DA#;
      SCN_NEEDSHOWN                  : constant := 16#07DB#;
      SCN_PAINTED                    : constant := 16#07DD#;
      SCN_USERLISTSELECTION          : constant := 16#07DE#;
      SCN_DWELLSTART                 : constant := 16#07E0#;
      SCN_DWELLEND                   : constant := 16#07E1#;
      SCN_ZOOM                       : constant := 16#07E2#;
   begin
      case NM.Code is
         when SCN_STYLENEEDED =>
            On_Style_Needed (Scintilla_Type'Class (Window),
                             NM.Pos);
         when SCN_DOUBLECLICK =>
            On_Double_Click (Scintilla_Type'Class (Window));
         when SCN_MODIFIED =>
            declare
               use type System.Address;

               type Text_Array is new String (1 .. NM.length);
               type Pointer_To_Text_Array is access all Text_Array;

               function To_PTA is new
                 Ada.Unchecked_Conversion
                 (System.Address, Pointer_To_Text_Array);
            begin
               if NM.text /= System.Null_Address then
                  declare
                     S : constant Pointer_To_Text_Array := To_PTA (NM.text);
                     T : constant GWindows.GString :=
                       GWindows.GStrings.To_GString_From_String
                       (String (S.all));
                  begin
                     On_Modified (Scintilla_Type'Class (Window),
                                  NM.Pos,
                                  NM.modificationType,
                                  T (T'First .. T'Last - 1),
                                  NM.linesAdded,
                                  NM.line,
                                  NM.foldLevelNow,
                                  NM.foldLevelPrev);
                  end;
               else
                  On_Modified (Scintilla_Type'Class (Window),
                               NM.Pos,
                               NM.modificationType,
                               "",
                               NM.linesAdded,
                               NM.line,
                               NM.foldLevelNow,
                               NM.foldLevelPrev);
               end if;
            end;
         when SCN_MACRORECORD =>
            On_Macro_Read (Scintilla_Type'Class (Window),
                           NM.message,
                           NM.wParam,
                           NM.lParam);
         when SCN_MARGINCLICK =>
            On_Margin_Click (Scintilla_Type'Class (Window),
                             NM.Pos,
                             NM.margin);
         when SCN_NEEDSHOWN =>
            On_Need_Shown (Scintilla_Type'Class (Window),
                           NM.Pos,
                           NM.length);
         when SCN_DWELLSTART =>
            On_Dwell_Start (Scintilla_Type'Class (Window),
                            NM.Pos);
         when SCN_DWELLEND =>
            On_Dwell_End (Scintilla_Type'Class (Window),
                            NM.Pos);
         when SCN_USERLISTSELECTION =>
            declare
               type Text_Array is new String (1 .. NM.length);
               type Pointer_To_Text_Array is access all Text_Array;

               function To_PTA is new
                 Ada.Unchecked_Conversion
                 (System.Address, Pointer_To_Text_Array);

               S : constant Pointer_To_Text_Array := To_PTA (NM.text);
               T : constant GWindows.GString :=
                 GWindows.GStrings.To_GString_From_String
                 (String (S.all));
            begin
               On_User_List_Selection (Scintilla_Type'Class (Window),
                                       NM.listType,
                                       T (T'First .. T'Last - 1));
            end;
         when SCN_CHARADDED =>
            On_Character_Added (Scintilla_Type'Class (Window),
                                GWindows.Windows.None,
                                GCharacter'Val (NM.ch));
         when SCN_UPDATEUI =>
            On_Update_UI (Scintilla_Type'Class (Window));
         when SCN_SAVEPOINTREACHED =>
            On_Save_Point_Reached (Scintilla_Type'Class (Window));
         when SCN_SAVEPOINTLEFT =>
            On_Save_Point_Left (Scintilla_Type'Class (Window));
         when SCN_MODIFYATTEMPTRO =>
            On_Attempt_To_Modify_Read_Only (Scintilla_Type'Class (Window));
         when SCN_PAINTED =>
            On_Painted (Scintilla_Type'Class (Window));
         when SCN_ZOOM =>
            On_Zoom (Scintilla_Type'Class (Window));
         when others =>
            null;
      end case;
   end On_Notify;

   --------------
   -- PageDown --
   --------------

   procedure PageDown (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_PAGEDOWN;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end PageDown;

   --------------------
   -- PageDownExtend --
   --------------------

   procedure PageDownExtend (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_PAGEDOWNEXTEND;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end PageDownExtend;

   ------------
   -- PageUp --
   ------------

   procedure PageUp (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_PAGEUP;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end PageUp;

   ------------------
   -- PageUpExtend --
   ------------------

   procedure PageUpExtend (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_PAGEUPEXTEND;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end PageUpExtend;

   -----------
   -- Paste --
   -----------

   procedure Paste (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_PASTE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Paste;

   ------------------------
   -- PointXFromPosition --
   ------------------------

   function PointXFromPosition
     (Control : Scintilla_Type;
      pos : Position)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_POINTXFROMPOSITION;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := To_Lparam (pos))
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end PointXFromPosition;

   ------------------------
   -- PointYFromPosition --
   ------------------------

   function PointYFromPosition
     (Control : Scintilla_Type;
      pos : Position)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_POINTYFROMPOSITION;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := To_Lparam (pos))
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end PointYFromPosition;

   ----------------------
   -- PositionFromLine --
   ----------------------

   function PositionFromLine
     (Control : Scintilla_Type;
      line : Integer)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_POSITIONFROMLINE;
         wParam : GWindows.Types.Wparam := To_Wparam (line);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end PositionFromLine;

   -----------------------
   -- PositionFromPoint --
   -----------------------

   function PositionFromPoint
     (Control : Scintilla_Type;
      x : Integer;
      y : Integer)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_POSITIONFROMPOINT;
         wParam : GWindows.Types.Wparam := To_Wparam (x);
         lParam : GWindows.Types.Lparam := To_Lparam (y))
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end PositionFromPoint;

   ----------------------------
   -- PositionFromPointClose --
   ----------------------------

   function PositionFromPointClose
     (Control : Scintilla_Type;
      x : Integer;
      y : Integer)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_POSITIONFROMPOINTCLOSE;
         wParam : GWindows.Types.Wparam := To_Wparam (x);
         lParam : GWindows.Types.Lparam := To_Lparam (y))
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end PositionFromPointClose;

   ----------
   -- Redo --
   ----------

   procedure Redo (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_REDO;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Redo;

   ---------------------
   -- ReleaseDocument --
   ---------------------

   procedure ReleaseDocument
     (Control : in out Scintilla_Type;
      doc : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_RELEASEDOCUMENT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := To_Lparam (doc));
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end ReleaseDocument;

   ----------------
   -- ReplaceSel --
   ----------------

   procedure ReplaceSel (Control : in out Scintilla_Type; text : GString) is

      S : String := GWindows.GStrings.To_String (text) & Character'Val (0);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_REPLACESEL;
         wParam : GWindows.Types.Wparam := 0;
         lParam : System.Address    := S (S'First)'Address);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end ReplaceSel;

   -------------------
   -- ReplaceTarget --
   -------------------

   function ReplaceTarget
     (Control : Scintilla_Type;
      text    : GString)
      return Integer
   is
      S : String := GWindows.GStrings.To_String (text) & Character'Val (0);

      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_REPLACETARGET;
         wParam : GWindows.Types.Wparam := text'Length;
         lParam : System.Address    := S (S'First)'Address)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end ReplaceTarget;

   ---------------------
   -- ReplaceTargetRE --
   ---------------------

   function ReplaceTargetRE
     (Control : Scintilla_Type;
      text    : GString)
      return Integer
   is
      S : String := GWindows.GStrings.To_String (text) & Character'Val (0);

      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_REPLACETARGETRE;
         wParam : GWindows.Types.Wparam := text'Length;
         lParam : System.Address    := S (S'First)'Address)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end ReplaceTargetRE;

   -----------------
   -- ScrollCaret --
   -----------------

   procedure ScrollCaret (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SCROLLCARET;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end ScrollCaret;

   ------------------
   -- SearchAnchor --
   ------------------

   procedure SearchAnchor (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SEARCHANCHOR;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SearchAnchor;

   --------------------
   -- SearchInTarget --
   --------------------

   function SearchInTarget
     (Control : Scintilla_Type;
      text : GString)
      return Integer
   is
      S : String := GWindows.GStrings.To_String (text) & Character'Val (0);

      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SEARCHINTARGET;
         wParam : GWindows.Types.Wparam := text'Length;
         lParam : System.Address        := S (S'First)'Address)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end SearchInTarget;

   ----------------
   -- SearchNext --
   ----------------

   function SearchNext
     (Control : Scintilla_Type;
      flags : Integer;
      text : GString)
      return Integer
   is
      S : String :=
        GWindows.GStrings.To_String (text) & Character'Val (0);

      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SEARCHNEXT;
         wParam : GWindows.Types.Wparam := To_Wparam (flags);
         lParam : System.Address        := S (S'First)'Address)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end SearchNext;

   ----------------
   -- SearchPrev --
   ----------------

   function SearchPrev
     (Control : Scintilla_Type;
      flags : Integer;
      text : GString)
      return Integer
   is
      S : String :=
        GWindows.GStrings.To_String (text) & Character'Val (0);

      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SEARCHPREV;
         wParam : GWindows.Types.Wparam := To_Wparam (flags);
         lParam : System.Address        := S (S'First)'Address)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end SearchPrev;

   ---------------
   -- SelectAll --
   ---------------

   procedure SelectAll (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SELECTALL;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SelectAll;

   --------------------------
   -- SelectionIsRectangle --
   --------------------------

   function SelectionIsRectangle (Control : Scintilla_Type) return Boolean is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SELECTIONISRECTANGLE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0)
        return Boolean;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return SendMessage;
   end SelectionIsRectangle;

   ---------------
   -- SetAnchor --
   ---------------

   procedure SetAnchor
     (Control : in out Scintilla_Type;
      posAnchor : Position)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETANCHOR;
         wParam : GWindows.Types.Wparam := To_Wparam (posAnchor);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetAnchor;

   ---------------------------
   -- SetBackSpaceUnIndents --
   ---------------------------

   procedure SetBackSpaceUnIndents
     (Control : in out Scintilla_Type;
      bsUnIndents : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETBACKSPACEUNINDENTS;
         wParam : GWindows.Types.Wparam := Boolean'Pos (bsUnIndents);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetBackSpaceUnIndents;

   ---------------------
   -- SetBufferedDraw --
   ---------------------

   procedure SetBufferedDraw
     (Control : in out Scintilla_Type;
      buffered : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETBUFFEREDDRAW;
         wParam : GWindows.Types.Wparam := Boolean'Pos (buffered);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetBufferedDraw;

   ------------------
   -- SetCaretFore --
   ------------------

   procedure SetCaretFore
     (Control : in out Scintilla_Type;
      fore : GWindows.Colors.Color_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETCARETFORE;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (fore);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetCaretFore;

   ----------------------
   -- SetCaretLineBack --
   ----------------------

   procedure SetCaretLineBack
     (Control : in out Scintilla_Type;
      back : GWindows.Colors.Color_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETCARETLINEBACK;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (back);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetCaretLineBack;

   -------------------------
   -- SetCaretLineVisible --
   -------------------------

   procedure SetCaretLineVisible
     (Control : in out Scintilla_Type;
      show : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETCARETLINEVISIBLE;
         wParam : GWindows.Types.Wparam := Boolean'Pos (show);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetCaretLineVisible;

   --------------------
   -- SetCaretPeriod --
   --------------------

   procedure SetCaretPeriod
     (Control : in out Scintilla_Type;
      periodMilliseconds : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETCARETPERIOD;
         wParam : GWindows.Types.Wparam := To_Wparam (periodMilliseconds);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetCaretPeriod;

   --------------------
   -- SetCaretPolicy --
   --------------------

   procedure SetCaretPolicy
     (Control     : in out Scintilla_Type;
      caretPolicy : in     Integer;
      caretSlop   : in     Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETCARETPOLICY;
         wParam : GWindows.Types.Wparam := To_Wparam (caretPolicy);
         lParam : GWindows.Types.Lparam := To_Lparam (caretSlop));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetCaretPolicy;

   -------------------
   -- SetCaretWidth --
   -------------------

   procedure SetCaretWidth
     (Control : in out Scintilla_Type;
      pixelWidth : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETCARETWIDTH;
         wParam : GWindows.Types.Wparam := To_Wparam (pixelWidth);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetCaretWidth;

   -----------------
   -- SetCodePage --
   -----------------

   procedure SetCodePage
     (Control : in out Scintilla_Type;
      codePage : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETCODEPAGE;
         wParam : GWindows.Types.Wparam := To_Wparam (codePage);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetCodePage;

   --------------------------
   -- SetControlCharSymbol --
   --------------------------

   procedure SetControlCharSymbol
     (Control : in out Scintilla_Type;
      symbol : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETCONTROLCHARSYMBOL;
         wParam : GWindows.Types.Wparam := To_Wparam (symbol);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetControlCharSymbol;

   -------------------
   -- SetCurrentPos --
   -------------------

   procedure SetCurrentPos
     (Control : in out Scintilla_Type;
      pos : Position)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETCURRENTPOS;
         wParam : GWindows.Types.Wparam := To_Wparam (pos);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetCurrentPos;

   ---------------
   -- SetCursor --
   ---------------

   procedure SetCursor
     (Control : in out Scintilla_Type;
      cursorType : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETCURSOR;
         wParam : GWindows.Types.Wparam := To_Wparam (cursorType);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetCursor;

   -------------------
   -- SetDocPointer --
   -------------------

   procedure SetDocPointer
     (Control : in out Scintilla_Type;
      pointer : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETDOCPOINTER;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := To_Lparam (pointer));
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetDocPointer;

   ------------------
   -- SetEdgeColor --
   ------------------

   procedure SetEdgeColor
     (Control : in out Scintilla_Type;
      edgeColor : GWindows.Colors.Color_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETEDGECOLOR;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (edgeColor);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetEdgeColor;

   -------------------
   -- SetEdgeColumn --
   -------------------

   procedure SetEdgeColumn
     (Control : in out Scintilla_Type;
      column : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETEDGECOLUMN;
         wParam : GWindows.Types.Wparam := To_Wparam (column);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetEdgeColumn;

   -----------------
   -- SetEdgeMode --
   -----------------

   procedure SetEdgeMode (Control : in out Scintilla_Type; mode : Integer) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETEDGEMODE;
         wParam : GWindows.Types.Wparam := To_Wparam (mode);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetEdgeMode;

   ----------------------
   -- SetEndAtLastLine --
   ----------------------

   procedure SetEndAtLastLine
     (Control : in out Scintilla_Type;
      endAtLastLine : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETENDATLASTLINE;
         wParam : GWindows.Types.Wparam := Boolean'Pos (endAtLastLine);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetEndAtLastLine;

   ----------------
   -- SetEOLMode --
   ----------------

   procedure SetEOLMode
     (Control : in out Scintilla_Type;
      eolMode : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETEOLMODE;
         wParam : GWindows.Types.Wparam := To_Wparam (eolMode);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetEOLMode;

   --------------
   -- SetFocus --
   --------------

   procedure SetFocus
     (Control : in out Scintilla_Type;
      focus   : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETFOCUS;
         wParam : GWindows.Types.Wparam := Boolean'Pos (focus);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetFocus;

   ---------------------
   -- SetFoldExpanded --
   ---------------------

   procedure SetFoldExpanded
     (Control : in out Scintilla_Type;
      line : Integer;
      expanded : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETFOLDEXPANDED;
         wParam : GWindows.Types.Wparam := To_Wparam (line);
         lParam : GWindows.Types.Wparam := Boolean'Pos (expanded));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetFoldExpanded;

   ------------------
   -- SetFoldFlags --
   ------------------

   procedure SetFoldFlags
     (Control : in out Scintilla_Type;
      Flags : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETFOLDFLAGS;
         wParam : GWindows.Types.Wparam := To_Wparam (Flags);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetFoldFlags;

   ------------------
   -- SetFoldLevel --
   ------------------

   procedure SetFoldLevel
     (Control : in out Scintilla_Type;
      line : Integer;
      level : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETFOLDLEVEL;
         wParam : GWindows.Types.Wparam := To_Wparam (line);
         lParam : GWindows.Types.Lparam := To_Lparam (level));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetFoldLevel;

   -----------------------
   -- SetHighlightGuide --
   -----------------------

   procedure SetHighlightGuide
     (Control : in out Scintilla_Type;
      column : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETHIGHLIGHTGUIDE;
         wParam : GWindows.Types.Wparam := To_Wparam (column);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetHighlightGuide;

   -------------------
   -- SetHScrollBar --
   -------------------

   procedure SetHScrollBar
     (Control : in out Scintilla_Type;
      show : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETHSCROLLBAR;
         wParam : GWindows.Types.Wparam := Boolean'Pos (show);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetHScrollBar;

   ---------------
   -- SetIndent --
   ---------------

   procedure SetIndent
     (Control : in out Scintilla_Type;
      indentSize : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETINDENT;
         wParam : GWindows.Types.Wparam := To_Wparam (indentSize);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetIndent;

   --------------------------
   -- SetIndentationGuides --
   --------------------------

   procedure SetIndentationGuides
     (Control : in out Scintilla_Type;
      show : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETINDENTATIONGUIDES;
         wParam : GWindows.Types.Wparam := Boolean'Pos (show);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetIndentationGuides;

   -----------------
   -- SetKeyWords --
   -----------------

   procedure SetKeyWords
     (Control    : in out Scintilla_Type;
      keywordSet : in     Integer;
      keyWords   : in     GString)
   is
      S : String :=
        GWindows.GStrings.To_String (keyWords) & Character'Val (0);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETKEYWORDS;
         wParam : GWindows.Types.Wparam := To_Wparam (keywordSet);
         lParam : System.Address        := S (S'First)'Address);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetKeyWords;

   --------------------
   -- SetLayoutCache --
   --------------------

   procedure SetLayoutCache
     (Control : in out Scintilla_Type;
      mode : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETLAYOUTCACHE;
         wParam : GWindows.Types.Wparam := To_Wparam (mode);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetLayoutCache;

   --------------
   -- SetLexer --
   --------------

   procedure SetLexer (Control : in out Scintilla_Type; lexer : Integer) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETLEXER;
         wParam : GWindows.Types.Wparam := To_Wparam (lexer);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetLexer;

   ----------------------
   -- SetLexerLanguage --
   ----------------------

   procedure SetLexerLanguage
     (Control : in out Scintilla_Type;
      language : GString)
   is
      S : String :=
        GWindows.GStrings.To_String (language) & Character'Val (0);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETLEXERLANGUAGE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : System.Address        := S (S'First)'Address);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetLexerLanguage;

   ------------------------
   -- SetLineIndentation --
   ------------------------

   procedure SetLineIndentation
     (Control : in out Scintilla_Type;
      line : Integer;
      indentSize : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETLINEINDENTATION;
         wParam : GWindows.Types.Wparam := To_Wparam (line);
         lParam : GWindows.Types.Lparam := To_Lparam (indentSize));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetLineIndentation;

   ------------------
   -- SetLineState --
   ------------------

   procedure SetLineState
     (Control : in out Scintilla_Type;
      line : Integer;
      state : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETLINESTATE;
         wParam : GWindows.Types.Wparam := To_Wparam (line);
         lParam : GWindows.Types.Lparam := To_Lparam (state));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetLineState;

   -------------------
   -- SetMarginLeft --
   -------------------

   procedure SetMarginLeft
     (Control : in out Scintilla_Type;
      pixelWidth : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETMARGINLEFT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := To_Lparam (pixelWidth));
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetMarginLeft;

   --------------------
   -- SetMarginMaskN --
   --------------------

   procedure SetMarginMaskN
     (Control : in out Scintilla_Type;
      margin : Integer;
      mask : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETMARGINMASKN;
         wParam : GWindows.Types.Wparam := To_Wparam (margin);
         lParam : GWindows.Types.Lparam := To_Lparam (mask));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetMarginMaskN;

   --------------------
   -- SetMarginRight --
   --------------------

   procedure SetMarginRight
     (Control : in out Scintilla_Type;
      pixelWidth : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETMARGINRIGHT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := To_Lparam (pixelWidth));
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetMarginRight;

   -------------------------
   -- SetMarginSensitiveN --
   -------------------------

   procedure SetMarginSensitiveN
     (Control : in out Scintilla_Type;
      margin : Integer;
      sensitive : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETMARGINSENSITIVEN;
         wParam : GWindows.Types.Wparam := To_Wparam (margin);
         lParam : GWindows.Types.Wparam := Boolean'Pos (sensitive));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetMarginSensitiveN;

   --------------------
   -- SetMarginTypeN --
   --------------------

   procedure SetMarginTypeN
     (Control : in out Scintilla_Type;
      margin : Integer;
      marginType : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETMARGINTYPEN;
         wParam : GWindows.Types.Wparam := To_Wparam (margin);
         lParam : GWindows.Types.Lparam := To_Lparam (marginType));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetMarginTypeN;

   ---------------------
   -- SetMarginWidthN --
   ---------------------

   procedure SetMarginWidthN
     (Control : in out Scintilla_Type;
      margin : Integer;
      pixelWidth : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETMARGINWIDTHN;
         wParam : GWindows.Types.Wparam := To_Wparam (margin);
         lParam : GWindows.Types.Lparam := To_Lparam (pixelWidth));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetMarginWidthN;

   ---------------------
   -- SetModEventMask --
   ---------------------

   procedure SetModEventMask
     (Control : in out Scintilla_Type;
      mask : Interfaces.C.unsigned)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETMODEVENTMASK;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (mask);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetModEventMask;

   --------------------------
   -- SetMouseDownCaptures --
   --------------------------

   procedure SetMouseDownCaptures
     (Control : in out Scintilla_Type;
      captures : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETMOUSEDOWNCAPTURES;
         wParam : GWindows.Types.Wparam := Boolean'Pos (captures);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetMouseDownCaptures;

   -----------------------
   -- SetMouseDwellTime --
   -----------------------

   procedure SetMouseDwellTime
     (Control : in out Scintilla_Type;
      periodMilliseconds : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETMOUSEDWELLTIME;
         wParam : GWindows.Types.Wparam := To_Wparam (periodMilliseconds);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetMouseDwellTime;

   -----------------
   -- SetOvertype --
   -----------------

   procedure SetOvertype
     (Control : in out Scintilla_Type;
      overtype : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETOVERTYPE;
         wParam : GWindows.Types.Wparam := Boolean'Pos (overtype);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetOvertype;

   -----------------------
   -- SetPrintColorMode --
   -----------------------

   procedure SetPrintColorMode
     (Control : in out Scintilla_Type;
      mode : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETPRINTCOLORMODE;
         wParam : GWindows.Types.Wparam := To_Wparam (mode);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetPrintColorMode;

   ---------------------------
   -- SetPrintMagnification --
   ---------------------------

   procedure SetPrintMagnification
     (Control : in out Scintilla_Type;
      magnification : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETPRINTMAGNIFICATION;
         wParam : GWindows.Types.Wparam := To_Wparam (magnification);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetPrintMagnification;

   -----------------
   -- SetProperty --
   -----------------

   procedure SetProperty
     (Control : in out Scintilla_Type;
      key : GString;
      value : GString)
   is
      S1 : String :=
        GWindows.GStrings.To_String (key) & Character'Val (0);

      S2 : String :=
        GWindows.GStrings.To_String (value) & Character'Val (0);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETPROPERTY;
         wParam : System.Address        := S1 (S1'First)'Address;
         lParam : System.Address        := S2 (S2'First)'Address);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetProperty;

   -----------------
   -- SetReadOnly --
   -----------------

   procedure SetReadOnly
     (Control : in out Scintilla_Type;
      readOnly : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETREADONLY;
         wParam : GWindows.Types.Wparam := Boolean'Pos (readOnly);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetReadOnly;

   ------------------
   -- SetSavePoint --
   ------------------

   procedure SetSavePoint (Control : Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETSAVEPOINT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetSavePoint;

   --------------------
   -- SetScrollWidth --
   --------------------

   procedure SetScrollWidth
     (Control : in out Scintilla_Type;
      pixelWidth : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETSCROLLWIDTH;
         wParam : GWindows.Types.Wparam := To_Wparam (pixelWidth);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetScrollWidth;

   --------------------
   -- SetSearchFlags --
   --------------------

   procedure SetSearchFlags
     (Control : in out Scintilla_Type;
      flags : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETSEARCHFLAGS;
         wParam : GWindows.Types.Wparam := To_Wparam (flags);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetSearchFlags;

   ------------
   -- SetSel --
   ------------

   procedure SetSel
     (Control : in out Scintilla_Type;
      start : Position;
      endp : Position)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETSEL;
         wParam : GWindows.Types.Wparam := To_Wparam (start);
         lParam : GWindows.Types.Lparam := To_Lparam (endp));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetSel;

   ----------------
   -- SetSelBack --
   ----------------

   procedure SetSelBack
     (Control    : in out Scintilla_Type;
      useSetting : in     Boolean;
      back       : in     GWindows.Colors.Color_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETSELBACK;
         wParam : GWindows.Types.Wparam := Boolean'Pos (useSetting);
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (back));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetSelBack;

   ---------------------
   -- SetSelectionEnd --
   ---------------------

   procedure SetSelectionEnd
     (Control : in out Scintilla_Type;
      pos : Position)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETSELECTIONEND;
         wParam : GWindows.Types.Wparam := To_Wparam (pos);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetSelectionEnd;

   -----------------------
   -- SetSelectionStart --
   -----------------------

   procedure SetSelectionStart
     (Control : in out Scintilla_Type;
      pos : Position)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETSELECTIONSTART;
         wParam : GWindows.Types.Wparam := To_Wparam (pos);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetSelectionStart;

   ----------------
   -- SetSelFore --
   ----------------

   procedure SetSelFore
     (Control    : in out Scintilla_Type;
      useSetting : in     Boolean;
      fore       : in     GWindows.Colors.Color_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETSELFORE;
         wParam : GWindows.Types.Wparam := Boolean'Pos (useSetting);
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (fore));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetSelFore;

   ---------------
   -- SetStatus --
   ---------------

   procedure SetStatus
     (Control : in out Scintilla_Type;
      statusCode : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETSTATUS;
         wParam : GWindows.Types.Wparam := To_Wparam (statusCode);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetStatus;

   ------------------
   -- SetStyleBits --
   ------------------

   procedure SetStyleBits
     (Control : in out Scintilla_Type;
      bits : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETSTYLEBITS;
         wParam : GWindows.Types.Wparam := To_Wparam (bits);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetStyleBits;

   ----------------
   -- SetStyling --
   ----------------

   procedure SetStyling
     (Control : in out Scintilla_Type;
      length : Integer;
      style : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETSTYLING;
         wParam : GWindows.Types.Wparam := To_Wparam (length);
         lParam : GWindows.Types.Lparam := To_Lparam (style));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetStyling;

   ------------------
   -- SetStylingEx --
   ------------------

   procedure SetStylingEx
     (Control : in out Scintilla_Type;
      length : Integer;
      styles : GString)
   is
      S : String :=
        GWindows.GStrings.To_String (styles) & Character'Val (0);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETSTYLINGEX;
         wParam : GWindows.Types.Wparam := To_Wparam (length);
         lParam : System.Address        := S (S'First)'Address);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetStylingEx;

   -------------------
   -- SetTabIndents --
   -------------------

   procedure SetTabIndents
     (Control : in out Scintilla_Type;
      tabIndents : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETTABINDENTS;
         wParam : GWindows.Types.Wparam := Boolean'Pos (tabIndents);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetTabIndents;

   -----------------
   -- SetTabWidth --
   -----------------

   procedure SetTabWidth
     (Control : in out Scintilla_Type;
      tabWidth : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETTABWIDTH;
         wParam : GWindows.Types.Wparam := To_Wparam (tabWidth);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetTabWidth;

   ------------------
   -- SetTargetEnd --
   ------------------

   procedure SetTargetEnd
     (Control : in out Scintilla_Type;
      pos : Position)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETTARGETEND;
         wParam : GWindows.Types.Wparam := To_Wparam (pos);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetTargetEnd;

   --------------------
   -- SetTargetStart --
   --------------------

   procedure SetTargetStart
     (Control : in out Scintilla_Type;
      pos : Position)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETTARGETSTART;
         wParam : GWindows.Types.Wparam := To_Wparam (pos);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetTargetStart;

   -------------
   -- SetText --
   -------------

   procedure SetText (Control : in out Scintilla_Type; text : GString)
   is
      S : String :=
        GWindows.GStrings.To_String (text) & Character'Val (0);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETTEXT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : System.Address        := S (S'First)'Address);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetText;

   -----------------------
   -- SetUndoCollection --
   -----------------------

   procedure SetUndoCollection
     (Control : in out Scintilla_Type;
      collectUndo : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETUNDOCOLLECTION;
         wParam : GWindows.Types.Wparam := Boolean'Pos (collectUndo);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetUndoCollection;

   -------------------
   -- SetUsePalette --
   -------------------

   procedure SetUsePalette
     (Control : in out Scintilla_Type;
      usePalette : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETUSEPALETTE;
         wParam : GWindows.Types.Wparam := Boolean'Pos (usePalette);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetUsePalette;

   ----------------
   -- SetUseTabs --
   ----------------

   procedure SetUseTabs
     (Control : in out Scintilla_Type;
      useTabs : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETUSETABS;
         wParam : GWindows.Types.Wparam := Boolean'Pos (useTabs);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetUseTabs;

   ----------------
   -- SetViewEOL --
   ----------------

   procedure SetViewEOL
     (Control : in out Scintilla_Type;
      visible : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETVIEWEOL;
         wParam : GWindows.Types.Wparam := Boolean'Pos (visible);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetViewEOL;

   ---------------
   -- SetViewWS --
   ---------------

   procedure SetViewWS (Control : in out Scintilla_Type; views : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETVIEWWS;
         wParam : GWindows.Types.Wparam := To_Wparam (views);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetViewWS;

   ----------------------
   -- SetVisiblePolicy --
   ----------------------

   procedure SetVisiblePolicy
     (Control       : in out Scintilla_Type;
      visiblePolicy : in     Integer;
      visibleSlop   : in     Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETVISIBLEPOLICY;
         wParam : GWindows.Types.Wparam := To_Wparam (visiblePolicy);
         lParam : GWindows.Types.Lparam := To_Lparam (visibleSlop));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetVisiblePolicy;

   ------------------
   -- SetWordChars --
   ------------------

   procedure SetWordChars
     (Control : in out Scintilla_Type;
      characters : GString)
   is
      S : String :=
        GWindows.GStrings.To_String (characters) & Character'Val (0);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETWORDCHARS;
         wParam : GWindows.Types.Wparam := 0;
         lParam : System.Address        := S (S'First)'Address);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetWordChars;

   -----------------
   -- SetWrapMode --
   -----------------

   procedure SetWrapMode (Control : in out Scintilla_Type; mode : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETWRAPMODE;
         wParam : GWindows.Types.Wparam := To_Wparam (mode);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetWrapMode;

   ----------------
   -- SetXOffset --
   ----------------

   procedure SetXOffset
     (Control : in out Scintilla_Type;
      newOffset : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETXOFFSET;
         wParam : GWindows.Types.Wparam := To_Wparam (newOffset);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetXOffset;

   -------------
   -- SetZoom --
   -------------

   procedure SetZoom (Control : in out Scintilla_Type; zoom : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SETZOOM;
         wParam : GWindows.Types.Wparam := To_Wparam (zoom);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end SetZoom;

   ---------------
   -- ShowLines --
   ---------------

   procedure ShowLines
     (Control : in out Scintilla_Type;
      lineStart : Integer;
      lineEnd : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_SHOWLINES;
         wParam : GWindows.Types.Wparam := To_Wparam (lineStart);
         lParam : GWindows.Types.Lparam := To_Lparam (lineEnd));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end ShowLines;

   -----------------
   -- StartRecord --
   -----------------

   procedure StartRecord (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_STARTRECORD;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end StartRecord;

   ------------------
   -- StartStyling --
   ------------------

   procedure StartStyling
     (Control : in out Scintilla_Type;
      pos : Position;
      mask : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_STARTSTYLING;
         wParam : GWindows.Types.Wparam := To_Wparam (pos);
         lParam : GWindows.Types.Lparam := To_Lparam (mask));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end StartStyling;

   ----------------
   -- StopRecord --
   ----------------

   procedure StopRecord (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_STOPRECORD;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end StopRecord;

   -------------------
   -- StyleClearAll --
   -------------------

   procedure StyleClearAll (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_STYLECLEARALL;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end StyleClearAll;

   -----------------------
   -- StyleResetDefault --
   -----------------------

   procedure StyleResetDefault
     (Control : in out Scintilla_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_STYLERESETDEFAULT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end StyleResetDefault;

   ------------------
   -- StyleSetBack --
   ------------------

   procedure StyleSetBack
     (Control : in out Scintilla_Type;
      style   : in     Integer;
      back    : in     GWindows.Colors.Color_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_STYLESETBACK;
         wParam : GWindows.Types.Wparam := To_Wparam (style);
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (back));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end StyleSetBack;

   ------------------
   -- StyleSetBold --
   ------------------

   procedure StyleSetBold
     (Control : in out Scintilla_Type;
      style : Integer;
      bold : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_STYLESETBOLD;
         wParam : GWindows.Types.Wparam := To_Wparam (style);
         lParam : GWindows.Types.Lparam := Boolean'Pos (bold));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end StyleSetBold;

   ------------------
   -- StyleSetCase --
   ------------------

   procedure StyleSetCase
     (Control : in out Scintilla_Type;
      style : Integer;
      caseForce : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_STYLESETCASE;
         wParam : GWindows.Types.Wparam := To_Wparam (style);
         lParam : GWindows.Types.Lparam := To_Lparam (caseForce));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end StyleSetCase;

   ------------------------
   -- StyleSetChangeable --
   ------------------------

   procedure StyleSetChangeable
     (Control : in out Scintilla_Type;
      style : Integer;
      changeable : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_STYLESETCHANGEABLE;
         wParam : GWindows.Types.Wparam := To_Wparam (style);
         lParam : GWindows.Types.Lparam := Boolean'Pos (changeable));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end StyleSetChangeable;

   --------------------------
   -- StyleSetCharacterSet --
   --------------------------

   procedure StyleSetCharacterSet
     (Control      : in out Scintilla_Type;
      style        : Integer;
      characterSet : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_STYLESETCHARACTERSET;
         wParam : GWindows.Types.Wparam := To_Wparam (style);
         lParam : GWindows.Types.Lparam := To_Lparam (characterSet));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end StyleSetCharacterSet;

   -----------------------
   -- StyleSetEOLFilled --
   -----------------------

   procedure StyleSetEOLFilled
     (Control : in out Scintilla_Type;
      style : Integer;
      filled : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_STYLESETEOLFILLED;
         wParam : GWindows.Types.Wparam := To_Wparam (style);
         lParam : GWindows.Types.Wparam := Boolean'Pos (filled));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end StyleSetEOLFilled;

   ------------------
   -- StyleSetFont --
   ------------------

   procedure StyleSetFont
     (Control : in out Scintilla_Type;
      style : Integer;
      fontName : GString)
   is
      S : String :=
        GWindows.GStrings.To_String (fontName) & Character'Val (0);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_STYLESETFONT;
         wParam : GWindows.Types.Wparam := To_Wparam (style);
         lParam : System.Address        := S (S'First)'Address);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end StyleSetFont;

   ------------------
   -- StyleSetFore --
   ------------------

   procedure StyleSetFore
     (Control : in out Scintilla_Type;
      style : Integer;
      fore : GWindows.Colors.Color_Type)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_STYLESETFORE;
         wParam : GWindows.Types.Wparam := To_Wparam (style);
         lParam : GWindows.Types.Lparam := GWindows.Types.Lparam (fore));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end StyleSetFore;

   --------------------
   -- StyleSetItalic --
   --------------------

   procedure StyleSetItalic
     (Control : in out Scintilla_Type;
      style : Integer;
      italic : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_STYLESETITALIC;
         wParam : GWindows.Types.Wparam := To_Wparam (style);
         lParam : GWindows.Types.Wparam := Boolean'Pos (italic));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end StyleSetItalic;

   ------------------
   -- StyleSetSize --
   ------------------

   procedure StyleSetSize
     (Control : in out Scintilla_Type;
      style : Integer;
      sizePoints : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_STYLESETSIZE;
         wParam : GWindows.Types.Wparam := To_Wparam (style);
         lParam : GWindows.Types.Lparam := To_Lparam (sizePoints));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end StyleSetSize;

   -----------------------
   -- StyleSetUnderline --
   -----------------------

   procedure StyleSetUnderline
     (Control : in out Scintilla_Type;
      style : Integer;
      underline : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_STYLESETUNDERLINE;
         wParam : GWindows.Types.Wparam := To_Wparam (style);
         lParam : GWindows.Types.Lparam := Boolean'Pos (underline));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end StyleSetUnderline;

   ---------------------
   -- StyleSetVisible --
   ---------------------

   procedure StyleSetVisible
     (Control : in out Scintilla_Type;
      style : Integer;
      visible : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_STYLESETVISIBLE;
         wParam : GWindows.Types.Wparam := To_Wparam (style);
         lParam : GWindows.Types.Wparam := Boolean'Pos (visible));
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end StyleSetVisible;

   ---------
   -- Tab --
   ---------

   procedure Tab (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_TAB;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Tab;

   ---------------
   -- TextWidth --
   ---------------

   function TextWidth
     (Control : Scintilla_Type;
      style : Integer;
      text : GString)
      return Integer
   is
      S : String :=
        GWindows.GStrings.To_String (text) & Character'Val (0);

      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_TEXTWIDTH;
         wParam : GWindows.Types.Wparam := To_Wparam (style);
         lParam : System.Address        := S (S'First)'Address)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end TextWidth;

   ----------------
   -- ToggleFold --
   ----------------

   procedure ToggleFold (Control : in out Scintilla_Type; line : Integer)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_TOGGLEFOLD;
         wParam : GWindows.Types.Wparam := To_Wparam (line);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end ToggleFold;

   ----------
   -- Undo --
   ----------

   procedure Undo (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_UNDO;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end Undo;

   ---------------
   -- UpperCase --
   ---------------

   procedure UpperCase (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_UPPERCASE;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end UpperCase;

   --------------
   -- UsePopUp --
   --------------

   procedure UsePopUp
     (Control : in out Scintilla_Type;
      allowPopUp : Boolean)
   is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_USEPOPUP;
         wParam : GWindows.Types.Wparam := Boolean'Pos (allowPopUp);
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end UsePopUp;

   ------------------
   -- UserListShow --
   ------------------

   procedure UserListShow
     (Control : in out Scintilla_Type;
      listType : Integer;
      itemList : GString)
   is
      S : String :=
        GWindows.GStrings.To_String (itemList) & Character'Val (0);

      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_USERLISTSHOW;
         wParam : GWindows.Types.Wparam := To_Wparam (listType);
         lParam : System.Address        := S (S'First)'Address);
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end UserListShow;

   ------------
   -- VCHome --
   ------------

   procedure VCHome (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_VCHOME;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end VCHome;

   ------------------
   -- VCHomeExtend --
   ------------------

   procedure VCHomeExtend (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_VCHOMEEXTEND;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end VCHomeExtend;

   ------------------------
   -- VisibleFromDocLine --
   ------------------------

   function VisibleFromDocLine
     (Control : Scintilla_Type;
      line : Integer)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_VISIBLEFROMDOCLINE;
         wParam : GWindows.Types.Wparam := To_Wparam (line);
         lParam : GWindows.Types.Lparam := 0)
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end VisibleFromDocLine;

   ---------------------
   -- WordEndPosition --
   ---------------------

   function WordEndPosition
     (Control : Scintilla_Type;
      pos : Position;
      onlyWordCharacters : Boolean)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_WORDENDPOSITION;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (pos);
         lParam : GWindows.Types.Lparam := Boolean'Pos (onlyWordCharacters))
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end WordEndPosition;

   --------------
   -- WordLeft --
   --------------

   procedure WordLeft (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_WORDLEFT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end WordLeft;

   --------------------
   -- WordLeftExtend --
   --------------------

   procedure WordLeftExtend (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_WORDLEFTEXTEND;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end WordLeftExtend;

   ------------------
   -- WordPartLeft --
   ------------------

   procedure WordPartLeft (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_WORDPARTLEFT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end WordPartLeft;

   ------------------------
   -- WordPartLeftExtend --
   ------------------------

   procedure WordPartLeftExtend (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_WORDPARTLEFTEXTEND;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end WordPartLeftExtend;

   -------------------
   -- WordPartRight --
   -------------------

   procedure WordPartRight (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_WORDPARTRIGHT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end WordPartRight;

   -------------------------
   -- WordPartRightExtend --
   -------------------------

   procedure WordPartRightExtend (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_WORDPARTRIGHTEXTEND;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end WordPartRightExtend;

   ---------------
   -- WordRight --
   ---------------

   procedure WordRight (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_WORDRIGHT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end WordRight;

   ---------------------
   -- WordRightExtend --
   ---------------------

   procedure WordRightExtend (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_WORDRIGHTEXTEND;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end WordRightExtend;

   -----------------------
   -- WordStartPosition --
   -----------------------

   function WordStartPosition
     (Control : Scintilla_Type;
      pos : Position;
      onlyWordCharacters : Boolean)
      return Integer
   is
      function SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_WORDSTARTPOSITION;
         wParam : GWindows.Types.Wparam := GWindows.Types.Wparam (pos);
         lParam : GWindows.Types.Lparam := Boolean'Pos (onlyWordCharacters))
        return GWindows.Types.Lresult;
      pragma Import (StdCall, SendMessage,
                       "SendMessage" & Character_Mode_Identifier);
   begin
      return GWindows.Types.To_Integer (SendMessage);
   end WordStartPosition;

   ------------
   -- ZoomIn --
   ------------

   procedure ZoomIn (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_ZOOMIN;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end ZoomIn;

   -------------
   -- ZoomOut --
   -------------

   procedure ZoomOut (Control : in out Scintilla_Type) is
      procedure SendMessage
        (hwnd   : GWindows.Types.Handle := Handle (Control);
         uMsg   : Interfaces.C.int      := SCI_ZOOMOUT;
         wParam : GWindows.Types.Wparam := 0;
         lParam : GWindows.Types.Lparam := 0);
      pragma Import (StdCall, SendMessage,
                     "SendMessage" & Character_Mode_Identifier);
   begin
      SendMessage;
   end ZoomOut;

   ------------------------
   -- On_Character_Added --
   ------------------------

   procedure On_Character_Added
     (Control     : in out Scintilla_Type;
      Special_Key : in     GWindows.Windows.Special_Key_Type;
      Value       : in     GCharacter)
   is
   begin
      Fire_On_Character_Added (Control, Special_Key, Value);
   end On_Character_Added;

   --------------------------------
   -- On_Character_Added_Handler --
   --------------------------------

   procedure On_Character_Added_Handler
     (Control : in out Scintilla_Type;
      Handler : in     GWindows.Windows.Character_Event)
   is
   begin
      Control.On_Character_Added_Event := Handler;
   end On_Character_Added_Handler;

   -----------------------------
   -- Fire_On_Character_Added --
   -----------------------------

   procedure Fire_On_Character_Added
     (Control     : in out Scintilla_Type;
      Special_Key : in     GWindows.Windows.Special_Key_Type;
      Value       : in     GCharacter)
   is
      use GWindows.Base;
      use type GWindows.Windows.Character_Event;
   begin
      if Control.On_Character_Added_Event /= null then
         Control.On_Character_Added_Event (Base_Window_Type'Class (Control),
                                           Special_Key,
                                           Value);
      end if;
   end Fire_On_Character_Added;

   --------------
   -- On_Focus --
   --------------

   procedure On_Focus (Control : in out Scintilla_Type)
   is
   begin
      Fire_On_Focus (Control);
   end On_Focus;

   ----------------------
   -- On_Focus_Handler --
   ----------------------

   procedure On_Focus_Handler
     (Control : in out Scintilla_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Focus_Event := Handler;
   end On_Focus_Handler;

   -------------------
   -- Fire_On_Focus --
   -------------------

   procedure Fire_On_Focus (Control : in out Scintilla_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Focus_Event /= null then
         Control.On_Focus_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Focus;

   -------------------
   -- On_Lost_Focus --
   -------------------

   procedure On_Lost_Focus (Control : in out Scintilla_Type)
   is
   begin
      Fire_On_Lost_Focus (Control);
   end On_Lost_Focus;

   ---------------------------
   -- On_Lost_Focus_Handler --
   ---------------------------

   procedure On_Lost_Focus_Handler
     (Control : in out Scintilla_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Lost_Focus_Event := Handler;
   end On_Lost_Focus_Handler;

   ------------------------
   -- Fire_On_Lost_Focus --
   ------------------------

   procedure Fire_On_Lost_Focus (Control : in out Scintilla_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Lost_Focus_Event /= null then
         Control.On_Lost_Focus_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Lost_Focus;

   --------------
   -- On_Change --
   --------------

   procedure On_Change (Control : in out Scintilla_Type)
   is
   begin
      Fire_On_Change (Control);
   end On_Change;

   ----------------------
   -- On_Change_Handler --
   ----------------------

   procedure On_Change_Handler
     (Control : in out Scintilla_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Change_Event := Handler;
   end On_Change_Handler;

   -------------------
   -- Fire_On_Change --
   -------------------

   procedure Fire_On_Change (Control : in out Scintilla_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Change_Event /= null then
         Control.On_Change_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Change;

   ------------------
   -- On_Update_UI --
   ------------------

   procedure On_Update_UI (Control : in out Scintilla_Type)
   is
   begin
      Fire_On_Update_UI (Control);
   end On_Update_UI;

   --------------------------
   -- On_Update_UI_Handler --
   --------------------------

   procedure On_Update_UI_Handler
     (Control : in out Scintilla_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Update_UI_Event := Handler;
   end On_Update_UI_Handler;

   -----------------------
   -- Fire_On_Update_UI --
   -----------------------

   procedure Fire_On_Update_UI (Control : in out Scintilla_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Update_UI_Event /= null then
         Control.On_Update_UI_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Update_UI;

   ---------------------------
   -- On_Save_Point_Reached --
   ---------------------------

   procedure On_Save_Point_Reached (Control : in out Scintilla_Type)
   is
   begin
      Fire_On_Save_Point_Reached (Control);
   end On_Save_Point_Reached;

   -----------------------------------
   -- On_Save_Point_Reached_Handler --
   -----------------------------------

   procedure On_Save_Point_Reached_Handler
     (Control : in out Scintilla_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Save_Point_Reached_Event := Handler;
   end On_Save_Point_Reached_Handler;

   --------------------------------
   -- Fire_On_Save_Point_Reached --
   --------------------------------

   procedure Fire_On_Save_Point_Reached (Control : in out Scintilla_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Save_Point_Reached_Event /= null then
         Control.On_Save_Point_Reached_Event
           (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Save_Point_Reached;

   ------------------------
   -- On_Save_Point_Left --
   ------------------------

   procedure On_Save_Point_Left (Control : in out Scintilla_Type)
   is
   begin
      Fire_On_Save_Point_Left (Control);
   end On_Save_Point_Left;

   --------------------------------
   -- On_Save_Point_Left_Handler --
   --------------------------------

   procedure On_Save_Point_Left_Handler
     (Control : in out Scintilla_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Save_Point_Left_Event := Handler;
   end On_Save_Point_Left_Handler;

   -----------------------------
   -- Fire_On_Save_Point_Left --
   -----------------------------

   procedure Fire_On_Save_Point_Left (Control : in out Scintilla_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Save_Point_Left_Event /= null then
         Control.On_Save_Point_Left_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Save_Point_Left;

   ------------------------------------
   -- On_Attempt_To_Modify_Read_Only --
   ------------------------------------

   procedure On_Attempt_To_Modify_Read_Only (Control : in out Scintilla_Type)
   is
   begin
      Fire_On_Attempt_To_Modify_Read_Only (Control);
   end On_Attempt_To_Modify_Read_Only;

   --------------------------------------------
   -- On_Attempt_To_Modify_Read_Only_Handler --
   --------------------------------------------

   procedure On_Attempt_To_Modify_Read_Only_Handler
     (Control : in out Scintilla_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Attempt_To_Modify_Read_Only_Event := Handler;
   end On_Attempt_To_Modify_Read_Only_Handler;

   -----------------------------------------
   -- Fire_On_Attempt_To_Modify_Read_Only --
   -----------------------------------------

   procedure Fire_On_Attempt_To_Modify_Read_Only
     (Control : in out Scintilla_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Attempt_To_Modify_Read_Only_Event /= null then
         Control.On_Attempt_To_Modify_Read_Only_Event
           (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Attempt_To_Modify_Read_Only;

   ----------------
   -- On_Painted --
   ----------------

   procedure On_Painted (Control : in out Scintilla_Type)
   is
   begin
      Fire_On_Painted (Control);
   end On_Painted;

   ------------------------
   -- On_Painted_Handler --
   ------------------------

   procedure On_Painted_Handler
     (Control : in out Scintilla_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Painted_Event := Handler;
   end On_Painted_Handler;

   ---------------------
   -- Fire_On_Painted --
   ---------------------

   procedure Fire_On_Painted (Control : in out Scintilla_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Painted_Event /= null then
         Control.On_Painted_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Painted;

   -------------
   -- On_Zoom --
   -------------

   procedure On_Zoom (Control : in out Scintilla_Type)
   is
   begin
      Fire_On_Zoom (Control);
   end On_Zoom;

   ---------------------
   -- On_Zoom_Handler --
   ---------------------

   procedure On_Zoom_Handler
     (Control : in out Scintilla_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Zoom_Event := Handler;
   end On_Zoom_Handler;

   ------------------
   -- Fire_On_Zoom --
   ------------------

   procedure Fire_On_Zoom (Control : in out Scintilla_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Zoom_Event /= null then
         Control.On_Zoom_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Zoom;

   ---------------------
   -- On_Style_Needed --
   ---------------------

   procedure On_Style_Needed (Control : in out Scintilla_Type;
                              Pos     : in     Position)
   is
   begin
      Fire_On_Style_Needed (Control, Pos);
   end On_Style_Needed;

   -----------------------------
   -- On_Style_Needed_Handler --
   -----------------------------

   procedure On_Style_Needed_Handler (Control : in out Scintilla_Type;
                                      Handler : in     Position_Event)
   is
   begin
      Control.On_Style_Needed_Event := Handler;
   end On_Style_Needed_Handler;

   --------------------------
   -- Fire_On_Style_Needed --
   --------------------------

   procedure Fire_On_Style_Needed (Control : in out Scintilla_Type;
                                           Pos     : in     Position)
   is
      use GWindows.Base;
   begin
      if Control.On_Style_Needed_Event /= null then
         Control.On_Style_Needed_Event (Base_Window_Type'Class (Control),
                                        Pos);
      end if;
   end Fire_On_Style_Needed;

   -------------------------
   -- On_Position_Changed --
   -------------------------

   procedure On_Position_Changed (Control : in out Scintilla_Type;
                                  Pos     : in     Position)
   is
   begin
      Fire_On_Position_Changed (Control, Pos);
   end On_Position_Changed;

   ---------------------------------
   -- On_Position_Changed_Handler --
   ---------------------------------

   procedure On_Position_Changed_Handler (Control : in out Scintilla_Type;
                                          Handler : in     Position_Event)
   is
   begin
      Control.On_Position_Changed_Event := Handler;
   end On_Position_Changed_Handler;

   ------------------------------
   -- Fire_On_Position_Changed --
   ------------------------------

   procedure Fire_On_Position_Changed
     (Control : in out Scintilla_Type;
      Pos     : in     Position)
   is
      use GWindows.Base;
   begin
      if Control.On_Position_Changed_Event /= null then
         Control.On_Position_Changed_Event (Base_Window_Type'Class (Control),
                                            Pos);
      end if;
   end Fire_On_Position_Changed;

   ---------------------
   -- On_Double_Click --
   ---------------------

   procedure On_Double_Click (Control   : in out Scintilla_Type)
   is
   begin
      Fire_On_Double_Click (Control);
   end On_Double_Click;

   -----------------------------
   -- On_Double_Click_Handler --
   -----------------------------

   procedure On_Double_Click_Handler
     (Control : in out Scintilla_Type;
      Handler : in     GWindows.Base.Action_Event)
   is
   begin
      Control.On_Double_Click_Event := Handler;
   end On_Double_Click_Handler;

   --------------------------
   -- Fire_On_Double_Click --
   --------------------------

   procedure Fire_On_Double_Click (Control : in out Scintilla_Type)
   is
      use GWindows.Base;
   begin
      if Control.On_Double_Click_Event /= null then
         Control.On_Double_Click_Event (Base_Window_Type'Class (Control));
      end if;
   end Fire_On_Double_Click;

   -----------------
   -- On_Modified --
   -----------------

   procedure On_Modified (Control             : in out Scintilla_Type;
                          Pos                 : in     Position;
                          Modification_Type   : in     Integer;
                          Text                : in     GString;
                          Lines_Added         : in     Integer;
                          Line                : in     Integer;
                          Fold_Level_Now      : in     Integer;
                          Fold_Level_Previous : in     Integer)
   is
   begin
      Fire_On_Modified (Control,
                        Pos,
                        Modification_Type,
                        Text,
                        Lines_Added,
                        Line,
                        Fold_Level_Now,
                        Fold_Level_Previous);
   end On_Modified;

   -------------------------
   -- On_Modified_Handler --
   -------------------------

   procedure On_Modified_Handler (Control : in out Scintilla_Type;
                                  Handler : in     Modified_Event)
   is
   begin
      Control.On_Modified_Event := Handler;
   end On_Modified_Handler;

   ----------------------
   -- Fire_On_Modified --
   ----------------------

   procedure Fire_On_Modified
     (Control             : in out Scintilla_Type;
      Pos                 : in     Position;
      Modification_Type   : in     Integer;
      Text                : in     GString;
      Lines_Added         : in     Integer;
      Line                : in     Integer;
      Fold_Level_Now      : in     Integer;
      Fold_Level_Previous : in     Integer)
   is
      use GWindows.Base;
   begin
      if Control.On_Modified_Event /= null then
         Control.On_Modified_Event (Base_Window_Type'Class (Control),
                                    Pos,
                                    Modification_Type,
                                    Text,
                                    Lines_Added,
                                    Line,
                                    Fold_Level_Now,
                                    Fold_Level_Previous);
      end if;
   end Fire_On_Modified;

   -------------------
   -- On_Macro_Read --
   -------------------

   procedure On_Macro_Read (Control : in out Scintilla_Type;
                            Message : in     Integer;
                            wParam  : in     Integer;
                            lParam  : in     Integer)
   is
   begin
      Fire_On_Macro_Read (Control,
                          Message,
                          wParam,
                          lParam);
   end On_Macro_Read;

   ---------------------------
   -- On_Macro_Read_Handler --
   ---------------------------

   procedure On_Macro_Read_Handler
     (Control : in out Scintilla_Type;
      Handler : in     Macro_Read_Event)
   is
   begin
      Control.On_Macro_Read_Event := Handler;
   end On_Macro_Read_Handler;

   ------------------------
   -- Fire_On_Macro_Read --
   ------------------------

   procedure Fire_On_Macro_Read (Control : in out Scintilla_Type;
                                 Message : in     Integer;
                                 wParam  : in     Integer;
                                 lParam  : in     Integer)
   is
      use GWindows.Base;
   begin
      if Control.On_Macro_Read_Event /= null then
         Control.On_Macro_Read_Event (Base_Window_Type'Class (Control),
                                      Message,
                                      wParam,
                                      lParam);
      end if;
   end Fire_On_Macro_Read;

   ---------------------
   -- On_Margin_Click --
   ---------------------

   procedure On_Margin_Click (Control : in out Scintilla_Type;
                              Pos     : in     Position;
                              Margin  : in     Integer)
   is
   begin
      Fire_On_Margin_Click (Control, Pos, Margin);
   end On_Margin_Click;

   -----------------------------
   -- On_Margin_Click_Handler --
   -----------------------------

   procedure On_Margin_Click_Handler (Control : in out Scintilla_Type;
                                      Handler : in     Margin_Click_Event)
   is
   begin
      Control.On_Margin_Click_Event := Handler;
   end On_Margin_Click_Handler;

   --------------------------
   -- Fire_On_Margin_Click --
   --------------------------

   procedure Fire_On_Margin_Click (Control : in out Scintilla_Type;
                                   Pos     : in     Position;
                                   Margin  : in     Integer)
   is
      use GWindows.Base;
   begin
      if Control.On_Margin_Click_Event /= null then
         Control.On_Margin_Click_Event (Base_Window_Type'Class (Control),
                                        Pos,
                                        Margin);
      end if;
   end Fire_On_Margin_Click;

   -------------------
   -- On_Need_Shown --
   -------------------

   procedure On_Need_Shown (Control : in out Scintilla_Type;
                            Pos     : in     Position;
                            Length  : in     Integer)
   is
   begin
      Fire_On_Need_Shown (Control, Pos, Length);
   end On_Need_Shown;

   ---------------------------
   -- On_Need_Shown_Handler --
   ---------------------------

   procedure On_Need_Shown_Handler
     (Control : in out Scintilla_Type;
      Handler : in     Need_Shown_Event)
   is
   begin
      Control.On_Need_Shown_Event := Handler;
   end On_Need_Shown_Handler;

   ------------------------
   -- Fire_On_Need_Shown --
   ------------------------

   procedure Fire_On_Need_Shown (Control : in out Scintilla_Type;
                                 Pos     : in     Position;
                                 Length  : in     Integer)
   is
      use GWindows.Base;
   begin
      if Control.On_Need_Shown_Event /= null then
         Control.On_Need_Shown_Event (Base_Window_Type'Class (Control),
                                      Pos,
                                      Length);
      end if;
   end Fire_On_Need_Shown;

   ----------------------------
   -- On_User_List_Selection --
   ----------------------------

   procedure On_User_List_Selection
     (Control   : in out Scintilla_Type;
      List_Type : in     Integer;
      Text      : in     GString)
   is
   begin
      Fire_On_User_List_Selection (Control,
                                   List_Type,
                                   Text);
   end On_User_List_Selection;

   ------------------------------------
   -- On_User_List_Selection_Handler --
   ------------------------------------

   procedure On_User_List_Selection_Handler
     (Control : in out Scintilla_Type;
      Handler : in     User_List_Selection_Event)
   is
   begin
      Control.On_User_List_Selection_Event := Handler;
   end On_User_List_Selection_Handler;

   ---------------------------------
   -- Fire_On_User_List_Selection --
   ---------------------------------

   procedure Fire_On_User_List_Selection (Control : in out Scintilla_Type;
                                          List_Type : in     Integer;
                                          Text      : in     GString)
   is
      use GWindows.Base;
   begin
      if Control.On_User_List_Selection_Event /= null then
         Control.On_User_List_Selection_Event
           (Base_Window_Type'Class (Control),
            List_Type,
            Text);
      end if;
   end Fire_On_User_List_Selection;

   --------------------
   -- On_Dwell_Start --
   --------------------

   procedure On_Dwell_Start (Control : in out Scintilla_Type;
                             Pos     : in     Position)
   is
   begin
      Fire_On_Dwell_Start (Control, Pos);
   end On_Dwell_Start;

   ----------------------------
   -- On_Dwell_Start_Handler --
   ----------------------------

   procedure On_Dwell_Start_Handler (Control : in out Scintilla_Type;
                                     Handler : in     Position_Event)
   is
   begin
      Control.On_Dwell_Start_Event := Handler;
   end On_Dwell_Start_Handler;

   procedure Fire_On_Dwell_Start (Control : in out Scintilla_Type;
                                  Pos     : in     Position)
   is
      use GWindows.Base;
   begin
      if Control.On_Dwell_Start_Event /= null then
         Control.On_Dwell_Start_Event (Base_Window_Type'Class (Control),
                                            Pos);
      end if;
   end Fire_On_Dwell_Start;

   ------------------
   -- On_Dwell_End --
   ------------------

   procedure On_Dwell_End (Control : in out Scintilla_Type;
                           Pos     : in     Position)
   is
   begin
      Fire_On_Dwell_End (Control, Pos);
   end On_Dwell_End;

   --------------------------
   -- On_Dwell_End_Handler --
   --------------------------

   procedure On_Dwell_End_Handler (Control : in out Scintilla_Type;
                                   Handler : in     Position_Event)
   is
   begin
      Control.On_Dwell_End_Event := Handler;
   end On_Dwell_End_Handler;

   -----------------------
   -- Fire_On_Dwell_End --
   -----------------------

   procedure Fire_On_Dwell_End (Control : in out Scintilla_Type;
                                Pos     : in     Position)
   is
      use GWindows.Base;
   begin
      if Control.On_Dwell_End_Event /= null then
         Control.On_Dwell_End_Event (Base_Window_Type'Class (Control),
                                     Pos);
      end if;
   end Fire_On_Dwell_End;

begin
   LoadLibrary (GWindows.GStrings.To_GString_C ("scilexer.dll"));
end GWindows.Scintilla;
