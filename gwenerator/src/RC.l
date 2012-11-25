--------------------------------------------------------------------------
--  RC.l
--
--  Resource Compiler script lexer file (AFLEX)
--
--  Copyright (c) Gautier de Montmollin 2008..2012
--  SWITZERLAND
--
--  Permission is hereby granted, free of charge, to any person obtaining a copy
--  of this software and associated documentation files (the "Software"), to deal
--  in the Software without restriction, including without limitation the rights
--  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
--  copies of the Software, and to permit persons to whom the Software is
--  furnished to do so, subject to the following conditions:

--  The above copyright notice and this permission notice shall be included in
--  all copies or substantial portions of the Software.

--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
--  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
--  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
--  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
--  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
--  THE SOFTWARE.

-- NB: this is the MIT License, as found 28-Jul-2008 on the site
-- http://www.opensource.org/licenses/mit-license.php
--------------------------------------------------------------------------
--
-- Change log:
--
-- 30-Apr-2009 GdM: Some additions for matching ResEdit 1.4.4.19 suppl. tags
-- ...
-- 28-Jul-2008 GdM: Created
--

INSTANCE    [^0-9{}\'\"\\ \t\n\f[:cntrl:]][^{}\'\"\\ \t\n\f[:cntrl:]]*
LETTER [a-zA-Z_]
DIGIT [0-9]
IDCHAR [0-9a-zA-Z_]
GUIL [\"]
NOTGUIL [^\"]
STAR [*]
NOTSTAR [^*]
SLASH [/]
LBRACE [{]
RBRACE [}]
NOTSLASHSTAR [^/*]
NOTGREATER [^>]

%%

-- /* ... */ -style comments
{SLASH}{STAR}(({NOTSTAR}*)|({STAR}+{NOTSLASHSTAR}))*({STAR}+{SLASH}) {
         RC_Help.RC_Comment(YYText(3..YYLength-2));
         }

-- // -style comment -> skip rest of line.
"//".*\n                 {
         RC_Help.RC_Comment(YYText);
                      linenum:= linenum + 1;
         }

"|"              { return BAR_t; }
,                { return COMMA_t; }
{LBRACE}         { return LBRACE_t; }
{RBRACE}         { return RBRACE_t; }
"#include"       { return C_INCLUDE_t; }
NOT              { return NOT_t; }

-----------
-- Noise --
-----------

"#define".*\n    { null; } -- skip rest of line - was { return C_CRAP_t; }
"#undef".*\n     { null; }
"#if".*\n        { null; }
"#ifdef".*\n     { null; }
"#ifndef".*\n    { null; }
"#else".*\n      { null; }
"#elif".*\n      { null; }
"#endif".*\n     { null; }
"#pragma".*\n    { null; }

---------------
-- Resources --
---------------

DIALOG           { return DIALOG_t; }
DIALOGEX         { return DIALOGEX_t; }
MENU             { return MENU_t; }
DLGINCLUDE       { return DLGINCLUDE_t; }
TEXTINCLUDE      { return TEXTINCLUDE_t; }
GUIDELINES       { return GUIDELINES_t; }
DESIGNINFO       { return DESIGNINFO_t; }
RT_MANIFEST      { return RT_MANIFEST_t; }
DLGINIT          { return DLGINIT_t; }

----------------
-- Properties --
----------------

LOADONCALL       { return LOADONCALL_t; }
MOVEABLE         { return MOVEABLE_t; }
PURE             { return PURE_t; }
IMPURE           { return IMPURE_t; }
FIXED            { return FIXED_t; }
PRELOAD          { return PRELOAD_t; }
DISCARDABLE      { return DISCARDABLE_t; }

LANGUAGE         { return LANGUAGE_t; }
STYLE            { return STYLE_t; }
EXSTYLE          { return EXSTYLE_t; }
CAPTION          { return CAPTION_t; }
CLASS            { return CLASS_t; }
FONT             { return FONT_t; }
LTEXT            { return LTEXT_t; }
CTEXT            { return CTEXT_t; }
RTEXT            { return RTEXT_t; }

BEGIN            { return BEGIN_t; }
END              { return END_t; }

CONTROL          { return CONTROL_t; }
EDITTEXT         { return EDITTEXT_t; }
COMBOBOX         { return COMBOBOX_t; }
GROUPBOX         { return GROUPBOX_t; }
LISTBOX          { return LISTBOX_t; }

RADIOBUTTON      { return RADIOBUTTON_t; }
AUTORADIOBUTTON  { return AUTORADIOBUTTON_t; }

CHECKBOX         { return CHECKBOX_t; }
AUTOCHECKBOX     { return AUTOCHECKBOX_t; }
AUTO3STATE       { return AUTO3STATE_t; }
STATE3           { return STATE3_t; }

PUSHBUTTON       { return PUSHBUTTON_t; }
DEFPUSHBUTTON    { return DEFPUSHBUTTON_t; }

SCROLLBAR        { return SCROLLBAR_t; }

-----------
-- Menus --
-----------
CHECKED                        { return CHECKED_t; }
GRAYED                         { return GRAYED_t; }
HELP                           { return HELP_t; }
INACTIVE                       { return INACTIVE_t; }
MENUBARBREAK                   { return MENUBARBREAK_t; }
MENUBREAK                      { return MENUBREAK_t; }
MENUITEM                       { return MENUITEM_t; }
POPUP                          { return POPUP_t; }
SEPARATOR                      { return SEPARATOR_t; }

------------------
-- Accelerators --
------------------
ACCELERATORS                   { return ACCELERATORS_t; }
CHARACTERISTICS                { return CHARACTERISTICS_t; }
VERSION                        { return VERSION_t; }
ASCII                          { return ASCII_t; }
VIRTKEY                        { return VIRTKEY_t; }
NOINVERT                       { return NOINVERT_t; }
ALT                            { return ALT_t; }
SHIFT                          { return SHIFT_t; }

--------------
-- Versions --
--------------

VERSIONINFO                    { return VERSIONINFO_t; }
FILEVERSION.*\n                   { return FILEVERSION_t; }
PRODUCTVERSION.*\n                { return PRODUCTVERSION_t; }
FILEFLAGSMASK.*\n                 { return FILEFLAGSMASK_t; }
FILEFLAGS.*\n                     { return FILEFLAGS_t; }
FILEOS.*\n                        { return FILEOS_t; }
FILETYPE.*\n                      { return FILETYPE_t; }
FILESUBTYPE.*\n                   { return FILESUBTYPE_t; }
BLOCK                          { return BLOCK_t; }
VALUE                          { return VALUE_t; }

-------------------
-- Graphic items --
-------------------
ICON                           { return ICON_t; }
AVI                            { return AVI_t; }
BITMAP                         { return BITMAP_t; }
BITMAP_FONT                    { return BITMAP_FONT_t; }
CURSOR                         { return CURSOR_t; }
PNG                            { return PNG_t; }

--------------
-- Toolbars --
--------------
TOOLBAR                        { return TOOLBAR_t; }
BUTTON                         { return BUTTON_t; }
SEPARATOR                      { return SEPARATOR_t; }

STRINGTABLE                    { return STRINGTABLE_t; }

------------------
-- Window Class --
------------------
ANIMATE_CLASS      { return ANIMATE_CLASS_t; }
DATETIMEPICK_CLASS { return DATETIMEPICK_CLASS_t; }
MONTHCAL_CLASS     { return MONTHCAL_CLASS_t; }
PROGRESS_CLASS     { return PROGRESS_CLASS_t; }
TRACKBAR_CLASS     { return TRACKBAR_CLASS_t; }
UPDOWN_CLASS       { return UPDOWN_CLASS_t; }
WC_BUTTON          { return WC_BUTTON_t; }
WC_COMBOBOX        { return WC_COMBOBOX_t; }
WC_COMBOBOXEX      { return WC_COMBOBOXEX_t; }
WC_EDIT            { return WC_EDIT_t; }
WC_HEADER          { return WC_HEADER_t; }
WC_IPADDRESS       { return WC_IPADDRESS_t; }
WC_LINK            { return WC_LINK_t; }
WC_LISTBOX         { return WC_LISTBOX_t; }
WC_LISTVIEW        { return WC_LISTVIEW_t; }
WC_TABCONTROL      { return WC_TABCONTROL_t; }
WC_TREEVIEW        { return WC_TREEVIEW_t; }
WC_STATIC          { return WC_STATIC_t; }
STATIC             { return STATIC_t; }

------------
-- Styles --
------------

-- Window style
WS_BORDER                      { return WS_BORDER_t; }
WS_CAPTION                     { return WS_CAPTION_t; }
WS_CHILD                       { return WS_CHILD_t; }
WS_CHILDWINDOW                 { return WS_CHILDWINDOW_t; }
WS_CLIPCHILDREN                { return WS_CLIPCHILDREN_t; }
WS_CLIPSIBLINGS                { return WS_CLIPSIBLINGS_t; }
WS_DISABLED                    { return WS_DISABLED_t; }
WS_DLGFRAME                    { return WS_DLGFRAME_t; }
WS_GROUP                       { return WS_GROUP_t; }
WS_HSCROLL                     { return WS_HSCROLL_t; }
WS_MINIMIZEBOX                 { return WS_MINIMIZEBOX_t; }
WS_MAXIMIZEBOX                 { return WS_MAXIMIZEBOX_t; }
WS_OVERLAPPED                  { return WS_OVERLAPPED_t; }
WS_POPUP                       { return WS_POPUP_t; }
WS_SIZEBOX                     { return WS_SIZEBOX_t; }
WS_SYSMENU                     { return WS_SYSMENU_t; }
WS_TABSTOP                     { return WS_TABSTOP_t; }
WS_VISIBLE                     { return WS_VISIBLE_t; }
WS_VSCROLL                     { return WS_VSCROLL_t; }
WS_THICKFRAME                  { return WS_THICKFRAME_t; }

-- Dialog style
DS_3DLOOK                      { return DS_3DLOOK_t; }
DS_ABSALIGN                    { return DS_ABSALIGN_t; }
DS_CENTER                      { return DS_CENTER_t; }
DS_CENTERMOUSE                 { return DS_CENTERMOUSE_t; }
DS_CONTROL                     { return DS_CONTROL_t; }
DS_FIXEDSYS                    { return DS_FIXEDSYS_t; }
DS_LOCALEDIT                   { return DS_LOCALEDIT_t; }
DS_MODALFRAME                  { return DS_MODALFRAME_t; }
DS_NOIDLEMSG                   { return DS_NOIDLEMSG_t; }
DS_SETFOREGROUND               { return DS_SETFOREGROUND_t; }
DS_CONTEXTHELP                 { return DS_CONTEXTHELP_t; }
DS_SHELLFONT                   { return DS_SHELLFONT_t; }
DS_SETFONT                     { return DS_SETFONT_t; }
DS_SYSMODAL                    { return DS_SYSMODAL_t; }

-- Static styles
SS_BITMAP                      { return SS_BITMAP_t; }
SS_BLACKFRAME                  { return SS_BLACKFRAME_t; }
SS_BLACKRECT                   { return SS_BLACKRECT_t; }
SS_CENTERIMAGE                 { return SS_CENTERIMAGE_t; }
SS_ENDELLIPSIS                 { return SS_ENDELLIPSIS_t; }
SS_ETCHEDHORZ                  { return SS_ETCHEDHORZ_t; }
SS_ETCHEDVERT                  { return SS_ETCHEDVERT_t; }
SS_GRAYFRAME                   { return SS_GRAYFRAME_t; }
SS_GRAYRECT                    { return SS_GRAYRECT_t; }
SS_ICON                        { return SS_ICON_t; }
SS_LEFT                        { return SS_LEFT_t; }
SS_LEFTNOWORDWRAP              { return SS_LEFTNOWORDWRAP_t; }
SS_NOPREFIX                    { return SS_NOPREFIX_t; }
SS_NOTIFY                      { return SS_NOTIFY_t; }
SS_REALSIZEIMAGE               { return SS_REALSIZEIMAGE_t; }
SS_RIGHT                       { return SS_RIGHT_t; }
SS_RIGHTJUST                   { return SS_RIGHTJUST_t; }
SS_SIMPLE                      { return SS_SIMPLE_t; }
SS_SUNKEN                      { return SS_SUNKEN_t; }
SS_WHITERECT                   { return SS_WHITERECT_t; }
SS_WORDELLIPSIS                { return SS_WORDELLIPSIS_t; }

-- Edit styles
ES_AUTOHSCROLL                 { return ES_AUTOHSCROLL_t; }
ES_AUTOVSCROLL                 { return ES_AUTOVSCROLL_t; }
ES_CENTER                      { return ES_CENTER_t; }
ES_LEFT                        { return ES_LEFT_t; }
ES_MULTILINE                   { return ES_MULTILINE_t; }
ES_NOHIDESEL                   { return ES_NOHIDESEL_t; }
ES_NUMBER                      { return ES_NUMBER_t; }
ES_OEMCONVERT                  { return ES_OEMCONVERT_t; }
ES_PASSWORD                    { return ES_PASSWORD_t; }
ES_READONLY                    { return ES_READONLY_t; }
ES_RIGHT                       { return ES_RIGHT_t; }
ES_UPPERCASE                   { return ES_UPPERCASE_t; }
ES_WANTRETURN                  { return ES_WANTRETURN_t; }

-- Box/Button styles
BS_3STATE                      { return BS_3STATE_t; }
BS_AUTO3STATE                  { return BS_AUTO3STATE_t; }
BS_AUTOCHECKBOX                { return BS_AUTOCHECKBOX_t; }
BS_AUTORADIOBUTTON             { return BS_AUTORADIOBUTTON_t; }
BS_BITMAP                      { return BS_BITMAP_t; }
BS_BOTTOM                      { return BS_BOTTOM_t; }
BS_CENTER                      { return BS_CENTER_t; }
BS_CHECKBOX                    { return BS_CHECKBOX_t; }
BS_DEFPUSHBUTTON               { return BS_DEFPUSHBUTTON_t; }
BS_ICON                        { return BS_ICON_t; }
BS_FLAT                        { return BS_FLAT_t; }
BS_LEFT                        { return BS_LEFT_t; }
BS_LEFTTEXT                    { return BS_LEFTTEXT_t; }
BS_MULTILINE                   { return BS_MULTILINE_t; }
BS_NOTIFY                      { return BS_NOTIFY_t; }
BS_OWNERDRAW                   { return BS_OWNERDRAW_t; }
BS_PUSHBUTTON                  { return BS_PUSHBUTTON_t; }
BS_PUSHLIKE                    { return BS_PUSHLIKE_t; }
BS_RADIOBUTTON                 { return BS_RADIOBUTTON_t; }
BS_RIGHT                       { return BS_RIGHT_t; }
BS_RIGHTBUTTON                 { return BS_RIGHTBUTTON_t; }
BS_TEXT                        { return BS_TEXT_t; }
BS_TOP                         { return BS_TOP_t; }
BS_VCENTER                     { return BS_VCENTER_t; }

-- Combo-box styles
CBS_AUTOHSCROLL                { return CBS_AUTOHSCROLL_t; }
CBS_SIMPLE                     { return CBS_SIMPLE_t; }
CBS_DISABLENOSCROLL            { return CBS_DISABLENOSCROLL_t; }
CBS_DROPDOWN                   { return CBS_DROPDOWN_t; }
CBS_DROPDOWNLIST               { return CBS_DROPDOWNLIST_t; }
CBS_HASSTRINGS                 { return CBS_HASSTRINGS_t; }
CBS_NOINTEGRALHEIGHT           { return CBS_NOINTEGRALHEIGHT_t; }
CBS_OWNERDRAWFIXED             { return CBS_OWNERDRAWFIXED_t; }
CBS_SORT                       { return CBS_SORT_t; }
CBS_UPPERCASE                  { return CBS_UPPERCASE_t; }

-- Listbox styles
LBS_DISABLENOSCROLL            { return LBS_DISABLENOSCROLL_t; }
LBS_EXTENDEDSEL                { return LBS_EXTENDEDSEL_t; }
LBS_HASSTRINGS                 { return LBS_HASSTRINGS_t; }
LBS_MULTIPLESEL                { return LBS_MULTIPLESEL_t; }
LBS_MULTICOLUMN                { return LBS_MULTICOLUMN_t; }
LBS_NOINTEGRALHEIGHT           { return LBS_NOINTEGRALHEIGHT_t; }
LBS_NOREDRAW                   { return LBS_NOREDRAW_t; }
LBS_NOSEL                      { return LBS_NOSEL_t; }
LBS_NOTIFY                     { return LBS_NOTIFY_t; }
LBS_OWNERDRAWFIXED             { return LBS_OWNERDRAWFIXED_t; }
LBS_SORT                       { return LBS_SORT_t; }
LBS_USETABSTOPS                { return LBS_USETABSTOPS_t; }

-- Progress bar styles
PBS_SMOOTH                     { return PBS_SMOOTH_t; }
PBS_VERTICAL                   { return PBS_VERTICAL_t; }

-- Scrollbar styles
SBS_VERT           { return SBS_VERT_t; }

-- Trackbar styles
TBS_AUTOTICKS                  { return TBS_AUTOTICKS_t; }
TBS_BOTTOM                     { return TBS_BOTTOM_t; }
TBS_BOTH                       { return TBS_BOTH_t; }
TBS_NOTICKS                    { return TBS_NOTICKS_t; }
TBS_VERT                       { return TBS_VERT_t; }
TBS_TOP                        { return TBS_TOP_t; }
TBS_TOOLTIPS                   { return TBS_TOOLTIPS_t; }

-- Up-down styles
UDS_ALIGNRIGHT                 { return UDS_ALIGNRIGHT_t; }
UDS_AUTOBUDDY                  { return UDS_AUTOBUDDY_t; }
UDS_HOTTRACK                   { return UDS_HOTTRACK_t; }
UDS_HORZ                       { return UDS_HORZ_t; }
UDS_ARROWKEYS                  { return UDS_ARROWKEYS_t; }
UDS_WRAP                       { return UDS_WRAP_t; }
UDS_NOTHOUSANDS                { return UDS_NOTHOUSANDS_t; }
UDS_SETBUDDYINT                { return UDS_SETBUDDYINT_t; }

-- Listview styles
LVS_AUTOARRANGE                { return LVS_AUTOARRANGE_t; }
LVS_ALIGNLEFT                  { return LVS_ALIGNLEFT_t; }
LVS_EDITLABELS                 { return LVS_EDITLABELS_t; }
LVS_ICON                       { return LVS_ICON_t; }
LVS_SMALLICON                  { return LVS_SMALLICON_t; }
LVS_LIST                       { return LVS_LIST_t; }
LVS_NOCOLUMNHEADER             { return LVS_NOCOLUMNHEADER_t; }
LVS_NOLABELWRAP                { return LVS_NOLABELWRAP_t; }
LVS_NOSORTHEADER               { return LVS_NOSORTHEADER_t; }
LVS_REPORT                     { return LVS_REPORT_t; }
LVS_SHAREIMAGELISTS            { return LVS_SHAREIMAGELISTS_t; }
LVS_SHOWSELALWAYS              { return LVS_SHOWSELALWAYS_t; }
LVS_SINGLESEL                  { return LVS_SINGLESEL_t; }
LVS_SORTASCENDING              { return LVS_SORTASCENDING_t; }
LVS_SORTDESCENDING             { return LVS_SORTDESCENDING_t; }

-- Treeview styles
TVS_DISABLEDRAGDROP            { return TVS_DISABLEDRAGDROP_t; }
TVS_EDITLABELS                 { return TVS_EDITLABELS_t; }
TVS_FULLROWSELECT              { return TVS_FULLROWSELECT_t; }
TVS_INFOTIP                    { return TVS_INFOTIP_t; }
TVS_HASBUTTONS                 { return TVS_HASBUTTONS_t; }
TVS_HASLINES                   { return TVS_HASLINES_t; }
TVS_LINESATROOT                { return TVS_LINESATROOT_t; }
TVS_NOSCROLL                   { return TVS_NOSCROLL_t; }
TVS_NOTOOLTIPS                 { return TVS_NOTOOLTIPS_t; }
TVS_SHOWSELALWAYS              { return TVS_SHOWSELALWAYS_t; }
TVS_SINGLEEXPAND               { return TVS_SINGLEEXPAND_t; }
TVS_TRACKSELECT                { return TVS_TRACKSELECT_t; }
 
-- Date time picker styles
DTS_APPCANPARSE                { return DTS_APPCANPARSE_t; }
DTS_RIGHTALIGN                 { return DTS_RIGHTALIGN_t; }
DTS_UPDOWN                     { return DTS_UPDOWN_t; }

-- Month calendar styles
MCS_NOTODAY        { return MCS_NOTODAY_t; }

-- Tab Control Styles
TCS_HOTTRACK                   { return TCS_HOTTRACK_t; }
TCS_BUTTONS                    { return TCS_BUTTONS_t; }
TCS_MULTILINE                  { return TCS_MULTILINE_t; }

-- Grid Styles
GS_COLUMNLABELS                { return GS_COLUMNLABELS_t; }
GS_READONLY                    { return GS_READONLY_t; }


-- Extended styles
WS_EX_ACCEPTFILES              { return WS_EX_ACCEPTFILES_t; }
WS_EX_APPWINDOW                { return WS_EX_APPWINDOW_t; }
WS_EX_CLIENTEDGE               { return WS_EX_CLIENTEDGE_t; }
WS_EX_CONTEXTHELP              { return WS_EX_CONTEXTHELP_t; }
WS_EX_CONTROLPARENT            { return WS_EX_CONTROLPARENT_t; }
WS_EX_DLGMODALFRAME            { return WS_EX_DLGMODALFRAME_t; }
WS_EX_LEFTSCROLLBAR            { return WS_EX_LEFTSCROLLBAR_t; }
WS_EX_NOPARENTNOTIFY           { return WS_EX_NOPARENTNOTIFY_t; }
WS_EX_RIGHT                    { return WS_EX_RIGHT_t; }
WS_EX_RTLREADING               { return WS_EX_RTLREADING_t; }
WS_EX_STATICEDGE               { return WS_EX_STATICEDGE_t; }
WS_EX_TOOLWINDOW               { return WS_EX_TOOLWINDOW_t; }
WS_EX_TOPMOST                  { return WS_EX_TOPMOST_t; }
WS_EX_TRANSPARENT              { return WS_EX_TRANSPARENT_t; }
WS_EX_WINDOWEDGE               { return WS_EX_WINDOWEDGE_t; }

-- Misc --
IDC_STATIC                     { return IDC_STATIC_t; }
HIDC_STATIC                    { return HIDC_STATIC_t; } 

TRUE                { yylval.intval := 0; return NUMBER; }
FALSE               { yylval.intval := 1; return NUMBER; }

0                { yylval.intval   := 0;
                   yylval.floatval := 0.0;
                   return NUMBER;
                 }

-?[1-9][0-9]*    { begin
                     yylval.intval := Long_Long_Integer'Value(yytext);
                   exception
                     when Constraint_Error =>
                      -- declare
                      --   msg: constant String:=
                      --    "Error with integer ? [" & yytext & ']';
                      -- begin
                      --   Put_Line(Standard_Error, msg);
                      --   raise Constraint_Error with msg;
                      -- end;
                      yylval.intval := -1;
                      -- not nice at all, but a >2**31 number
                      -- seems to appear only at top (some checksum)
                   end;
                   yylval.floatval:= Long_Float( yylval.intval );
                   return NUMBER;}

0[1-7][0-7]*     { yylval.intval := Long_Long_Integer'Value("8#"&yytext&'#');
                   yylval.floatval:= Long_Float( yylval.intval );
                   return NUMBER;}

-?[0-9]*\.?[0-9]*([eE][+-]?[0-9]+)? {
                   yylval.floatval := Long_Float'Value(yytext);
                   return FLOAT_t;
                   }

0x[0-9A-Fa-f]+L?   { declare
                     hexa_c: String:= yytext;
                     hexa_last: Integer:= hexa_c'Last;
                   begin
                     if hexa_c(hexa_last)='L' then
                       hexa_last:= hexa_last - 1;
                     end if;
                     yylval.intval := Long_Long_Integer'Value("16#" & hexa_c(hexa_c'First+2 .. hexa_last) & '#');
                     yylval.floatval:= Long_Float( yylval.intval );
                     return NUMBER;
                   end;
                }

\"(\"\"|[^\n\"])*\"    {return RCString;} -- borrowed from ada95.l

-- Doesn't work with double quotes
-- \"({NOTGUIL})*\"    {return RCString;}

"<"{NOTGREATER}*">" {return INCString;}

[a-zA-Z_][a-zA-Z0-9_.]* { return IDENT_t; }


[ \t]+               { null; -- \t } ;
\n                {linenum:= linenum+1;}

[|]                { null; -- ??
                     -- C: return *yytext;
                   }
-- "("                { return '('; }
-- ")"                { return ')'; }
-- "}"                { return '}'; }
-- "{"                { return '{'; }

%%

with RC_Tokens; use RC_Tokens;
with RC_Help;   use RC_Help;

##
