--------------------------------------------------------------------------
--  RC.y
--
--  Resource Compiler script grammar file (AYACC)
--
--  Copyright (c) Gautier de Montmollin 2008 .. 2018
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
----------------------------------------------------------------------------
--
-- Change log (important changes only):
--
-- 15-May-2009 GdM: - constants generated for VersionInfo items
--                  - some additions for matching ResEdit 1.4.4.19 suppl. tags
-- 27-Nov-2008 GdM: For push buttons, both closing and non-closing are created,
--                    but only one is shown (it can be reversed any time)
--  5-Sep-2008 GdM: Include file with symbols processed; minor changes
--  5-Aug-2008 GdM: Menus implemented
--  1-Aug-2008 GdM: Accepts .rc files from Visual Studio 2008
-- 30-Jul-2008 GdM: Dialogs: most standard controls and common controls implemented
-- 29-Jul-2008 GdM: First working version (.rc -> working package)
-- 28-Jul-2008 GdM: Created
--

%token NUMBER
%token FLOAT_t, COMMA_t, BAR_t, LBRACE_t, RBRACE_t, NOT_t
       C_INCLUDE_t

-----------
-- Items --
-----------

%token DIALOG_t, DIALOGEX_t, CONTROL_t, CAPTION_t,
       BEGIN_t, END_t, LANGUAGE_t, STYLE_t, EXSTYLE_t,
       FONT_t, CLASS_t
%token PURE_t, IMPURE_t, LOADONCALL_t
       DISCARDABLE_t, MOVEABLE_t, PRELOAD_t, FIXED_t
%token DLGINCLUDE_t, TEXTINCLUDE_t,
       GUIDELINES_t, DESIGNINFO_t,
       RT_MANIFEST_t, DLGINIT_t
%token MENU_t, POPUP_t, MENUITEM_t, SEPARATOR_t,
       GRAYED_t, INACTIVE_t, CHECKED_t,
       HELP_t, MENUBARBREAK_t, MENUBREAK_t
%token ACCELERATORS_t, CHARACTERISTICS_t, VERSION_t, ASCII_t, VIRTKEY_t,
       NOINVERT_t, ALT_t, SHIFT_t
%token ICON_t, BITMAP_t, BITMAP_FONT_t, CURSOR_t,
       PNG_t, AVI_t
%token VERSIONINFO_t, FILEVERSION_t, PRODUCTVERSION_t, FILEFLAGSMASK_t,
       FILEFLAGS_t, FILEOS_t, FILETYPE_t, FILESUBTYPE_t, BLOCK_t,
       VALUE_t
%token TOOLBAR_t, BUTTON_t, SEPARATOR_t

--------------
-- Controls --
--------------
%token EDITTEXT_t, LTEXT_t, CTEXT_t, RTEXT_t,
       COMBOBOX_t, GROUPBOX_t, LISTBOX_t,
       PUSHBUTTON_t, DEFPUSHBUTTON_t,
       RADIOBUTTON_t, AUTORADIOBUTTON_t,
       CHECKBOX_t, STATE3_t, AUTOCHECKBOX_t, AUTO3STATE_t
       SCROLLBAR_t

------------------
-- Window Class --
------------------
%token ANIMATE_CLASS_t, DATETIMEPICK_CLASS_t, HOTKEY_CLASS_t, LINK_CLASS_t,
       MONTHCAL_CLASS_t, NATIVEFNTCTL_CLASS_t, PROGRESS_CLASS_t, REBARCLASSNAME_t
       STANDARD_CLASSES_t,
       STATUSCLASSNAME_t,
       TOOLBARCLASSNAME_t, TOOLTIPS_CLASS_t, TRACKBAR_CLASS_t, UPDOWN_CLASS_t,
       WC_BUTTON_t, WC_COMBOBOX_t, WC_COMBOBOXEX_t, WC_EDIT_t,
       WC_HEADER_t, WC_LISTBOX_t, WC_IPADDRESS_t, WC_LINK_t
       WC_LISTVIEW_t, WC_NATIVEFONTCTL_t, WC_PAGESCROLLER_t
       WC_SCROLLBAR_t, WC_STATIC_t, STATIC_t, WC_TABCONTROL_t
       WC_TREEVIEW_t

------------
-- Styles --
------------

-- Window style
%token WS_BORDER_t, WS_CAPTION_t, WS_VISIBLE_t,
       WS_DLGFRAME_t, WS_POPUP_t, WS_SYSMENU_t,
       WS_HSCROLL_t, WS_VSCROLL_t, WS_TABSTOP_t,
       WS_GROUP_t, WS_DISABLED_t,
       WS_MINIMIZEBOX_t, WS_MAXIMIZEBOX_t,
       WS_THICKFRAME_t,
       WS_CHILD_t, WS_CHILDWINDOW_t,
       WS_CLIPSIBLINGS_t, WS_CLIPCHILDREN_t
       WS_SIZEBOX_t, WS_OVERLAPPED_t
-- Dialog style
%token DS_3DLOOK_t, DS_CENTER_t,
       DS_MODALFRAME_t, DS_SYSMODAL_t,
       DS_SHELLFONT_t, DS_SETFONT_t, DS_FIXEDSYS_t,
       DS_NOIDLEMSG_t, DS_CENTERMOUSE_t,
       DS_LOCALEDIT_t, DS_SETFOREGROUND_t, DS_CONTEXTHELP_t,
       DS_CONTROL_t, DS_ABSALIGN_t
-- Static styles
%token SS_NOPREFIX_t, SS_SUNKEN_t, SS_BLACKFRAME_t,
       SS_CENTERIMAGE_t, SS_BITMAP_t, SS_ICON_t, SS_SIMPLE_t,
       SS_LEFTNOWORDWRAP_t, SS_ENDELLIPSIS_t,
       SS_BLACKRECT_t, SS_GRAYRECT_t, SS_WHITERECT_t,
       SS_REALSIZEIMAGE_t, SS_GRAYFRAME_t
       SS_LEFT_t, SS_RIGHT_t, SS_RIGHTJUST_t
       SS_NOTIFY_t, SS_ETCHEDHORZ_t, SS_ETCHEDVERT_t,
       SS_WORDELLIPSIS_t
-- Edit styles
%token ES_MULTILINE_t, ES_READONLY_t,
       ES_AUTOHSCROLL_t, ES_AUTOVSCROLL_t,
       ES_WANTRETURN_t, ES_NUMBER_t,
       ES_LEFT_t, ES_CENTER_t, ES_RIGHT_t,
       ES_PASSWORD_t, ES_UPPERCASE_t,
       ES_OEMCONVERT_t, ES_NOHIDESEL_t
-- Box/button styles
%token BS_LEFTTEXT_t, BS_AUTORADIOBUTTON_t,  BS_3STATE_t
       BS_CHECKBOX_t, BS_AUTOCHECKBOX_t, BS_BITMAP_t, BS_OWNERDRAW_t,
       BS_BOTTOM_t, BS_FLAT_t, BS_LEFT_t, BS_RIGHT_t, BS_CENTER_t, BS_VCENTER_t,
       BS_PUSHLIKE_t, BS_TOP_t, BS_MULTILINE_t,
       BS_DEFPUSHBUTTON_t, BS_PUSHBUTTON_t, BS_RADIOBUTTON_t, BS_AUTO3STATE_t,
       BS_TEXT_t, BS_RIGHTBUTTON_t, BS_ICON_t,
       BS_NOTIFY_t, 
       BS_SPLITBUTTON_t, BS_DEFSPLITBUTTON_t,
       BS_COMMANDLINK_t, BS_DEFCOMMANDLINK_t
-- Combo-box styles
%token CBS_SIMPLE_t, CBS_DROPDOWN_t, CBS_DROPDOWNLIST_t,
       CBS_SORT_t, CBS_HASSTRINGS_t, CBS_AUTOHSCROLL_t,
       CBS_DISABLENOSCROLL_t, CBS_OWNERDRAWFIXED_t,
       CBS_UPPERCASE_t, CBS_NOINTEGRALHEIGHT_t
-- Listbox styles
%token LBS_SORT_t, LBS_MULTIPLESEL_t, LBS_MULTICOLUMN_t,
       LBS_NOINTEGRALHEIGHT_t, LBS_USETABSTOPS_t,
       LBS_NOTIFY_t, LBS_NOREDRAW_t, LBS_EXTENDEDSEL_t,
       LBS_DISABLENOSCROLL_t, LBS_NOSEL_t,
       LBS_OWNERDRAWFIXED_t, LBS_HASSTRINGS_t
-- Progress bar styles
%token PBS_VERTICAL_t, PBS_SMOOTH_t
-- Scrollbar styles
%token SBS_VERT_t
-- Trackbar styles
%token TBS_NOTICKS_t, TBS_AUTOTICKS_t, TBS_VERT_t, TBS_TOP_t,
       TBS_BOTTOM_t, TBS_TOOLTIPS_t, TBS_BOTH_t
-- Up-down styles
%token UDS_HORZ_t, UDS_ARROWKEYS_t, UDS_WRAP_t, UDS_NOTHOUSANDS_t,
       UDS_SETBUDDYINT_t, UDS_ALIGNRIGHT_t, UDS_AUTOBUDDY_t,
       UDS_HOTTRACK_t
-- Listview styles
%token LVS_ALIGNLEFT_t, LVS_ICON_t, LVS_SMALLICON_t, LVS_REPORT_t,
       LVS_SHOWSELALWAYS_t, LVS_SORTASCENDING_t, LVS_SORTDESCENDING_t,
       LVS_AUTOARRANGE_t, LVS_NOCOLUMNHEADER_t, LVS_NOSORTHEADER_t, LVS_LIST_t,
       LVS_SINGLESEL_t, LVS_EDITLABELS_t, LVS_NOLABELWRAP_t,
       LVS_SHAREIMAGELISTS_t
-- Treeview styles
%token TVS_INFOTIP_t, TVS_NOSCROLL_t, TVS_HASLINES_t,
       TVS_SHOWSELALWAYS_t, TVS_HASBUTTONS_t, TVS_LINESATROOT_t,
       TVS_NOTOOLTIPS_t, TVS_EDITLABELS_t, TVS_DISABLEDRAGDROP_t,
       TVS_SINGLEEXPAND_t, TVS_TRACKSELECT_t, TVS_FULLROWSELECT_t
-- Date time picker styles
%token DTS_RIGHTALIGN_t, DTS_APPCANPARSE_t, DTS_UPDOWN_t
-- Month calendar styles
%token MCS_NOTODAY_t
-- Tab Control Styles
%token TCS_HOTTRACK_t, TCS_BUTTONS_t, TCS_MULTILINE_t
-- Grid Styles
%token GS_COLUMNLABELS_t, GS_READONLY_t
-- Extended styles
%token WS_EX_CLIENTEDGE_t, WS_EX_STATICEDGE_t, WS_EX_ACCEPTFILES_t,
       WS_EX_APPWINDOW_t, WS_EX_TOOLWINDOW_t,
       WS_EX_CONTROLPARENT_t, WS_EX_NOPARENTNOTIFY_t,
       WS_EX_CONTEXTHELP_t, WS_EX_LEFT_t, WS_EX_RIGHT_t, WS_EX_TRANSPARENT_t,
       WS_EX_TOPMOST_t, WS_EX_DLGMODALFRAME_t,
       WS_EX_WINDOWEDGE_t, WS_EX_LEFTSCROLLBAR_t, WS_EX_RTLREADING_t

-- Misc --
%token IDC_STATIC_t, HIDC_STATIC_t

%token IDENT_t, STRINGTABLE_t
%token RCString, INCString, CONSUME_EOL_t

%start rc

{

  type const_type is (
    intval,
    floatval,
    doubleval,
    stringval,
    any_type
  );

  type YYSType is record
     text    : String(1..80);
     length  : Natural := 0;
     vartype : const_type;
     intval  : Long_Long_Integer;
     floatval: Long_Float;
  end record;

}

%%

rc      : RC_items {RC_Help.YY_ACCEPT;}
        | error    {RC_Help.YY_ABORT;}
        ;

RC_items  : RC_item
          | RC_item RC_items;

RC_item   : dialog
          | menu
          | accelerator
          | graphic
          | version_info
          | string_table
          | toolbar
          | language
          | dlginclude
          | textinclude
          | include
          | guidelines
          | manifest
          | dialog_info
          ;

-------------------
-- Window styles --
-------------------

ws_styles_optional :
          -- nothing
          | COMMA_t ws_style_list
          ;

ws_style_list:
            ws_style
          | ws_style BAR_t ws_style_list
          ;

ws_style  :
                  WS_HSCROLL_t
          |       WS_VSCROLL_t
          |       WS_BORDER_t
            { style_switch(simple_border):= True; }
          | NOT_t WS_BORDER_t
            { style_switch(simple_border):= False; }
          |       WS_VISIBLE_t
            { style_switch(hidden):= False; }
          | NOT_t WS_VISIBLE_t
            { style_switch(hidden):= True; }
          |       WS_CAPTION_t
          |       WS_DLGFRAME_t
          |       WS_POPUP_t
          |       WS_SYSMENU_t
                  { style_switch(sys_menu):= True; }
          |       WS_TABSTOP_t
          | NOT_t WS_TABSTOP_t
          |       WS_GROUP_t
          | NOT_t WS_GROUP_t
          |       WS_DISABLED_t
                  { style_switch(disabled):= True; }
          |       WS_MINIMIZEBOX_t
          |       WS_MAXIMIZEBOX_t
          |       WS_THICKFRAME_t
          |       WS_CHILD_t
          |       WS_CHILDWINDOW_t
          |       WS_CLIPSIBLINGS_t
          |       WS_CLIPCHILDREN_t
          |       WS_SIZEBOX_t
          |       WS_OVERLAPPED_t
          ;

------------
-- Dialog --
------------
-- http://msdn.microsoft.com/en-us/library/aa381002(VS.85).aspx

dialog    :    RC_Ident
               { if anonymous_item then
                   anonymous_dialog_counter:=
                     anonymous_dialog_counter+1;
                   last_dialog_ident:= U("Dialog_" &
                     Trim(Integer'Image(anonymous_dialog_counter),both));
                 else
                   last_dialog_ident:= last_ident;
                 end if;
               }
               dialog_hint
               {
                 Open_if_separate(S(last_dialog_ident));
                 Ada_Put_Line(to_spec,
                   "  type " & S(last_dialog_ident) &
                   "_Type is new Window_Type with record"
                 );
                 Ada_New_Line(to_spec);
                 Ada_New_Line(to_body);
                 last_caption:= U("""""");
                 style_switch:= (others => False); -- Reset all style switches
                 static_counter:= 0;
               }
               properties
               rect
               {
                 last_dialog_rect:= last_rect;
               }
               dlg_optional_statements
               {
                 last_dialog_caption:= last_caption;
                 dialog_style_switch:= style_switch;
                 Ada_Proc_Dialog(
                    to_body,
                    S(last_dialog_ident) & "_Type",
                    S(last_dialog_caption)
                 );
               }

               BEGIN_block
               { empty_dialog_record:= True; }
               dlg_items
               END_block
               { Ada_Proc_Dialog(
                    to_spec,
                    S(last_dialog_ident) & "_Type",
                    S(last_dialog_caption)
                 );
                 Ada_Put_Line(to_spec, ";");
                 Ada_New_Line(to_spec);
                 Ada_Put_Line(to_body,
                   "  end Create_Contents;  --  " &
                   S(last_dialog_ident) & "_Type" );
                 Close_if_separate(S(last_dialog_ident));
               }
          ;

dialog_hint:
            DIALOG_t
          | DIALOGEX_t
          ;

dlg_optional_statements
          :
          | dlg_optional_statement dlg_optional_statements
          ;

dlg_optional_statement
          : caption
          | font
          | language
          | dlg_styles
          | dlg_ex_styles
          | menu_for_dialog
          | class_for_dialog
          ;

language :  LANGUAGE_t
            RC_Ident -- lang
            COMMA_t
            RC_Ident -- sublang
          ;

font      : FONT_t
            { style_switch(shell_font):= True; }
            NUMBER -- size
            COMMA_t
            RCSTRING -- face
            font_ext
            ;

font_ext  :
          | COMMA_t NUMBER -- weight
            COMMA_t NUMBER -- italic
            COMMA_t NUMBER -- charset
          ;

properties:               -- !! right name for it ?
          | property properties
          ;

property  : PURE_t
          | IMPURE_t
          | LOADONCALL_t
          | DISCARDABLE_t
          | MOVEABLE_t
          | PRELOAD_t
          | FIXED_t
          ;

dlg_styles: STYLE_t
            dlg_style_list
          ;

dlg_style_list:
            dlg_style
          | dlg_style BAR_t dlg_style_list
          ;

dlg_style : DS_3DLOOK_t
          | DS_CENTER_t
          | DS_MODALFRAME_t
          | DS_SYSMODAL_t
          | DS_SETFONT_t     { style_switch(shell_font):= True; }
          | DS_SHELLFONT_t   { style_switch(shell_font):= True; }
          | DS_FIXEDSYS_t    { style_switch(shell_font):= True; }
          | DS_NOIDLEMSG_t
          | DS_CENTERMOUSE_t
          | DS_LOCALEDIT_t
          | DS_SETFOREGROUND_t
          | DS_CONTEXTHELP_t
          | DS_CONTROL_t
          | DS_ABSALIGN_t
          | ws_style
          | NUMBER
          ;

dlg_ex_styles:
            EXSTYLE_t
            ex_style_list
          ;

menu_for_dialog :
            MENU_t
            RC_Ident
            ;

class_for_dialog :
            CLASS_t
            class
            ;

caption   : CAPTION_t
            RCSTRING
            { last_caption:= U(yytext); }
          ;

dlg_items : -- empty
          | {
              style_switch:= (others => False); -- Reset all style switches
              last_text:= U("""""");
            }
            dlg_item
            dlg_items
          ;

dlg_item  : control
          | edittext    {empty_dialog_record:= False;}
          | label
          | combobox    {empty_dialog_record:= False;}
          | groupbox    {empty_dialog_record:= False;}
          | listbox     {empty_dialog_record:= False;}
          | checkbox    {empty_dialog_record:= False;}
          | pushbutton  {empty_dialog_record:= False;}
          | radiobutton {empty_dialog_record:= False;}
          | scrollbar   {empty_dialog_record:= False;}
          | icon        {empty_dialog_record:= False;}
          ;

-------------
-- Control --
-------------
-- http://msdn.microsoft.com/en-us/library/aa380911(VS.85).aspx
-- CONTROL text, id, class, style, x, y, width, height [, extended-style]

control   :    CONTROL_t
               { control:= unknown;
                 Reset_control_styles;
               }
               control_text
               { last_control_text:= U(yytext); }
               COMMA_t
               RC_Ident -- id
               { Insert_last_symbol;
               }
               COMMA_t
               class
               { last_class:= U(yytext); }
               COMMA_t
               ctrl_style_list
               COMMA_t
               rect
               ex_styles_optional
               { Ada_untyped_control; }
          ;

control_text :
          RCString -- correct syntax (MSDN)
        | RC_Ident -- image reference for WC_Static
        ;


class : window_class
      | RCSTRING
        { Identify_control_class(yytext); }
      ;


------------------
-- Window Class --
------------------
-- http://msdn.microsoft.com/en-us/library/bb775491(VS.85).aspx

window_class:
        ANIMATE_CLASS_t
        -- Creates animation controls (silently display an AVI clip).
      | DATETIMEPICK_CLASS_t
        -- Creates date and time picker controls.
        { control:= date_time;
		}
      | HOTKEY_CLASS_t -- Creates hot key controls.
      | LINK_CLASS_t
      | MONTHCAL_CLASS_t
        -- Creates month calendar controls.
        { control:= calendar;
		}
      | NATIVEFNTCTL_CLASS_t -- Creates native font controls. These controls are used with native fonts.
      | PROGRESS_CLASS_t
	    -- Creates progress bars.
        { control:= progress;
          Control_Direction:= Horizontal;
		}
      | REBARCLASSNAME_t -- Creates rebar controls. These controls act as a container for child windows.
      | STANDARD_CLASSES_t
      | STATUSCLASSNAME_t -- Creates status windows.
      | TOOLBARCLASSNAME_t
      | TOOLTIPS_CLASS_t
      | TRACKBAR_CLASS_t
        -- Creates trackbars (select from a range of values by moving a slider).
        { control:= track_bar;
          Trackbar_Control_Ticks:= No_Ticks;
          Control_Direction:= Horizontal;
        }
      | UPDOWN_CLASS_t
        { control:= up_down;
          Control_Direction:= Vertical;
		}
      | WC_BUTTON_t
      | WC_COMBOBOX_t
      | WC_COMBOBOXEX_t
      | WC_EDIT_t
      | WC_HEADER_t -- Creates header controls (headings at the top of columns).
      | WC_LISTBOX_t
      | WC_IPADDRESS_t -- Creates IP address controls.
      | WC_LINK_t          -- Creates SysLink controls. These controls contain hypertext links.
      | WC_LISTVIEW_t
        -- Creates list-view controls.
        { control:= list_view; }
      | WC_NATIVEFONTCTL_t -- Creates native font controls (invisible)
      | WC_PAGESCROLLER_t  -- Creates pager controls (contain and scroll another window).
      | WC_SCROLLBAR_t     -- Creates scrollbar controls (scroll the contents of a window).
      | WC_STATIC_t
        -- Creates static controls. These controls contain noneditable text.
        -- ResEdit seems to use WC_STATIC for pictures; some sources use STATIC
        { control:= static; }
      |    STATIC_t
        -- Creates static controls. These controls contain noneditable text.
        -- ResEdit seems to use WC_STATIC for pictures; some sources use STATIC
        { control:= static; }
      | WC_TABCONTROL_t
        -- Creates tab controls
        { control:= tab_control; }
      | WC_TREEVIEW_t
        -- Creates tree-view controls.
        { control:= tree_view; }
      ;

ctrl_style_list:
            ctrl_style
          | ctrl_style BAR_t ctrl_style_list
          ;

ctrl_style: ws_style
          | ss_style
          | es_style_only
          | bs_style_only
          | cbs_style_only
          | PBS_VERTICAL_t
            { Control_Direction:= Vertical; }
          | PBS_SMOOTH_t
            { style_switch(smooth):= True; }
          | TBS_VERT_t
            { Control_Direction:= Vertical; }
          | TBS_TOP_t
            { Trackbar_Control_Ticks:= Top_Ticks; }
          | TBS_BOTTOM_t
            { Trackbar_Control_Ticks:= Bottom_Ticks; }
          | TBS_NOTICKS_t
            -- ignore (default); ResEdit combines with others...
          | TBS_AUTOTICKS_t
		  | TBS_TOOLTIPS_t
            { style_switch(tips):= True; }
          | TBS_BOTH_t
          | UDS_HORZ_t
            { Control_Direction:= Horizontal; }
	      | UDS_ARROWKEYS_t
            { style_switch(keys):= True; }
          | UDS_WRAP_t
            { style_switch(wrap):= True; }
          | UDS_NOTHOUSANDS_t
            { style_switch(no_1000):= True; }
          | UDS_SETBUDDYINT_t
          | UDS_ALIGNRIGHT_t
          | UDS_AUTOBUDDY_t
          | UDS_HOTTRACK_t
          | LVS_ALIGNLEFT_t
            { lv_align := GWindows.Common_Controls.Align_Left; }
          | LVS_EDITLABELS_t
          | LVS_ICON_t
            { lv_type:= GWindows.Common_Controls.Icon_View; }
          | LVS_SMALLICON_t
            { lv_type:= GWindows.Common_Controls.Small_Icon_View; }
          | LVS_LIST_t
            { lv_type:= GWindows.Common_Controls.List_View; }
          | LVS_REPORT_t
            { lv_type:= GWindows.Common_Controls.Report_View; }
          | LVS_SHOWSELALWAYS_t
          | LVS_SORTASCENDING_t
            { lv_sort:= GWindows.Common_Controls.Sort_Ascending; }
          | LVS_SORTDESCENDING_t
            { lv_sort:= GWindows.Common_Controls.Sort_Descending; }
          | LVS_AUTOARRANGE_t
            { lv_auto_arrange:= True; }
          | LVS_NOCOLUMNHEADER_t
          | LVS_NOSORTHEADER_t
          | LVS_NOLABELWRAP_t
          | LVS_SINGLESEL_t
            { lv_select:= GWindows.Common_Controls.Single; }          
          | LVS_SHAREIMAGELISTS_t
          | TVS_INFOTIP_t
            { style_switch(tips):= True; }
          | TVS_NOSCROLL_t
          | TVS_HASLINES_t
            { style_switch(has_lines):= True; }
          | TVS_SHOWSELALWAYS_t
          | TVS_HASBUTTONS_t
            { style_switch(has_buttons):= True; }
          | TVS_LINESATROOT_t
            { style_switch(lines_at_root):= True; }
          | TVS_NOTOOLTIPS_t
          | TVS_EDITLABELS_t
          | TVS_DISABLEDRAGDROP_t
          | TVS_SINGLEEXPAND_t
            { style_switch(single_expand):= True; }
          | TVS_TRACKSELECT_t
          | TVS_FULLROWSELECT_t
          | DTS_APPCANPARSE_t
          | DTS_RIGHTALIGN_t
          | DTS_UPDOWN_t
          | MCS_NOTODAY_t
          | TCS_HOTTRACK_t
          | TCS_BUTTONS_t
          | TCS_MULTILINE_t
          | GS_COLUMNLABELS_t
          | GS_READONLY_t      { style_switch(read_only):= True; }
          | NUMBER
          ;

-------------------
-- Extend styles --
-------------------

ex_styles_optional :
          -- nothing
          | COMMA_t ex_style_list
          | COMMA_t ex_style_list
              COMMA_t HIDC_STATIC_t
              -- ^ one more undocumented M$ fiddling (help id ?)...
          ;

ex_style_list:
            ex_style
          | ex_style BAR_t ex_style_list
          ;

ex_style  : ex_style_only
          | NUMBER
          ;

ex_style_only
          : WS_EX_CLIENTEDGE_t
            { style_switch(fully_sunken):= True; }
          | WS_EX_STATICEDGE_t
            { style_switch(half_sunken):= True; }
          | WS_EX_WINDOWEDGE_t
          | WS_EX_CONTROLPARENT_t
          | WS_EX_ACCEPTFILES_t
          | WS_EX_APPWINDOW_t
          | WS_EX_TOOLWINDOW_t
          | WS_EX_NOPARENTNOTIFY_t
          | WS_EX_CONTEXTHELP_t
          | WS_EX_LEFT_t   -- exported everywhere by ResEdit 1.6.x (2014)
          | WS_EX_RIGHT_t
          | WS_EX_LEFTSCROLLBAR_t
          | WS_EX_TOPMOST_t
          | WS_EX_TRANSPARENT_t
          | WS_EX_DLGMODALFRAME_t
          | WS_EX_RTLREADING_t
          ;

-------------------
-- Static styles --
-------------------

ss_style  : SS_NOPREFIX_t
          | SS_SUNKEN_t
            { style_switch(half_sunken):= True; }
          | SS_BLACKFRAME_t
          | SS_CENTERIMAGE_t
            { style_switch(center_image):= True; }
          | SS_BITMAP_t
            { control:= bitmap; -- overrides the "control:= static;" of WC_STATIC
            }
          | SS_ICON_t
            { control:= icon;   -- overrides the "control:= static;" of WC_STATIC
            }
          | SS_REALSIZEIMAGE_t
            { style_switch(real_size_image):= True; }
          | SS_SIMPLE_t
          | SS_LEFTNOWORDWRAP_t
          | SS_LEFT_t
          | SS_RIGHT_t
          | SS_RIGHTJUST_t
            { style_switch(right_justify):= True; }
          | SS_BLACKRECT_t
          | SS_GRAYRECT_t
          | SS_GRAYFRAME_t
          | SS_WHITERECT_t
            { style_switch(whiterect):= True; }
          | SS_ENDELLIPSIS_t
          | SS_NOTIFY_t
          | SS_ETCHEDHORZ_t
          | SS_ETCHEDVERT_t
          | SS_WORDELLIPSIS_t
          ;


----------------
-- Edit boxes --
----------------

edittext  : EDITTEXT_t
            { style_switch(simple_border):= True;  
              --  By default in GWindows (and elsewhere), edit boxes have borders.
              --  ResEdit adds the style NOT WS_BORDER to hide the border
            }
            edit_text
            ctrl_properties_notext
            es_styles_optional -- also with optional extended styles
            {
              Ada_edit_control;
            }
            ;

-- Windres (.res -> .rc) outputs a text (seems wrong)
edit_text :
          | RCString COMMA_t
          ;

es_styles_optional :
          -- nothing
          | COMMA_t
            es_style_list
            ex_styles_optional
          ;

es_style_list:
            es_style
          | es_style BAR_t es_style_list
          ;

es_style  : es_style_only
          | ss_style
          | ws_style
          | NUMBER
          ;

es_style_only :
            ES_MULTILINE_t   { style_switch(multi_line):= True; }
          | ES_READONLY_t    { style_switch(read_only):= True; }
          | ES_AUTOHSCROLL_t { style_switch(auto_h_scroll):= True; }
          | ES_AUTOVSCROLL_t { style_switch(auto_v_scroll):= True; }
          | ES_WANTRETURN_t
          | ES_NUMBER_t
          | ES_PASSWORD_t
          | ES_UPPERCASE_t
          | ES_LEFT_t
          | ES_CENTER_t
          | ES_RIGHT_t
          | ES_OEMCONVERT_t
          | ES_NOHIDESEL_t
          ;


label     : pos_hint
            ctrl_properties
            es_styles_optional -- also with optional extended styles
            { Ada_label_control; }
            ;

pos_hint : LTEXT_t {last_alignment:= GWindows.Static_Controls.Left;   }
         | CTEXT_t {last_alignment:= GWindows.Static_Controls.Center; }
         | RTEXT_t {last_alignment:= GWindows.Static_Controls.Right;  }
         ;

-----------------
-- Combo-boxes --
-----------------

combobox  : COMBOBOX_t
            { combo:= no_drop; }
            ctrl_properties_notext
            cbs_styles_optional -- also with optional extended styles
            { Ada_combo_control; }
            ;

cbs_styles_optional :
          -- nothing
          | COMMA_t
            cbs_style_list
            ex_styles_optional
          ;

cbs_style_list:
            cbs_style
          | cbs_style BAR_t cbs_style_list
          ;

cbs_style : cbs_style_only
          | ws_style
          | NUMBER
          ;

cbs_style_only
          : CBS_SIMPLE_t          { combo:= no_drop; }
          | CBS_DROPDOWN_t        { combo:= drop_down; }
          | CBS_DROPDOWNLIST_t    { combo:= drop_down_list; }
          | CBS_SORT_t            { style_switch(sort):= True; }
          | CBS_HASSTRINGS_t
          | CBS_UPPERCASE_t
          | CBS_AUTOHSCROLL_t
          | CBS_DISABLENOSCROLL_t
          | CBS_OWNERDRAWFIXED_t
          | CBS_NOINTEGRALHEIGHT_t
          ;

-----------------
-- Group boxes --
-----------------

groupbox  : GROUPBOX_t
            ctrl_properties
            gbs_styles_optional -- also with optional extended styles
            {
              Ada_Put_Line(to_spec, "    " & S(last_Ada_ident) & ": Group_Box_Type;");
              Ada_Coord_conv(last_rect);
              Ada_Put_Line(to_body,
                "    Create( Window." & S(last_Ada_ident) & ", Window, " &
                S(last_text) & ", x,y,w,h);"
              );
            }
            ;

gbs_styles_optional :
          -- nothing
          | COMMA_t
            gbs_style_list
            ex_styles_optional
          ;

gbs_style_list:
            gbs_style
          | gbs_style BAR_t gbs_style_list
          ;

gbs_style : NUMBER
          | ws_style
          | bs_style_only
          ;

----------------
-- List boxes --
----------------

listbox   : LISTBOX_t
            lbs_text
            ctrl_properties_notext
            lbs_styles_optional -- also with optional extended styles
            { Ada_list_box_control; }
            ;

-- Windres (.res -> .rc) outputs a text (seems wrong)
lbs_text  :
          | RCString COMMA_t
          ;

lbs_styles_optional :
          -- nothing
          | COMMA_t
            lbs_style_list
            ex_styles_optional
          ;

lbs_style_list: lbs_style
          | lbs_style BAR_t lbs_style_list
          ;

lbs_style :       LBS_SORT_t
            { style_switch(sort):= True; }
          |       LBS_MULTIPLESEL_t
          |       LBS_MULTICOLUMN_t
          |       LBS_NOINTEGRALHEIGHT_t
          |       LBS_USETABSTOPS_t
          |       LBS_NOTIFY_t
          | NOT_t LBS_NOTIFY_t
          |       LBS_NOREDRAW_t
          |       LBS_EXTENDEDSEL_t
          |       LBS_DISABLENOSCROLL_t
          |       LBS_NOSEL_t
          |       LBS_OWNERDRAWFIXED_t
          |       LBS_HASSTRINGS_t
          | ws_style
          | NUMBER
          ;

---------------------
-- Check boxes [x] --
---------------------

checkbox  : checkbox_hint
            ctrl_properties
            bs_styles_optional -- also with optional extended styles
            {
              style_switch(checkbox):= True;
              Ada_button_control;
            }
            ;

checkbox_hint : CHECKBOX_t
               { style_switch(auto):= False;
                 style_switch(state3):= False;
               }
            |   STATE3_t
               { style_switch(auto):= False;
                 style_switch(state3):= True;
               }
            |   AUTOCHECKBOX_t
               { style_switch(auto):= True;
                 style_switch(state3):= False;
               }
            |   AUTO3STATE_t
               { style_switch(auto):= True;
                 style_switch(state3):= True;
               }
            ;

------------------
-- Push buttons --
------------------

pushbutton  :
            pushbutton_hint
            ctrl_properties
            bs_styles_optional -- also with optional extended styles
            {
              style_switch(push):= True;
              Ada_button_control;
            }
            ;

pushbutton_hint :
            PUSHBUTTON_t
          | DEFPUSHBUTTON_t
            { style_switch(default):= True; }
           ;

-----------------------
-- Radio buttons (.) --
-----------------------

radiobutton :
            radiobutton_hint
            ctrl_properties
            bs_styles_optional -- also with optional extended styles
            {
              style_switch(radio):= True;
              Ada_button_control;
            }
            ;

radiobutton_hint :
             RADIOBUTTON_t
           | AUTORADIOBUTTON_t
             { style_switch(auto):= True; }
           ;

-------------------
-- Button styles --
-------------------

bs_styles_optional :
          -- nothing
          | COMMA_t
            bs_style_list
            ex_styles_optional
          ;

bs_style_list:
            bs_style
          | bs_style BAR_t bs_style_list
          ;

bs_style  : bs_style_only
          | ws_style
          | NUMBER
          ;

bs_style_only :
            BS_LEFTTEXT_t
          | BS_AUTORADIOBUTTON_t
            { style_switch(auto):= True;
              style_switch(radio):= True;
            }
          | BS_RADIOBUTTON_t
            { style_switch(radio):= True; }
          | BS_3STATE_t
            { style_switch(state3):= True; }
          | BS_AUTO3STATE_t
            { style_switch(state3):= True;
              style_switch(auto):= True;
            }
          | BS_CHECKBOX_t
            { style_switch(checkbox):= True; }
          | BS_AUTOCHECKBOX_t
            { style_switch(auto):= True;
              style_switch(checkbox):= True; }
          | BS_BITMAP_t
            { style_switch(bitmap):= True; }
          | BS_ICON_t
            { style_switch(icon):= True; }
          | BS_OWNERDRAW_t
            { style_switch(ownerdraw):= True; }
          | BS_NOTIFY_t
          | BS_TOP_t
          | BS_BOTTOM_t
          | BS_CENTER_t
          | BS_LEFT_t
          | BS_RIGHT_t
          | BS_FLAT_t
          | BS_VCENTER_t
          | BS_MULTILINE_t
            { style_switch (multi_line):= True; }
          | BS_PUSHLIKE_t
          | BS_PUSHBUTTON_t
            { style_switch(push):= True; }
          | BS_DEFPUSHBUTTON_t
            { style_switch(push):= True;
              style_switch(default):= True; }
          | BS_TEXT_t
          | BS_RIGHTBUTTON_t
          | BS_SPLITBUTTON_t
          | BS_DEFSPLITBUTTON_t
            { style_switch(default):= True; }
          | BS_COMMANDLINK_t
          | BS_DEFCOMMANDLINK_t
            { style_switch(default):= True; }
          ;

-----------------
-- Scroll bars --
-----------------

scrollbar :
         SCROLLBAR_t
         ctrl_properties_notext
         sbs_styles_optional
            {
              if style_switch(vertical) then
                Ada_normal_control("GWindows.Scroll_Bars.Scroll_Bar_Type", ", Vertical");
              else
                Ada_normal_control("GWindows.Scroll_Bars.Scroll_Bar_Type", ", Horizontal");
              end if;
            }
            ;

sbs_styles_optional :
          -- nothing
          | COMMA_t sbs_style_list
          ;

sbs_style_list:
            sbs_style
          | sbs_style BAR_t sbs_style_list
          ;

sbs_style : SBS_VERT_t { style_switch(vertical):= True; }
          | ws_style
          | NUMBER
          ;

----------------------
-- Icons in dialogs --
----------------------

icon      : ICON_t
            icon_file  -- image file name
            { last_control_text:= U(yytext); }
            COMMA_t
            ctrl_properties_notext
            es_styles_optional -- also with optional extended styles
            { Ada_icon_control; }
            ;

icon_file : RC_Ident | RCString ;

-----------------------------
-- Common part to controls --
-----------------------------

ctrl_properties_notext :
            RC_Ident -- control's identifier
            { Insert_last_symbol; }
            COMMA_t
            rect
            ;

ctrl_properties :
            RCSTRING -- default text
            { last_text:= U(yytext); }
            COMMA_t
            ctrl_properties_notext
            ;

rect : NUMBER
       { RC_Help.last_rect.x:= yylval.intval;
       }
       COMMA_t
       NUMBER
       { RC_Help.last_rect.y:= yylval.intval;
       }
       COMMA_t
       NUMBER
       { RC_Help.last_rect.w:= yylval.intval;
       }
       COMMA_t
       NUMBER
       { RC_Help.last_rect.h:= yylval.intval;
       }
       ;

-----------
-- Menus --
-----------

menu : RC_Ident
       { if anonymous_item then
           anonymous_menu_counter:=
             anonymous_menu_counter+1;
           last_dialog_ident:= U("Menu_" &
             Trim(Integer'Image(anonymous_menu_counter),both));
         else
           last_dialog_ident:= last_ident;
         end if;
       }
       MENU_t
       properties
       {
         Open_if_separate(S(last_dialog_ident));
         Ada_Put_Line(to_spec,
           "  type " & S(last_dialog_ident) &
           "_Type is tagged record"
         );
         menu_popup_counter:= 0;
         popup_top:= 0;
         Ada_Put_Line(to_spec,
           "    Main: Menu_Type; -- Root of the whole menu tree"
         );
         Ada_New_Line(to_body);
         Ada_Proc_Menu(
            to_body,
            S(last_dialog_ident) & "_Type"
         );
         Ada_New_Line(to_body);
         Ada_Put_Line(to_body, "  is");
         Ada_Put_Line(to_body, "  begin");
         Ada_Put_Line(to_body, "    Menu.Main:= Create_Menu;");
       }
       BEGIN_block
       { empty_dialog_record:= True;
       }
       menu_items_optional
       END_block
       { if empty_dialog_record then
           Ada_Put_Line(to_spec, "    null;  --  empty!");
         end if;
         Ada_Put_Line(to_spec,
           "  end record;  --  " & S(last_dialog_ident) & "_Type"
         );
         Ada_New_Line(to_spec);
         Ada_Proc_Menu(
            to_spec,
            S(last_dialog_ident) & "_Type"
         );
         Ada_Put_Line(to_spec, ";");
         Ada_New_Line(to_spec);
         Ada_Put_Line(to_body,
           "  end Create_Full_Menu;  --  " &
           S(last_dialog_ident) & "_Type" );
         Close_if_separate(S(last_dialog_ident));
       }
     ;


menu_items_optional :
          -- nothing
          | menu_item_list
          ;

menu_item_list:
            menu_item
          | menu_item menu_item_list
          ;

menu_item : menu_entry
          | menu_separator
          | popup
            { empty_dialog_record:= False; }
          ;

popup :     POPUP_t
            RCString
            { last_popup_title:= U(yytext); }
            menu_options
            {
              menu_popup_counter:= menu_popup_counter + 1;
              Ada_Put_Line(to_spec,
                "    " &
                Popup_num_to_Ada_ident(menu_popup_counter) &
                ": Menu_Type; "
                & " -- level" & Integer'Image(popup_top+1) &
                "; title: " &
                S(last_popup_title)
              );
              Ada_Put_Line(to_body,
                "    Menu." &
                Popup_num_to_Ada_ident(menu_popup_counter) &
                ":= Create_Popup;"
              );
              Ada_Put_Line(to_body,
                "    Append_Menu(Menu." &
                Popup_num_to_Ada_ident(popup_stack(popup_top)) &
                ", " & S(last_popup_title) &
                ", Menu." &
                Popup_num_to_Ada_ident(menu_popup_counter) &
                ");"
              );
              popup_top:= popup_top+1;
              popup_stack(popup_top):= menu_popup_counter;
            }
            BEGIN_block
            menu_items_optional
            END_block
            {
              popup_top:= popup_top-1;
            }
          ;

menu_entry :
            MENUITEM_t
            RCString
            {
              style_switch:= (others => False); -- Reset all style switches
              append_item_cmd := To_Unbounded_String(
                "    Append_Item(Menu." &
                Popup_num_to_Ada_ident(popup_stack(popup_top)) &
                ", " & Replace_special_characters(yytext));
            }
            COMMA_t
            RC_Ident
            {
              Insert_last_symbol;
              append_item_cmd := append_item_cmd & ", " & S(last_Ada_constant) & ");";
              if S(last_Ada_constant) = "0" then
                Ada_Put_Line(to_body, 
                  "    --  Constraint error would be raised on line after next, but better having an explanation...");
                Ada_Put_Line(to_body,
                  "    raise Constraint_Error with ""Forgot to set a command for menu item, value 0"";");
              end if;
              Ada_Put_Line(to_body, To_String (append_item_cmd));
            }
            menu_options
            {
              if style_switch(grayed) then
                Ada_Put_Line(to_body, "    State(Menu." &
                Popup_num_to_Ada_ident(popup_stack(popup_top)) &
                ", Command, " & S(last_Ada_constant) &
                ", Grayed);");
              end if;
              if style_switch(inactive) then
                Ada_Put_Line(to_body, "    State(Menu." &
                Popup_num_to_Ada_ident(popup_stack(popup_top)) &
                ", Command, " & S(last_Ada_constant) &
                ", Disabled);");
              end if;
              if style_switch(checked) then
                Ada_Put_Line(to_body, "    Check(Menu." &
                Popup_num_to_Ada_ident(popup_stack(popup_top)) &
                ", Command, " & S(last_Ada_constant) &
                ", True);");
              end if;
            }
            ;

menu_options :
          -- nothing
          | COMMA_t menu_option_list
          | menu_option_list
          ;

menu_option_list:
            menu_option
          | menu_option COMMA_t menu_option_list
          | menu_option menu_option_list
          ;

menu_option : GRAYED_t   { style_switch(grayed):= True; }
            | INACTIVE_t { style_switch(inactive):= True; }
            | CHECKED_t  { style_switch(checked):= True; }
            | HELP_t
            | MENUBARBREAK_t
            | MENUBREAK_t
            ;

menu_separator :
            MENUITEM_t
            SEPARATOR_t
            {
              Ada_Put_Line(to_body,
                "    Append_Separator(Menu." &
                Popup_num_to_Ada_ident(popup_stack(popup_top)) &
                ");"
              );
            }
            ;

------------------
-- Accelerators --
------------------

accelerator :
              RC_Ident
              ACCELERATORS_t
              properties
              BEGIN_block
              accels
              END_block
            ;

accels :
          -- nothing
          | accel_list
          ;

accel_list:
            accel
          | accel accel_list
          ;

accel :
        keystroke
        COMMA_t
        RC_Ident
        accel_options
      ;

keystroke : RC_Ident | RCString ;

accel_options :
          -- nothing
          | COMMA_t accel_option_list
          ;

accel_option_list:
            accel_option
          | accel_option COMMA_t accel_option_list
          ;

accel_option : ASCII_t
             | VIRTKEY_t
             | NOINVERT_t
             | ALT_t
             | SHIFT_t
             | CONTROL_t
             ;

------------------
-- Graphic item --
------------------

graphic :   RC_Ident
            graphic_type
            properties
            file_name
          ;

graphic_type
          :
            BITMAP_t
          | BITMAP_FONT_t
          | CURSOR_t
          | ICON_t
          | PNG_t
          | AVI_t
          ;

file_name : RC_Ident | RCString ;

--------------
-- Versions --
--------------

version_info :
               RC_Ident
               VERSIONINFO_t
               {
                 Open_if_separate("Version_info", with_body => False);
                 if not separate_items then
                   Ada_Put_Line(to_spec, "  package Version_info is");
                 end if;
               }
               fixed_infos
               version_block_contents
               { if not separate_items then
                   Ada_Put_Line(to_spec, "  end Version_info;");
                 end if;
                 Close_if_separate("Version_info", with_body => False);
               }
             ;

fixed_infos :
          -- nothing
          | fixed_info_list
          ;

fixed_info_list:
            fixed_info
          | fixed_info fixed_info_list
          ;

fixed_info : fixed_info_type
           ;

fixed_info_type :
            FILEVERSION_t
          | PRODUCTVERSION_t
          | FILEFLAGSMASK_t
          | FILEFLAGS_t
          | FILEOS_t
          | FILETYPE_t
          | FILESUBTYPE_t
          ;

version_block_contents
          : wrapped_block_contents
          | block_content_list
          ;

wrapped_block_contents:
        BEGIN_block
        block_content_list
        END_block
       ;


block_content_list:
            block_content
          | block_content block_content_list
          ;

block_content :
             block
           | VALUE_t
             {RC_Help.version_info_value_counter:= 0;}
             value_arg_list
           ;

block : BLOCK_t
        RCString
        wrapped_block_contents
      ;

value_arg_list:
            value_arg
          | value_arg COMMA_t value_arg_list
          ;

value_arg :  RCString
             {RC_Help.version_info_value_counter:= RC_Help.version_info_value_counter + 1;
              case RC_Help.version_info_value_counter is
                when 1 =>
                  declare
                    item: constant String:= yytext;
                  begin
                    Ada_Put(to_spec, "    " & item(item'First+1..item'Last-1));
                  end;
                when 2 =>
                  Ada_Put_Line(to_spec, ": constant String:= " & yytext & ';');
                when others =>
                  null;
              end case;
             }
           | NUMBER
             {RC_Help.version_info_value_counter:= RC_Help.version_info_value_counter + 1;
              case RC_Help.version_info_value_counter is
                when 1 =>
                  null; -- should not happen...
                when 2 =>
                  Ada_Put_Line(to_spec, ": constant:=" & Long_Long_Integer'Image(yylval.intval) & ';');
                when others =>
                  null;
              end case;
             }
           ;

--------------
-- Toolbars --
--------------

toolbar : RC_Ident
          TOOLBAR_t
          properties
          RC_Ident
          Comma_t
          RC_Ident
          BEGIN_block
          toolbar_items
          END_block
          ;

toolbar_items :
          -- nothing
          | toolbar_item_list
          ;

toolbar_item_list:
            toolbar_item
          | toolbar_item toolbar_item_list
          ;

toolbar_item :
            BUTTON_t RC_Ident
          | SEPARATOR_t
          ;

-------------------
-- String tables --
-------------------

string_table :
               STRINGTABLE_t
               properties
               BEGIN_block
               string_tbl_items
               END_block
             ;

string_tbl_items :
          -- nothing
          | string_tbl_item_list
          ;

string_tbl_item_list:
            string_tbl_item
          | string_tbl_item string_tbl_item_list
          ;

string_tbl_item:   RC_Ident RCString;



dlginclude : NUMBER DLGINCLUDE_t
             RCSTRING
             {Treat_include(yytext(2..yylength-1));}
             -- 1 DLGINCLUDE "dlge.h"
        ;

textinclude: NUMBER TEXTINCLUDE_t
             properties
             BEGIN_block
             t_i_items
             END_block
        ;

t_i_items :
          -- nothing
          | t_i_item_list
          ;

t_i_item_list:
            t_i_item
          | t_i_item t_i_item_list
          ;

t_i_item: RCString;


include : C_INCLUDE_t RCSTRING
          {Treat_include(yytext(2..yylength-1));}
          -- #include "resource.h"
        | C_INCLUDE_t INCString
          {Treat_include(yytext(2..yylength-1));}
          -- #include <windows.h>
        ;

guidelines :
             GUIDELINES_t DESIGNINFO_t
             properties
             BEGIN_block
             guidelines_blocks
             END_block
           ;

guidelines_blocks :
          -- nothing
          | guidelines_block_list
          ;

guidelines_block_list:
            guidelines_block
          | guidelines_block guidelines_block_list
          ;

guidelines_block :
    guidelines_name COMMA_t DIALOG_t
    BEGIN_block
        guidelines_lines
    END_block
    ;

guidelines_name : RC_Ident | RCString ;

guidelines_lines :
          -- nothing
          | guidelines_line_list
          ;

guidelines_line_list:
            guidelines_line
          | guidelines_line guidelines_line_list
          ;

guidelines_line :
        RC_Ident COMMA_t NUMBER  -- xxxMARGIN
          ;

manifest : RC_Ident RT_MANIFEST_t properties RCString ;

dialog_info :
           RC_Ident
           DLGINIT_t
           BEGIN_block
           dlginit_stuff_list
           END_block
           ;

dlginit_stuff_list:
            dlginit_stuff
          | dlginit_stuff dlginit_stuff_list
          | dlginit_stuff COMMA_t dlginit_stuff_list
          ;

dlginit_stuff: RC_Ident | RCString;

--------------------
-- Terminal items --
--------------------

RC_Ident : IDENT_t
           { last_ident:= U(yytext);
             last_Ada_constant:= Ada_ify(yytext);
             last_Ada_ident:= last_Ada_constant;
             -- normally no confusion here (record entry vs int. constant)
             anonymous_item:= False;
           }
         | NUMBER
           { last_ident:= U(yytext);
             last_Ada_constant:= last_ident;
             if yylval.intval < -1 then
               last_Ada_ident:= U("RC_item_Minus_Invalid" & yytext);
             elsif yylval.intval = -1 then
               New_static_item;
               last_Ada_constant:= U("IDC_STATIC");
             else
               last_Ada_ident:= U("RC_item_" & yytext);
             end if;
             anonymous_item:= True;
           }
         | IDC_STATIC_t
           { last_ident:= U(yytext);
             last_Ada_constant:= last_ident;
             New_static_item;
             anonymous_item:= True;
           }
         ;

Style_Ident : IDENT_t | NUMBER ;

BEGIN_block : BEGIN_t | LBRACE_t ;   -- RC has both C and Pascal syntaxes !
END_block   : END_t   | RBRACE_t ;   -- RC has both C and Pascal syntaxes !

%%

-- This header comes from RC.y (bottom)

with RC_Tokens, RC_Shift_Reduce, RC_Goto, RC_Help, RC_IO;
use  RC_Tokens, RC_Shift_Reduce, RC_Goto, RC_Help, RC_IO;

with RC_DFA, YYroutines, YYerror;
use  RC_DFA, YYroutines;

with Ada.Text_IO;                       use Ada.Text_IO;
with Text_IO; -- for compat.

with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;

with Interfaces;                        use Interfaces;

with GWindows.Static_Controls,
     GWindows.Common_Controls;

-- Header end.

##