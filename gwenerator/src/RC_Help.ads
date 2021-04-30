--------------------------------------------------------------------------
--  RC_Help.ads
--
--  Helper for the MS Windows Resource Compiler script parser
--
--  Copyright (c) Gautier de Montmollin 2008 .. 2019
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

--  NB: this is the MIT License, as found 28-Jul-2008 on the site
--  http://www.opensource.org/licenses/mit-license.php
----------------------------------------------------------------------------

with GWindows.Static_Controls,
     GWindows.Common_Controls;

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;

package RC_Help is

  Grammar_Version: constant String:= "14-Apr-2021";
  Web: constant String:= "http://sf.net/projects/gnavi";

  function S(Source: Unbounded_String) return String
    renames Ada.Strings.Unbounded.To_String;
  function U(Source: String) return Unbounded_String
    renames Ada.Strings.Unbounded.To_Unbounded_String;

  function Ada_ify(s: String) return Unbounded_String;
  function Replace_special_characters(str: String) return String;

  function RC_to_Package_name(
    rc_name         : String;
    has_input_param : Boolean; -- False => ignores rc_name and uses a default name
    as_file_name    : Boolean
  ) return String;

  has_input: Boolean;
  source_name: Unbounded_String;

  linenum : Integer;

  --------------------------------
  -- GWenerator / RC2GW Options --
  --------------------------------

  base_unit_x        : Positive;
  base_unit_y        : Positive;
  separate_items     : Boolean;
  generate_test      : Boolean;
  initialize_controls: Boolean; -- Analogy: Ada's pragma Initialize_Scalars

  type Pkg_output is (to_spec, to_body);

  GWen_proj: Unbounded_String;

  ------------------------
  -- Style combinations --
  ------------------------
  --  They come either from control name like AUTORADIOBUTTON, or
  --  from styles BS_AUTORADIOBUTTON

  type Style_switch_type is
    ( auto,
      disabled,
      hidden,       -- = not visible (not ws_visible)
      radio,        -- buttons
      state3,
      checkbox,
      push,
      default,
      bitmap,
      icon,
      ownerdraw,
      shell_font,
      sys_menu,
      auto_h_scroll,
      auto_v_scroll,
      read_only,
      sort,
      vertical,
      tips,
      keys,
      wrap,
      no_1000,
      smooth,
      simple_border,
      fully_sunken,
      half_sunken,
      whiterect,
      multi_line, --  Edit boxes or Box/button styles
      grayed,     --  Menus
      inactive,
      checked,
      has_buttons, -- tree view
      has_lines,
      lines_at_root,
      single_expand,
      real_size_image,
      center_image,
      right_justify
    );

  type Set_of_styles is array(Style_switch_type) of Boolean;

  dialog_style_switch, style_switch: Set_of_styles;

  ------------------------------------------------------------
  -- Static controls - very standard Windows                --
  -- Common controls (CommCtrl.h) - extension to Windows... --
  ------------------------------------------------------------

  type Control_type is
    ( unknown,
      --  "Static controls":
      static,
      icon,
      bitmap,
      button,
      edit,
      --  "Common controls":
      track_bar, -- slider
      up_down,   -- spin
      progress,
      list_view, SysListView32, -- list view (with specific/alias notation)
      tree_view, SysTreeView32, -- tree view (with specific/alias notation)
      tab_control,
      date_time,
      calendar
    );

  control: Control_type;

  type Control_Direction_Type is (Horizontal, Vertical);
  Control_Direction: Control_Direction_Type; -- direction of various controls

  type Trackbar_Control_Ticks_Type is (Top_Ticks,
                                       Bottom_Ticks,
                                       Both_Ticks,
                                       No_Ticks);

  Trackbar_Control_Ticks: Trackbar_Control_Ticks_Type;

  last_alignment: GWindows.Static_Controls.Alignment_Type:= GWindows.Static_Controls.Left;
  --
  --  List view options
  --
  lv_type  : GWindows.Common_Controls.List_View_Control_View_Type;
  lv_select: GWindows.Common_Controls.List_View_Control_Select_Type;
  lv_sort  : GWindows.Common_Controls.List_View_Control_Sort_Type;
  lv_align : GWindows.Common_Controls.List_View_Control_Alignment_Type;
  lv_auto_arrange: Boolean;

  type Combo_type is (no_drop, drop_down, drop_down_list);
  combo: Combo_type;

  type Rect_type is record
    x,y,w,h: Long_Long_Integer;
  end record;

  last_rect, last_dialog_rect : Rect_type;

  function Image(rect: Rect_type) return String;

  last_ident, last_Ada_ident, last_Ada_constant : Unbounded_String;
  anonymous_item: Boolean;
  empty_dialog_record: Boolean;
  last_text, last_class, last_control_text: Unbounded_String;
  last_dialog_ident: Unbounded_String;
  last_caption, last_dialog_caption: Unbounded_String;
  version_info_value_counter: Natural;
  version_info_value: Unbounded_String;

  static_counter: Natural;
  --  Counter for objects labelled as static (-1, ID_STATIC) but
  --  that cannot be anonymous like labels.
  --  Essentially Group_Boxes are concerned.
  procedure New_static_item;

  anonymous_dialog_counter: Natural;
  anonymous_menu_counter: Natural;

  menu_popup_counter, popup_top: Natural;
  popup_stack: array(0..1000) of Natural; -- recall parent popups
  function Popup_num_to_Ada_ident(n: Natural) return String;
  last_popup_title: Unbounded_String;
  append_item_cmd : Unbounded_String;

  procedure Insert_symbol(sym_name: String; value: Integer);
  procedure Insert_last_symbol;
  procedure Treat_include(fn: String);

  Ada_files: array(Pkg_output) of Ada.Text_IO.File_Type;
  Syntax_Error : exception;
  procedure Ada_Begin;

  procedure YY_Accept;
  procedure YY_Abort;
  procedure YY_Terminate;

  procedure Open_if_separate(item: String; with_body: Boolean:= True);
  procedure Close_if_separate(item: String; with_body: Boolean:= True);

  procedure Ada_Put(to: Pkg_output; s: String);
  procedure Ada_Put_Line(to: Pkg_output; s: String);
  procedure Ada_New_Line(to: Pkg_output);
  procedure Ada_Comment(to: Pkg_output; s: String);
  procedure Ada_Proc_Dialog(
    to       : Pkg_output;
    type_name: String;
    title    : String
  );
  procedure Ada_Proc_Menu(
    to       : Pkg_output;
    type_name: String
  );
  procedure Ada_Coord_conv(rect: Rect_type);

  procedure Ada_normal_control_create(comma_text, extra: String:= ""; with_id: Boolean:= True);
  procedure Ada_normal_control(type_name: String; comma_text, extra: String:= ""; with_id: Boolean:= True);
  procedure Ada_label_control; -- Static text
  procedure Ada_icon_control;
  procedure Ada_bitmap_control;
  procedure Ada_button_control;
  procedure Ada_edit_control;
  procedure Ada_list_box_control;
  procedure Ada_combo_control;

  procedure Reset_control_styles;
  --  All that begin with CONTROL, e.g. CONTROL "" ,IDC_EDIT11,"EDIT", ...
  procedure Ada_untyped_control;

  procedure Ada_optional_disabling;

  --  Control class is given as a string, not a token (e.g. "Button")
  procedure Identify_control_class(RC_String: String);

  procedure RC_Comment(s: String);

  procedure Reset_Globals;

end RC_Help;
