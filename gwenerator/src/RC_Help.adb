--------------------------------------------------------------------------
--  RC_Help.adb
--
--  Helper for the MS Windows Resource Compiler script parser
--
--  Copyright (c) Gautier de Montmollin 2008 .. 2024
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

with Ada.Characters.Handling,
     Ada.Strings.Fixed,
     Ada.Unchecked_Deallocation;

with Resource_Header,
     Time_display;

package body RC_Help is

  use Ada.Characters.Handling, Ada.Strings,
      Ada.Strings.Fixed, Ada.Text_IO;

  function Ada_ify (s : String) return Unbounded_String is
    us : Unbounded_String := U (s);
    i : Natural;
  begin
    loop
      i := Index (us, "__");
      exit when i = 0;
      Delete (us, i, i);
    end loop;
    return us;
  end Ada_ify;

  function Replace_special_characters (str : String) return String is
    us : Unbounded_String := U (str);
    i : Natural;
  begin
    loop
      i := Index (us, "\t");
      exit when i = 0;
      Replace_Slice (us, i, i + 1, """ & To_GString_From_String ((1 => ASCII.HT)) & """);
    end loop;
    return S (us);
  end Replace_special_characters;

  function Combo_type_name (combo_choice : Combo_type) return String is
  begin
    case combo_choice is
      when no_drop => return "Combo_Box_Type";
      when drop_down => return "Drop_Down_Combo_Box_Type";
      when drop_down_list => return "Drop_Down_List_Box_Type";
    end case;
  end Combo_type_name;

  function Image (i : Long_Long_Integer) return String is
  begin
    return Trim (Long_Long_Integer'Image (i), Both);
  end Image;

  function Image (rect : Rect_type) return String is
  begin
    return
      Image (rect.x) & ',' &
      Image (rect.y) & ',' &
      Image (rect.w) & ',' &
      Image (rect.h);
  end Image;

  procedure iPut (to : Pkg_output; s : String; as_comment : Boolean);

  procedure iNew_Line (to : Pkg_output) is
  begin
    Ada.Text_IO.New_Line (Ada_files (to));
    iPut (to, "", as_comment => False);
  end iNew_Line;

  procedure iPut (to : Pkg_output; s : String; as_comment : Boolean) is
    pr : Boolean := False;
  begin
    if as_comment then
      for i in s'Range loop
        case s (i) is
          when ASCII.CR | ASCII.LF =>
            if not pr then  --  avoid pairs
              iNew_Line (to);
              iPut (to, "--  ", as_comment => False);
            end if;
            pr := True;
          when others =>
            Ada.Text_IO.Put (Ada_files (to), s (i));
            pr := False;
        end case;
      end loop;
    else
      Ada.Text_IO.Put (Ada_files (to), s);
    end if;
  end iPut;

  procedure iPut_Line (to : Pkg_output; s : String; as_comment : Boolean) is
  begin
    iPut (to, s, as_comment);
    iNew_Line (to);
  end iPut_Line;

  procedure Ada_Put (to : Pkg_output; s : String) is
  begin
    iPut (to, s, as_comment => False);
  end Ada_Put;

  procedure Ada_Put_Line (to : Pkg_output; s : String) is
  begin
    iPut_Line (to, s, as_comment => False);
  end Ada_Put_Line;

  procedure Ada_New_Line (to : Pkg_output) is
  begin
    iNew_Line (to);
  end Ada_New_Line;

  procedure Ada_Comment (to : Pkg_output; s : String) is
  begin
    iPut_Line (to, "--  " & s, as_comment => True);
  end Ada_Comment;

  function Root_name (sn : String) return String is
  begin
    for i in reverse sn'Range loop
      if sn (i) = '.' then -- skip file extension, if any
        return sn (sn'First .. i - 1);
      end if;
    end loop;
    return sn;
  end Root_name;

  function RC_to_Package_name (
    rc_name         : String;
    has_input_param : Boolean;
    as_file_name    : Boolean
  ) return String
  is
    sep_child : constant array (Boolean) of Character := ('.', '-');
    as_child : constant Boolean := False;
    --  Problem with making a the main resource package a child:
    --  parent may not be a package but a procedure.
    sep : constant array (Boolean) of Character := ('_', sep_child (as_file_name));
    suffix : constant String := sep (as_child) & "Resource_GUI";
    first : Natural := rc_name'First;
  begin
    if has_input_param then
      if as_file_name then
        return To_Lower (Root_name (rc_name) & suffix);
        --  NB: would need to be smarter for Linux and other case-sensitve systems...
      else  --  remove path by finding last separator
        for i in rc_name'Range loop
          if rc_name (i) = '\' or rc_name (i) = '/' then
            first := i + 1;
          end if;
        end loop;
        return Root_name (rc_name (first .. rc_name'Last)) & suffix;
      end if;
    else
      return "Input" & suffix;
    end if;
  end RC_to_Package_name;

  function pkg (as_file_name : Boolean := False) return String is
  begin
    return RC_to_Package_name (S (source_name), has_input, as_file_name);
  end pkg;

  function Image_zeros (n : Natural) return String is
    im : constant String := Integer'Image (n + 10_000);
  begin
    return im (im'Last - 3 .. im'Last);
  end Image_zeros;

  function Popup_num_to_Ada_ident (n : Natural) return String is
  begin
    if n = 0 then
      return "Main";
    else
      return "Popup_" & Image_zeros (n);
    end if;
  end Popup_num_to_Ada_ident;

  procedure New_static_item is
  begin
    static_counter := static_counter + 1;
    last_Ada_ident := U ("Static_" & Image_zeros (static_counter));
  end New_static_item;

  ------------------------------------
  -- Management of resource symbols --
  ------------------------------------

  type Dir_node;
  type p_Dir_node is access Dir_node;

  type Entry_type is record
    RC_Ident    : Unbounded_String;
    Ada_constant : Unbounded_String;
    value       : Integer;
    common      : Boolean;  -- designates common Win32 ID's (See GWindows.Constants)
  end record;

  type Dir_node is record
    left, right : p_Dir_node;
    item        : Entry_type;
  end record;

  procedure Dispose is new Ada.Unchecked_Deallocation (Dir_node, p_Dir_node);

  procedure Delete (p : in out p_Dir_node) is
  begin
    if p /= null then
       Delete (p.left);
       Delete (p.right);
       Dispose (p);
       p := null;
    end if;
  end Delete;

  type Sorting_mode is (by_name, by_value);

  --  We need two dictionaries
  --  - by value, for the nice sorted output
  --  - by name, for avoiding duplicating names

  symbols_by_name, symbols_by_value : p_Dir_node := null;

  procedure Insert (item      :  in     Entry_type;
                    node      :  in out p_Dir_node;
                    sort_mode :  in     Sorting_mode) is
  begin
    if node = null then
      node := new Dir_node'
        ((item         => item,
           left         => null,
           right        => null
           )
        );
      if sort_mode = by_name then
        --  This new name is unique, let's insert by value too
        Insert (item, symbols_by_value, by_value);
      end if;
    else
      case sort_mode is
        when by_value =>
          if item.value >= node.item.value then
             --  '=': values may be duplicate (several constants with same values)
            Insert (item, node.right, sort_mode);
          else
            Insert (item, node.left, sort_mode);
          end if;
        when by_name =>
          if item.RC_Ident > node.item.RC_Ident then
            Insert (item, node.right, sort_mode);
          elsif item.RC_Ident < node.item.RC_Ident then
            Insert (item, node.left, sort_mode);
          else
            --  duplicated names are ignored
            null;
          end if;
      end case;
    end if;
  end Insert;

  procedure Insert_last_symbol is
  begin
    if not anonymous_item then
      Insert ((last_ident, last_Ada_constant, -1, False), symbols_by_name, by_name);
      --  In principle already inserted via resource.h, unless that one is missing
    end if;
  end Insert_last_symbol;

  procedure Insert_symbol (sym_name : String; value : Integer) is
  begin
    Insert ((U (sym_name), Ada_ify (sym_name), value, False), symbols_by_name, by_name);
  end Insert_symbol;

  procedure Insert_Common (symb : String; value : Integer) is
  begin
    Insert ((U (symb), U (symb), value, True), symbols_by_name, by_name);
  end Insert_Common;

  first_include : Boolean;

  procedure Treat_include (fn : String) is
    done : Boolean;
    use Resource_Header;
  begin
    if first_include then
      Convert_Header_File (fn, done);
      if done then
        Put_Line (Current_Error, " . . . . Processed main header file: " & fn & '.');
        first_include := False;  --  otherwise, we try with the next one, if any...
      end if;
    end if;
  end Treat_include;

  procedure Output_symbols is

    max_name, max_img : Integer := 0;

    procedure Set_max (p : p_Dir_node) is
    begin
      if p /= null then
        Set_max (p.left);
        --
        max_name := Integer'Max (max_name, S (p.item.Ada_constant)'Length);
        max_img := Integer'Max (max_img,  Integer'Image (p.item.value)'Length);
        --
        Set_max (p.right);
      end if;
    end Set_max;

    procedure Output_node_and_sons (p : p_Dir_node) is
    begin
      if p /= null then
        Output_node_and_sons (p.left);
        --
        declare
          nm : constant String := S (p.item.Ada_constant);
          im : constant String := Integer'Image (p.item.value);
        begin
          if p.item.common then
            null;  --  Do not output ID's already available in GWindows.Constants
          else
            Ada_Put_Line (to_spec,
               "  " & nm & (1 + max_name - nm'Length) * ' ' &
               ": constant := " & (max_img - im'Length) * ' ' & im & ';'
            );
          end if;
        end;
        Output_node_and_sons (p.right);
      end if;
    end Output_node_and_sons;

  begin
    Ada_Put_Line (to_spec, "  --------------------------------------------------");
    Ada_Put_Line (to_spec, "  --  Defined resource symbols --> Ada constants  --");
    Ada_Put_Line (to_spec, "  --------------------------------------------------");
    Ada_New_Line (to_spec);
    Ada_Put_Line (to_spec, "  --  NB: only items with a defined symbol get a constant here");
    Ada_Put_Line (to_spec, "  --  These constants are needed for getting button and menu feedbacks.");
    Ada_New_Line (to_spec);
    Set_max (symbols_by_value);
    Output_node_and_sons (symbols_by_value);
  end Output_symbols;

  --

  procedure Blurb (to : Pkg_output) is
  begin
    Ada_Put_Line (to, "---------------------------------------------------------------------------");
    Ada_Put_Line (to, "--  GUI contents of resource script file: " & S (source_name));
    Ada_Put_Line (to, "--  Transcription time: " & Time_display);
    if GWen_proj /= "" then
      Ada_Put_Line (to, "--  GWenerator project file: " & S (GWen_proj));
    end if;
    Ada_Put_Line (to, "--");
    Ada_Put_Line (to, "--  Translated by the RC2GW or by the GWenerator tool.");
    Ada_Put_Line (to, "--  URL: " & Web);
    Ada_Put_Line (to, "--");
    Ada_Put_Line (to, "--  This file contains only automatically generated code. Do not edit this.");
    Ada_Put_Line (to, "--  Rework the resource script instead, and re-run the translator.");
    Ada_Put_Line (to, "--  RC Grammar version: " & Grammar_Version);
    Ada_Put_Line (to, "---------------------------------------------------------------------------");
    Ada_New_Line (to);
  end Blurb;

  procedure Ada_package_headers (eventual_child : String) is
  begin
    Blurb (to_spec);
    Blurb (to_body);
    --
    Ada_Put_Line (to_spec, "with GWindows.Base;                     use GWindows.Base;");
    Ada_Put_Line (to_spec, "with GWindows.Constants;                use GWindows.Constants;");
    Ada_Put_Line (to_spec, "with GWindows.Windows;                  use GWindows.Windows;");
    Ada_Put_Line (to_spec, "with GWindows.Buttons;                  use GWindows.Buttons;");
    Ada_Put_Line (to_spec, "with GWindows.Buttons.Graphic;          use GWindows.Buttons.Graphic;");
    Ada_Put_Line (to_spec, "with GWindows.Buttons.Owner_Drawn;      use GWindows.Buttons.Owner_Drawn;");
    Ada_Put_Line (to_spec, "with GWindows.Edit_Boxes;               use GWindows.Edit_Boxes;");
    Ada_Put_Line (to_spec, "with GWindows.List_Boxes;               use GWindows.List_Boxes;");
    Ada_Put_Line (to_spec, "with GWindows.Combo_Boxes;              use GWindows.Combo_Boxes;");
    Ada_Put_Line (to_spec, "with GWindows.Static_Controls;          use GWindows.Static_Controls;");
    Ada_Put_Line (to_spec, "with GWindows.Scroll_Bars;              use GWindows.Scroll_Bars;");
    Ada_Put_Line (to_spec, "with GWindows.Common_Controls;          use GWindows.Common_Controls;");
    Ada_Put_Line (to_spec, "with GWindows.Menus;                    use GWindows.Menus;");
    Ada_Put_Line (to_spec, "use GWindows;");
    Ada_Put_Line (to_spec, "with Interfaces.C;                      use Interfaces.C;");
    Ada_New_Line (to_spec);
    Ada_Put_Line (to_spec, "pragma Warnings (""U"");  --  turn off warnings for unused entity");
    Ada_New_Line (to_spec);
    Ada_Put_Line (to_spec, "package " & pkg & eventual_child & " is");
    Ada_New_Line (to_spec);
    --
    Ada_Put_Line (to_body, "with GWindows.Types;                    use GWindows.Types;");
    Ada_Put_Line (to_body, "with GWindows.Drawing;                  use GWindows.Drawing;");
    Ada_Put_Line (to_body, "with GWindows.Drawing_Objects;");
    Ada_Put_Line (to_body, "with GWindows.GStrings;                 use GWindows.GStrings;");

    Ada_Put_Line (to_body, "with System;");
    Ada_New_Line (to_body);
    if separate_items then
      Ada_Put_Line (to_body, "with " & pkg & ".Helpers;");
      Ada_Put_Line (to_body, " use " & pkg & ".Helpers;");
      Ada_Put_Line (to_body, "with " & pkg & ".Constants;");
      Ada_Put_Line (to_body, " use " & pkg & ".Constants;");
    end if;

    Ada_Put_Line (to_body, "pragma Warnings (""U"");  --  turn off warnings for unused entity");
    Ada_New_Line (to_body);
    Ada_Put_Line (to_body, "package body " & pkg & eventual_child & " is");
    Ada_New_Line (to_body);
  end Ada_package_headers;

  procedure Create (on_pkg : Pkg_output; name : String) is
  begin
    Create (Ada_files (on_pkg), Out_File, name);
    Put_Line (Current_Error, " . . . . Writing: " & name);
  end Create;

  procedure Open_if_separate (item : String; with_body : Boolean := True) is
  begin
    if separate_items then
      Create (to_spec, pkg (as_file_name => True) & '-' & To_Lower (item) & ".ads");
      if with_body then
        Create (to_body, pkg (as_file_name => True) & '-' & To_Lower (item) & ".adb");
        Ada_package_headers (eventual_child => '.' & item);
      else
        Blurb (to_spec);
        Ada_Put_Line (to_spec, "package " & pkg & '.' & item & " is");
        Ada_New_Line (to_spec);
      end if;
    end if;
  end Open_if_separate;

  procedure Close_if_separate (item : String; with_body : Boolean := True) is
  begin
    if separate_items then
      for to in Pkg_output loop
        if to = to_spec or with_body then
          Ada_New_Line (to);
          Ada_Put_Line (to, "end " & pkg & '.' & item & ';');
          Close (Ada_files (to));
        end if;
      end loop;
    end if;
  end Close_if_separate;

  --  "Correct" casing for <Enum>'Image
  generic
    type Enum is (<>);
  function Enum_Img_Mixed (e : Enum) return String;

  function Enum_Img_Mixed (e : Enum) return String is
    s : String := Enum'Image (e);
    low : Boolean := False;
  begin
    for i in s'Range loop
      if low then
        s (i) := To_Lower (s (i));
      end if;
      low := s (i) /= '_';
    end loop;
    return s;
  end Enum_Img_Mixed;

  function Img is new Enum_Img_Mixed (Boolean);
  function Img is new Enum_Img_Mixed (GWindows.Static_Controls.Alignment_Type);
  function Img is new Enum_Img_Mixed (GWindows.Static_Controls.Border_Type);
  function Img is new Enum_Img_Mixed (Control_Direction_Type);
  function Img is new Enum_Img_Mixed (GWindows.Common_Controls.List_View_Control_Select_Type);
  function Img is new Enum_Img_Mixed (GWindows.Common_Controls.List_View_Control_View_Type);
  function Img is new Enum_Img_Mixed (GWindows.Common_Controls.List_View_Control_Sort_Type);
  function Img is new Enum_Img_Mixed (GWindows.Common_Controls.List_View_Control_Alignment_Type);
  function Img is new Enum_Img_Mixed (Trackbar_Control_Ticks_Type);

  --  Add or remove specific Windows styles that are not in GWindows Create parameters.
  --
  procedure Ada_Dialog_Pre_settings (
    to          : Pkg_output;
    type_name   : String
  )
  is
  begin
    if not dialog_style_switch (sys_menu) then  --  maybe also: " and ws_dlgframe)"
      Ada_Put_Line (to, "  --  Pre-Create operation to switch off default styles, or");
      Ada_Put_Line (to, "  --  add ones that are not in usual GWindows Create parameters.");
      Ada_Put_Line (to, "  --");
      Ada_Put_Line (to, "  procedure On_Pre_Create (Window    : in out " & type_name & ";");
      Ada_Put_Line (to, "                           dwStyle   : in out Interfaces.C.unsigned;");
      Ada_Put      (to, "                           dwExStyle : in out Interfaces.C.unsigned)");
      case to is
        when to_spec =>
          Ada_Put_Line (to, ";");
        when to_body =>
          Ada_New_Line (to);
          Ada_Put_Line (to, "  is");
          Ada_Put_Line (to, "    pragma Unmodified (Window);");
          Ada_Put_Line (to, "    pragma Unmodified (dwExStyle);");
          Ada_Put_Line (to, "    WS_SYSMENU : constant := 16#0008_0000#;");
          Ada_Put_Line (to, "  begin");
          if not style_switch (sys_menu) then
            Ada_Put_Line (to, "    dwStyle := dwStyle and not WS_SYSMENU;");
          end if;
          Ada_Put_Line (to, "  end On_Pre_Create;");
      end case;
      Ada_New_Line (to);
    end if;
  end Ada_Dialog_Pre_settings;

  dialog_mem : array (1 .. 10_000) of Unbounded_String;
  dialog_top : Natural;

  procedure Ada_Proc_Dialog (
    to          : Pkg_output;
    type_name   : String;
    title       : String
  )
  is
  begin
    --  First, finish defining type
    if to = to_spec then
      dialog_top := dialog_top + 1;
      dialog_mem (dialog_top) := U (type_name);
      if empty_dialog_record then
        Ada_Put_Line (to_spec, "    null;  --  empty!");
      end if;
      Ada_Put_Line (to_spec,
        "  end record; -- " & S (last_dialog_ident) & "_Type"
      );
      Ada_New_Line (to_spec);
    end if;

    Ada_Put_Line (to, "  --  Dialog at resource line" & Integer'Image (linenum));
    Ada_New_Line (to);
    Ada_Dialog_Pre_settings (to, type_name);
    Ada_Put_Line (to, "  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog");
    Ada_Put_Line (to, "  --");
    Ada_Put_Line (to, "  procedure Create_Full_Dialog");
    Ada_Put_Line (to, "     (Window      : in out " & type_name & ";");
    Ada_Put_Line (to, "      Parent      : in out GWindows.Base.Base_Window_Type'Class;");
    Ada_Put_Line (to, "      Title       : in     GString := " & title & ";");
    Ada_Put_Line (to, "      Left        : in     Integer := Use_Default;  --  Default = as designed");
    Ada_Put_Line (to, "      Top         : in     Integer := Use_Default;  --  Default = as designed");
    Ada_Put_Line (to, "      Width       : in     Integer := Use_Default;  --  Default = as designed");
    Ada_Put_Line (to, "      Height      : in     Integer := Use_Default;  --  Default = as designed");
    Ada_Put_Line (to, "      Help_Button : in     Boolean := False;");
    Ada_Put (to,      "      Is_Dynamic  : in     Boolean := False)");
    case to is
      when to_spec =>
        Ada_Put_Line (to, ";");
      when to_body =>
        Ada_New_Line (to_body);
        Ada_Put_Line (to_body, "  is");
        Ada_Put_Line (to_body, "    x, y, w, h : Integer;");
        Ada_Put_Line (to_body, "  begin");
        Ada_Coord_conv (last_dialog_rect);
        Ada_Put_Line (to_body, "    if Left   /= Use_Default then x := Left;   end if;");
        Ada_Put_Line (to_body, "    if Top    /= Use_Default then y := Top;    end if;");
        Ada_Put_Line (to_body, "    if Width  /= Use_Default then w := Width;  end if;");
        Ada_Put_Line (to_body, "    if Height /= Use_Default then h := Height; end if;");
        Ada_Put_Line (to_body,
           "    Create_As_Dialog"
        );
        Ada_Put_Line (to_body, "     (Window => Window_Type (Window),");
        Ada_Put_Line (to_body, "      Parent => Parent,");
        Ada_Put_Line (to_body, "      Title  => Title,");
        Ada_Put_Line (to_body, "      Left   => x,");
        Ada_Put_Line (to_body, "      Top    => y,");
        Ada_Put_Line (to_body, "      Width  => w,");
        Ada_Put_Line (to_body, "      Height => h,");
        Ada_Put_Line (to_body, "      Help_Button => Help_Button,");
        Ada_Put_Line (to_body, "      Is_Dynamic  => Is_Dynamic");
        Ada_Put_Line (to_body, "    );");
        Ada_Put_Line (to_body, "    if Width = Use_Default then  Client_Area_Width (Window, w); end if;");
        Ada_Put_Line (to_body, "    if Height = Use_Default then Client_Area_Height (Window, h); end if;");
        if style_switch (shell_font) then
          Ada_Put_Line (to_body, "    Use_GUI_Font (Window);");
        end if;
        Ada_Put_Line (to_body, "    Create_Contents (Window, True);");
        Ada_Put_Line (to_body, "  end Create_Full_Dialog;  --  " &
                   S (last_dialog_ident) & "_Type");
    end case;
    Ada_New_Line (to);
    Ada_Put_Line (to, "  --    b) Create all contents, not the window itself (must be");
    Ada_Put_Line (to, "  --        already created) -> can be used in/as any kind of window.");
    Ada_Put_Line (to, "  --");
    Ada_Put_Line (to, "  procedure Create_Contents");
    Ada_Put_Line (to, "      (Window      : in out " & type_name & ";");
    Ada_Put_Line (to, "       for_dialog  : in     Boolean;          --  True: buttons do close the window");
    Ada_Put_Line (to, "       resize      : in     Boolean := False  --  optionally resize Window as designed");
    Ada_Put (to,      "     )");
    if to = to_body then
      Ada_New_Line (to);
      Ada_Put_Line (to_body, "  is");
      Ada_Put_Line (to_body, "    x, y, w, h : Integer;");
      Ada_Put_Line (to_body, "  begin");
      Ada_Put_Line (to_body, "    if resize then");
      Ada_Coord_conv (last_dialog_rect);
      Ada_Put_Line (to_body, "      Move (Window, x, y);");
      Ada_Put_Line (to_body, "      Client_Area_Size (Window, w, h);");
      Ada_Put_Line (to_body, "    end if;");
      if style_switch (shell_font) then
        Ada_Put_Line (to_body, "    Use_GUI_Font (Window);");
      end if;
    end if;
  end Ada_Proc_Dialog;

  procedure Ada_Proc_Menu (
    to        : Pkg_output;
    type_name : String
  )
  is
  begin
    Ada_Put_Line (to, "  --  Menu at line" & Integer'Image (linenum));
    Ada_Put      (to, "  procedure Create_Full_Menu (New_Menu : in out " & type_name & ")");
  end Ada_Proc_Menu;

  procedure Ada_Helpers_spec (to : Pkg_output) is
  begin
    Ada_New_Line (to);
    Ada_Put_Line (to, "  procedure Dlg_to_Scn");
    Ada_Put_Line (to, "    (xd, yd, wd, hd :  in Integer;");
    Ada_Put_Line (to, "     xs, ys, ws, hs : out Integer);");
    Ada_New_Line (to);
    Ada_Put_Line (to, "  procedure Use_GUI_Font (Window : in out GWindows.Base.Base_Window_Type'Class);");
    Ada_New_Line (to);
    Ada_Put_Line (to, "  function Num_resource (id : Natural) return GString;  --  Just turn 123 into ""#123"".");
  end Ada_Helpers_spec;

  procedure Ada_Begin is
  begin
    if separate_items then
      Create (to_spec, pkg (as_file_name => True) & "-helpers.ads");
      Blurb (to_spec);
      Ada_Put_Line (to_spec, "with GWindows.Base;");
      Ada_New_Line (to_spec);
      Ada_Put_Line (to_spec, "package " & pkg & ".Helpers is");
      Ada_Put_Line (to_spec, "  use GWindows;");
      Ada_Helpers_spec (to_spec);
      Ada_Put_Line (to_spec, "end " & pkg & ".Helpers;");
      Close (Ada_files (to_spec));
      --
      Create (to_spec, pkg (as_file_name => True) & ".ads");
      Blurb (to_spec);
      Ada_Put_Line (to_spec, "--  Option separate_items selected (-s with RC2GW), then all items are in child packages.");
      Ada_Put_Line (to_spec, "--  Children are");
      Ada_Put_Line (to_spec, "--    1 per dialog or menu item");
      Ada_Put_Line (to_spec, "--    " & pkg & ".Constants for resource constants (ID's)");
      Ada_Put_Line (to_spec, "--    " & pkg & ".Helpers for internal helper routines");
      Ada_Put_Line (to_spec, "--    " & pkg & ".Version_info for constants stored in the VersionInfo part.");
      Ada_New_Line (to_spec);
      Ada_Put_Line (to_spec, "package " & pkg & " is");
      Ada_Put_Line (to_spec, "  --  empty: everything is in child packages.");
      Ada_Put_Line (to_spec, "end " & pkg & ';');
      Close (Ada_files (to_spec));
    else
      Create (to_spec, pkg (as_file_name => True) & ".ads");
      Create (to_body, pkg (as_file_name => True) & ".adb");
      Ada_package_headers (eventual_child => "");
      Ada_Put_Line (to_body, "  --  ** Generated code begins here \/ \/ \/.");
    end if;
    --
    popup_stack (0) := 0; -- root is "Main" menu
    anonymous_dialog_counter := 0;
    anonymous_menu_counter := 0;
  end Ada_Begin;

  procedure Ada_Coord_conv (rect : Rect_type) is
  begin
     Ada_Put_Line (to_body,
       "    Dlg_to_Scn (" &  --  Window_Type (Window),
       Image (rect.x) & ", " &
       Image (rect.y) & ", " &
       Image (rect.w) & ", " &
       Image (rect.h) & ", x, y, w, h);");
  end Ada_Coord_conv;

  procedure Ada_normal_control_create (comma_text, extra : String := ""; with_id : Boolean := True) is
    function id_part return String is
    begin
      if with_id then
        return ", ID => " & S (last_Ada_constant);
      else
        return "";
      end if;
    end id_part;

  begin
    Ada_Put_Line (to_body,
      "    Create (Window." & S (last_Ada_ident) & ", Window" &
      comma_text & ", x, y, w, h" & extra & id_part &
      ");"
    );
  end Ada_normal_control_create;

  procedure Ada_normal_control (type_name : String; comma_text, extra : String := ""; with_id : Boolean := True) is
  begin
    Ada_Coord_conv (last_rect);
    Ada_Put_Line (to_spec, "    " & S (last_Ada_ident) & " : " & type_name & ";");
    Ada_normal_control_create (comma_text, extra, with_id);
    Ada_optional_disabling;
  end Ada_normal_control;

  procedure Ada_very_normal_control (type_name : String) is
  begin
    Ada_normal_control (type_name, ", " & S (last_text));
  end Ada_very_normal_control;

  function Static_tail return String is
    use GWindows.Static_Controls;
    border : Border_Type := None;
  begin
    if style_switch (simple_border) then
      border := Simple;
    elsif style_switch (half_sunken) then
      border := Half_Sunken; -- Window style: Static Edge (WS_EX_STATICEDGE)
    elsif style_switch (fully_sunken) then
      null; -- border:= Fully_Sunken; -- Window style: Client Edge (WS_EX_CLIENTEDGE)
    end if;
    return ", GWindows.Static_Controls." & Img (last_alignment) & ", " & Img (border);
  end Static_tail;

  procedure Ada_label_control is -- Static text
  begin
    if anonymous_item then
      Ada_Coord_conv (last_rect);
      Ada_Put_Line (to_spec, "    --  Label: " & S (last_ident));
      Ada_Put_Line (to_body,
        "    Create_Label (Window, " &
        S (last_text) &
        ", x, y, w, h" &
        Static_tail
        & ");"
      );
    else
      empty_dialog_record := False;
      Ada_normal_control (
        "Label_Type",
        ", " & S (last_text),
        Static_tail
      );
    end if;
  end Ada_label_control;

  procedure Ada_button_control is
    temp_ustr : Unbounded_String;
  begin
    if style_switch (state3) then
      Ada_very_normal_control ("Three_State_Box_Type");
    elsif style_switch (checkbox) then
      Ada_very_normal_control ("Check_Box_Type");
    elsif style_switch (radio) then
      Ada_very_normal_control ("Radio_Button_Type");
    elsif style_switch (bitmap) then
      Ada_very_normal_control ("Bitmap_Button_Type");
    elsif style_switch (icon) then
      Ada_very_normal_control ("Icon_Button_Type");
    elsif style_switch (ownerdraw) then
      Ada_very_normal_control ("Owner_Drawn_Button_Type");
    elsif style_switch (push) then
      Ada_Coord_conv (last_rect);
      --  Here it is a bit tricky, since, as expected,
      --  Dialog_Button's close the window and Button don't .
      --  If we want a "real", permanent, window, then we want
      --  the latter sort.
      ------------------------------------
      -- "Dialog" version of the button --
      ------------------------------------
      Ada_Put (to_spec, "    " & S (last_Ada_ident) & " : ");
      if style_switch (default) then
        Ada_Put (to_spec, "Default_");
      end if;
      Ada_Put_Line (to_spec, "Dialog_Button_Type;    --  Closes parent window after click");
      Ada_Put_Line (to_body, "    --  Both versions of the button are created.");
      Ada_Put_Line (to_body, "    --  The more meaningful one is made visible, but this choice");
      Ada_Put_Line (to_body, "    --  can be reversed, for instance on a ""Browse"" button.");
      Ada_normal_control_create (", " & S (last_text));
      ------------------------------------
      -- "Window" version of the button --
      ------------------------------------
      temp_ustr := last_Ada_ident;
      last_Ada_ident := U (S (last_Ada_ident) & "_permanent");
      Ada_Put (to_spec, "    " & S (last_Ada_ident) & " : ");
      if style_switch (default) then
        Ada_Put (to_spec, "Default_");
      end if;
      Ada_Put_Line (to_spec, "Button_Type;  --  Doesn't close parent window after click");
      Ada_normal_control_create (", " & S (last_text));
      Ada_Put_Line (to_body, "    if for_dialog then  --  Hide the non-closing button");
      Ada_Put_Line (to_body, "      Hide (Window." & S (last_Ada_ident) & ");");
      Ada_Put_Line (to_body, "    else  --  Hide the closing button");
      Ada_Put_Line (to_body, "      Hide (Window." & S (temp_ustr) & ");");
      Ada_Put_Line (to_body, "    end if;");
      if style_switch (disabled) then
        Ada_Put_Line (to_body, "    Enabled (Window." & S (last_Ada_ident) & ", False);");
        Ada_Put_Line (to_body, "    Enabled (Window." & S (temp_ustr) & ", False);");
      end if;
    end if;
    if style_switch (multi_line) then
      Ada_Put_Line (to_body, "    Multi_Line (Window." & S (last_Ada_ident) & ");");
      if temp_ustr /= "" then
        Ada_Put_Line (to_body, "    Multi_Line (Window." & S (temp_ustr) & ");");
      end if;
    end if;
    Ada_optional_disabling;
  end Ada_button_control;

  procedure Ada_edit_control is
  begin
    if style_switch (multi_line) then
      Ada_normal_control (
        "Multi_Line_Edit_Box_Type",
        ", " & S (last_text),
        ", " & Img (style_switch (auto_h_scroll))
      );
    else
      Ada_normal_control (
        "Edit_Box_Type",
        ", " & S (last_text),
        ", Horizontal_Scroll => " & Img (style_switch (auto_h_scroll)) &
        ", Read_Only => " & Img (style_switch (read_only))
      );
    end if;
    if not style_switch (simple_border) then
      Ada_Put_Line (to_body, "    Border (Window." & S (last_Ada_ident) & ", False);");
    end if;
  end Ada_edit_control;

  procedure Ada_list_box_control is
  begin
    Ada_normal_control (
      "List_Box_Type",
      "",
      ", " & Img (style_switch (sort))
    );
    if initialize_controls then
      Ada_Put_Line (to_body, "    for N in 0 .. 5 loop");
      Ada_Put_Line (to_body, "      Add (Window." & S (last_Ada_ident) &
        ", To_GString_From_String (""List_Box item Nr"" & N'Img));"
      );
      Ada_Put_Line (to_body, "    end loop;");
    end if;
  end Ada_list_box_control;

  procedure Ada_combo_control is
  begin
    if combo = drop_down_list then
      Ada_normal_control (
        Combo_type_name (combo),
        --  No Text parameter - otherwise the wrong Create is called,
        --  and a Combo Box is created instead of a Drop Down List Box!
        "",
        ", " & Img (style_switch (sort))
      );
    else
      Ada_normal_control (
        Combo_type_name (combo),
        ", " & S (last_text),
        ", " & Img (style_switch (sort))
      );
    end if;
    if initialize_controls then
      Ada_Put_Line (to_body, "    for N in 0 .. 5 loop");
      Ada_Put_Line (to_body, "      Add (Window." & S (last_Ada_ident) &
        ", To_GString_From_String (""Combo item Nr"" & N'Img));"
      );
      Ada_Put_Line (to_body, "    end loop;");
    end if;
  end Ada_combo_control;

  procedure Ada_icon_control is
    mem_alignment : constant GWindows.Static_Controls.Alignment_Type := last_alignment;
  begin
    if S (last_control_text) = """""" then
      null; -- phantom icon...
    else
      if style_switch (center_image) then
        if style_switch (real_size_image) then
          last_alignment := GWindows.Static_Controls.Static_Size;
        else
          last_alignment := GWindows.Static_Controls.Center;
        end if;
      elsif style_switch (right_justify) then
        last_alignment := GWindows.Static_Controls.Right;
      end if;
      Ada_normal_control (
        "Icon_Type",
        ", Num_resource (" & S (last_control_text) & ')',
        Static_tail,
        with_id => False
      );
      last_alignment := mem_alignment;
    end if;
  end Ada_icon_control;

  procedure Ada_bitmap_control is
  begin
    if S (last_control_text) = """""" then
      null; -- phantom bitmap...
    else
      Ada_normal_control (
        "Bitmap_Type",
         ", Num_resource (" & S (last_control_text) & ')',
         --  ^ direct resource name, as string
        Static_tail,
        with_id => False
      );
    end if;
  end Ada_bitmap_control;

  procedure Reset_control_styles is
  begin
    lv_type  := GWindows.Common_Controls.List_View;
    lv_select := GWindows.Common_Controls.Multiple; -- MSDN: By default, multiple items may be selected
    lv_sort  := GWindows.Common_Controls.No_Sorting;
    lv_auto_arrange := False;
    lv_align := GWindows.Common_Controls.Align_None;
  end Reset_control_styles;

  --  All that begin with CONTROL, e.g. CONTROL "" ,IDC_EDIT11,"EDIT", ...
  procedure Ada_untyped_control is
    use GWindows.Common_Controls;
  begin
    if control /= unknown then
      empty_dialog_record := False;
    end if;
    case control is
      when unknown =>
        Ada_Comment (to_spec, "    Unknown CONTROL Class: " & S (last_class));
      when static =>
        last_text := last_control_text;
        Ada_label_control;
      when button =>
        last_text := last_control_text;
        Ada_button_control;
      when edit =>
        last_text := last_control_text;
        Ada_edit_control;
      when icon =>
        Ada_icon_control;
      when bitmap =>
        Ada_bitmap_control;
      when track_bar =>
        Ada_normal_control (
          "Trackbar_Control_Type",
           "",
           ", " & Img (Trackbar_Control_Ticks) &
           ", " & Img (Control_Direction) &
           ", Tips => " & Img (style_switch (tips)),
           with_id => False
        );
      when up_down =>
        Ada_normal_control (
          "Up_Down_Control_Type",
           "",
           ", " & Img (style_switch (keys)) &
           ", " & Img (Control_Direction) &
           ", " & Img (style_switch (wrap)) &
           ", Auto_Buddy => False" &
           ", Thousands => " & Img (not style_switch (no_1000)),
           with_id => False
        );
      when progress =>
        Ada_normal_control (
          "Progress_Control_Type",
           "",
           ", " & Img (Control_Direction) &
           ", " & Img (style_switch (smooth)),
          with_id => False
        );
        if initialize_controls then
          Ada_Put_Line (to_body, "    Position (Window." & S (last_Ada_ident) &
            ", 66);  --  reminds to initialize; nice for testing"
          );
        end if;
      when list_view | SysListView32 =>
        Ada_normal_control (
          "List_View_Control_Type",
          "",
          ", " & Img (lv_select) &
          ", " & Img (lv_type) &
          ", " & Img (lv_sort) &
          ", " & Img (lv_auto_arrange) &
          ", " & Img (lv_align),
          with_id => False
       );
        if initialize_controls then
          Ada_Put_Line (to_body, "    --  Initialize_controls");
          Ada_Put_Line (to_body, "    Insert_Column (Window." & S (last_Ada_ident)  & ", ""Item"", 0, 75);");
          if lv_type = Report_View then
            Ada_Put_Line (to_body, "    Insert_Column (Window." & S (last_Ada_ident)  & ", ""Sub_Item"", 1, 100);");
          end if;
          Ada_Put_Line (to_body, "    for N in 0 .. 25 loop");
          Ada_Put_Line (to_body, "       Insert_Item (Window." & S (last_Ada_ident)  & ", To_GString_From_String (""Item Nb"" & N'Img), N);");
          if lv_type = Report_View then
            Ada_Put_Line (to_body, "       Set_Sub_Item (Window." & S (last_Ada_ident)  & ",");
            Ada_Put_Line (to_body, "                     To_GString_From_String (""Sub of"" & N'Img),");
            Ada_Put_Line (to_body, "                     N,");
            Ada_Put_Line (to_body, "                    1);");
          end if;
          Ada_Put_Line (to_body, "    end loop;");
        end if;
      when tree_view | SysTreeView32 =>
        Ada_normal_control (
          "Tree_View_Control_Type",
          "",
          ", Buttons=> " & Img (style_switch (has_buttons)) &
          ", Lines => " & Img (style_switch (has_lines)) &
          ", Lines_At_Root => " & Img (style_switch (lines_at_root)) &
          ", Single_Expand => " & Img (style_switch (single_expand)),
          with_id => False
        );
        if initialize_controls then
          Ada_Put_Line (to_body, "    declare  --  Initialize_controls");
          Ada_Put_Line (to_body, "       Root_Node : Tree_Item_Node;");
          Ada_Put_Line (to_body, "       An_Item   : Tree_Item_Node;");
          Ada_Put_Line (to_body, "    begin");
          Ada_Put_Line (to_body, "       Insert_Item (Window." & S (last_Ada_ident)  & ", ""Root"", 0, Root_Node, As_A_Root);");
          Ada_Put_Line (to_body, "       Insert_Item (Window." & S (last_Ada_ident)  & ", ""Child 1"", Root_Node, An_Item);");
          Ada_Put_Line (to_body, "       Insert_Item (Window." & S (last_Ada_ident)  & ", ""Child 2"", Root_Node, An_Item);");
          Ada_Put_Line (to_body, "       Insert_Item (Window." & S (last_Ada_ident)  & ", ""Child 3"", Root_Node, An_Item);");
          Ada_Put_Line (to_body, "       Insert_Item (Window." & S (last_Ada_ident)  & ", ""Sub-Child 1"", An_Item, An_Item);");
          Ada_Put_Line (to_body, "       Insert_Item (Window." & S (last_Ada_ident)  & ", ""Sub-Child 2"", An_Item, An_Item);");
          Ada_Put_Line (to_body, "       Insert_Item (Window." & S (last_Ada_ident)  & ", ""Sub-Child 3"", An_Item, An_Item);");
          Ada_Put_Line (to_body, "       Expand (Window." & S (last_Ada_ident)  & ", Root_Node);");
          Ada_Put_Line (to_body, "    end;");
        end if;
      when tab_control =>
        Ada_normal_control ("Tab_Window_Control_Type", with_id => False);
        --  Tab_Window_Control_Type allows to associate a window
        --  to a tab via the Tab_Window method
      when date_time =>
        Ada_normal_control (
          "Date_Time_Picker_Type",
           "",
           ", Method=> Up_Down",
          with_id => False
        );
      when calendar =>
        Ada_normal_control (
          "Date_Time_Picker_Type",
           "",
           ", Method => Calendar",
          with_id => False
        );
    end case;
    Ada_optional_disabling;
  end Ada_untyped_control;

  procedure Ada_optional_disabling is
  begin
    if style_switch (disabled) then
      Ada_Put_Line (to_body, "    Disable (Window." & S (last_Ada_ident) & ");");
    end if;
    if style_switch (hidden) then
      Ada_Put_Line (to_body, "    Hide (Window." & S (last_Ada_ident) & ");");
    end if;
  end Ada_optional_disabling;

  --  Control class is given as a string, not a token (e.g. "Button")
  procedure Identify_control_class (RC_String : String) is
  begin
    for c in Control_type loop
      if To_Upper (RC_String) = '"' & To_Upper (Control_type'Image (c)) & '"' then
        control := c;
        exit;
      end if;
    end loop;
  end Identify_control_class;

  procedure Test_Generation is
    root_pkg : constant String := Root_name (S (source_name));
  begin
    Create (to_body, To_Lower (root_pkg & "_Test_App.adb"));
    Ada_Put_Line (to_body, "--  GWindows test application, generated by GWenerator");
    Ada_New_Line (to_body);
    Ada_Put_Line (to_body, "with GWindows.Application,");
    Ada_Put_Line (to_body, "     GWindows.Base,");
    Ada_Put_Line (to_body, "     GWindows.Edit_Boxes,");
    Ada_Put_Line (to_body, "     GWindows.Message_Boxes,");
    Ada_Put_Line (to_body, "     GWindows.Windows;");
    Ada_New_Line (to_body);
    Ada_Put_Line (to_body, "with " & root_pkg & "_Resource_GUI;");
    Ada_New_Line (to_body);
    Ada_Put_Line (to_body, "procedure " & root_pkg & "_Test_App is");
    Ada_Put_Line (to_body, "  use " & root_pkg & "_Resource_GUI;");
    Ada_Put_Line (to_body, "  use GWindows.Base, GWindows.Edit_Boxes,");
    Ada_Put_Line (to_body, "      GWindows.Message_Boxes, GWindows.Windows;");
    Ada_New_Line (to_body);
    Ada_Put_Line (to_body, "  pragma Linker_Options (""-mwindows"");");
    Ada_New_Line (to_body);
    for i in 1 .. dialog_top loop
      Ada_Put_Line (to_body,
        "  Dlg_" & Trim (Integer'Image (i), Both) &
        "     : " & root_pkg & "_Resource_GUI." & S (dialog_mem (i)) & ";"
      );
    end loop;
    Ada_Put_Line (to_body, "  Result    : Integer;");
    Ada_Put_Line (to_body, "  No_Parent : Window_Type;");
    Ada_New_Line (to_body);
    Ada_Put_Line (to_body, "begin");
    for i in 1 .. dialog_top loop
      declare
        dlg : constant String :=  "Dlg_" & Trim (Integer'Image (i), Both);
      begin
        Ada_Put_Line (to_body, "  Create_Full_Dialog (" & dlg & ", No_Parent);");
        Ada_Put_Line (to_body, "  Center(" & dlg & ");");
        Ada_Put_Line (to_body, "  Result := GWindows.Application.Show_Dialog (" & dlg & ");");
      end;
    end loop;
    Ada_Put_Line (to_body, "end " & root_pkg & "_Test_App;");
    Close (Ada_files (to_body));
  end Test_Generation;

  procedure YY_Accept is
  begin
    if separate_items then
      Create (to_spec, pkg (as_file_name => True) & "-constants.ads");
      Blurb (to_spec);
      Ada_Put_Line (to_spec, "--  Constants from resource.h or equivalent");
      Ada_New_Line (to_spec);
      Ada_Put_Line (to_spec, "package " & pkg & ".Constants is");
      Output_symbols;
      Ada_Put_Line (to_spec, "end " & pkg & ".Constants;");
      Close (Ada_files (to_spec));
      --
      Create (to_body, pkg (as_file_name => True) & "-helpers.adb");
      Ada_Put_Line (to_body, "with GWindows.Types;              use GWindows.Types;");
      Ada_Put_Line (to_body, "with GWindows.Drawing_Objects;");
      Ada_Put_Line (to_body, "with GWindows.GStrings;                 use GWindows.GStrings;");
      Ada_Put_Line (to_body, "with Interfaces.C;                      use Interfaces.C;");
      Ada_Put_Line (to_body, "with System;");
      Ada_New_Line (to_body);
      Ada_Put_Line (to_body, "package body " & pkg & ".Helpers is");
    else
      Ada_New_Line (to_spec);
      Output_symbols;
      Ada_New_Line (to_spec);
      Ada_Put_Line (to_spec, "  --  ** Some helper utilities (spec).");
      Ada_Helpers_spec (to_spec);
      Ada_New_Line (to_body);
      Ada_Put_Line (to_body, "  --  ** Generated code ends here /\ /\ /\.");
      Ada_New_Line (to_body);
      Ada_Put_Line (to_body, "  --  ** Some helper utilities (body).");
    end if;
    Ada_New_Line (to_body);
    Ada_Put_Line (to_body, "  procedure Dlg_to_Scn (  --  Converts dialog coords to screen (pixel) coordinates.");
    Ada_Put_Line (to_body, "    xd, yd, wd, hd :  in Integer;");
    Ada_Put_Line (to_body, "    xs, ys, ws, hs : out Integer)");
    Ada_Put_Line (to_body, "  is");
    Ada_Put_Line (to_body, "    --  function GetDialogBaseUnits return Integer;");
    Ada_Put_Line (to_body, "    --  pragma Import (StdCall, GetDialogBaseUnits, ""GetDialogBaseUnits"");");
    Ada_Put_Line (to_body, "    --  baseunit, baseunitX, baseunitY: Integer;");
    Ada_Put_Line (to_body, "    baseunitX : constant :=" & Positive'Image (base_unit_x) & ';');
    Ada_Put_Line (to_body, "    baseunitY : constant :=" & Positive'Image (base_unit_y) & ';');
    Ada_Put_Line (to_body, "  begin");
    Ada_Put_Line (to_body, "    --  baseunit := GetDialogBaseUnits; -- this gives X=8, Y=16 (SYSTEM font)");
    Ada_Put_Line (to_body, "    --  baseunitX := baseunit mod (2 ** 16);");
    Ada_Put_Line (to_body, "    --  baseunitY := baseunit  / (2 ** 16);");
    Ada_Put_Line (to_body, "    --  NB: the other way with MapDialogRect works only");
    Ada_Put_Line (to_body, "    --    by full moon, hence the user-defined units.");
    Ada_Put_Line (to_body, "    xs := (xd * baseunitX) / 4;");
    Ada_Put_Line (to_body, "    ws := (wd * baseunitX) / 4;");
    Ada_Put_Line (to_body, "    ys := (yd * baseunitY) / 8;");
    Ada_Put_Line (to_body, "    hs := (hd * baseunitY) / 8;");
    Ada_Put_Line (to_body, "  end Dlg_to_Scn;");
    Ada_New_Line (to_body);
    Ada_Put_Line (to_body, "  package Common_Fonts is");
    Ada_Put_Line (to_body, "    GUI_Font : GWindows.Drawing_Objects.Font_Type;");
    Ada_Put_Line (to_body, "    URL_Font : GWindows.Drawing_Objects.Font_Type;");
    Ada_Put_Line (to_body, "    --  ^ These fonts are created once, at startup");
    Ada_Put_Line (to_body, "    --    it avoid GUI resource leak under Windows 95/98/ME");
    Ada_Put_Line (to_body, "    procedure Create_Common_Fonts;");
    Ada_Put_Line (to_body, "    --  in initialisation part if this pkg becomes standalone");
    Ada_Put_Line (to_body, "  end Common_Fonts;");
    Ada_New_Line (to_body);
    Ada_Put_Line (to_body, "  procedure Use_GUI_Font (Window : in out GWindows.Base.Base_Window_Type'Class)");
    Ada_Put_Line (to_body, "  is");
    Ada_Put_Line (to_body, "  begin");
    Ada_Put_Line (to_body, "    --  Use Standard Windows GUI font instead of system font");
    Ada_Put_Line (to_body, "    GWindows.Base.Set_Font (Window, Common_Fonts.GUI_Font);");
    Ada_Put_Line (to_body, "  end Use_GUI_Font;");
    Ada_New_Line (to_body);
    Ada_Put_Line (to_body, "  function Num_resource (id : Natural) return GString is");
    Ada_Put_Line (to_body, "    img : constant String := Integer'Image (id);");
    Ada_Put_Line (to_body, "  begin");
    Ada_Put_Line (to_body, "    return To_GString_From_String ('#' & img (img'First + 1 .. img'Last));");
    Ada_Put_Line (to_body, "  end Num_resource;");
    Ada_New_Line (to_body);
    Ada_Put_Line (to_body, "  package body Common_Fonts is");
    Ada_New_Line (to_body);
    Ada_Put_Line (to_body, "    procedure Create_Common_Fonts is");
    Ada_New_Line (to_body);
    Ada_Put_Line (to_body, "     type Face_Name_Type is array (1 .. 32) of GWindows.GChar_C;");
    Ada_New_Line (to_body);
    Ada_Put_Line (to_body, "     type LOGFONT is record");
    Ada_Put_Line (to_body, "       lfHeight         : Interfaces.C.long;");
    Ada_Put_Line (to_body, "       lfWidth          : Interfaces.C.long;");
    Ada_Put_Line (to_body, "       lfEscapement     : Interfaces.C.long;");
    Ada_Put_Line (to_body, "       lfOrientation    : Interfaces.C.long;");
    Ada_Put_Line (to_body, "       lfWeight         : Interfaces.C.long;");
    Ada_Put_Line (to_body, "       lfItalic         : Interfaces.C.char;");
    Ada_Put_Line (to_body, "       lfUnderline      : Interfaces.C.char;");
    Ada_Put_Line (to_body, "       lfStrikeOut      : Interfaces.C.char;");
    Ada_Put_Line (to_body, "       lfCharSet        : Interfaces.C.char;");
    Ada_Put_Line (to_body, "       lfOutPrecision   : Interfaces.C.char;");
    Ada_Put_Line (to_body, "       lfClipPrecision  : Interfaces.C.char;");
    Ada_Put_Line (to_body, "       lfQuality        : Interfaces.C.char;");
    Ada_Put_Line (to_body, "       lfPitchAndFamily : Interfaces.C.char;");
    Ada_Put_Line (to_body, "       lfFaceName       : Face_Name_Type;");
    Ada_Put_Line (to_body, "     end record;");
    Ada_New_Line (to_body);
    Ada_Put_Line (to_body, "     Log_of_current_font : aliased LOGFONT;");
    Ada_New_Line (to_body);
    Ada_Put_Line (to_body, "     subtype PVOID   is System.Address;                      --  winnt.h");
    Ada_Put_Line (to_body, "     subtype LPVOID  is PVOID;                               --  windef.h");
    Ada_New_Line (to_body);
    Ada_Put_Line (to_body, "     function GetObject");
    Ada_Put_Line (to_body, "       (hgdiobj   : GWindows.Types.Handle := GWindows.Drawing_Objects.Handle (GUI_Font);");
    Ada_Put_Line (to_body, "        cbBufferl : Interfaces.C.int      := LOGFONT'Size / 8;");
    Ada_Put_Line (to_body, "        lpvObject : LPVOID                := Log_of_current_font'Address)");
    Ada_Put_Line (to_body, "       return Interfaces.C.int;");
    Ada_Put_Line (to_body, "     pragma Import (StdCall, GetObject,");
    Ada_Put_Line (to_body, "                      ""GetObject"" & Character_Mode_Identifier);");
    Ada_New_Line (to_body);
    Ada_Put_Line (to_body, "     function CreateFontIndirect");
    Ada_Put_Line (to_body, "       (lpvObject : LPVOID                := Log_of_current_font'Address)");
    Ada_Put_Line (to_body, "       return GWindows.Types.Handle;");
    Ada_Put_Line (to_body, "     pragma Import (StdCall, CreateFontIndirect,");
    Ada_Put_Line (to_body, "                      ""CreateFontIndirect"" & Character_Mode_Identifier);");
    Ada_New_Line (to_body);
    Ada_Put_Line (to_body, "    begin");
    Ada_Put_Line (to_body, "      GWindows.Drawing_Objects.Create_Stock_Font");
    Ada_Put_Line (to_body, "        (GUI_Font,");
    Ada_Put_Line (to_body, "         GWindows.Drawing_Objects.Default_GUI);");
    Ada_Put_Line (to_body, "      if GetObject = 0 then");
    Ada_Put_Line (to_body, "        GWindows.Drawing_Objects.Create_Font (URL_Font,");
    Ada_Put_Line (to_body, "          ""MS Sans Serif"",");
    Ada_Put_Line (to_body, "          14, Underline => True);");
    Ada_Put_Line (to_body, "            --  !! ^ Not so nice (non-unsharpened font, size ~..., color ?)");
    Ada_Put_Line (to_body, "      else");
    Ada_Put_Line (to_body, "        Log_of_current_font.lfUnderline := Interfaces.C.char'Val (1);");
    Ada_Put_Line (to_body, "        GWindows.Drawing_Objects.Handle (URL_Font, CreateFontIndirect);");
    Ada_Put_Line (to_body, "      end if;");
    Ada_Put_Line (to_body, "    end Create_Common_Fonts;");
    Ada_New_Line (to_body);
    Ada_Put_Line (to_body, "  end Common_Fonts;");
    Ada_New_Line (to_body);
    Ada_Put_Line (to_body, "begin");
    Ada_Put_Line (to_body, "  Common_Fonts.Create_Common_Fonts;");
    for to in Pkg_output loop
      if not (to = to_spec and separate_items) then
        Ada_New_Line (to);
        Ada_Put_Line (to, "  --  Last line of resource script file:" & Integer'Image (linenum));
        Ada_New_Line (to);
        if separate_items then
          Ada_Put_Line (to, "end " & pkg & ".Helpers;");
        else
          Ada_Put_Line (to, "end " & pkg & ';');
        end if;
        Close (Ada_files (to));
      end if;
    end loop;
    Delete (symbols_by_name);
    Delete (symbols_by_value);
    if generate_test then
      Test_generation;
    end if;
  end YY_Accept;

  procedure YY_Abort is
  begin
    Put_Line (Current_Error, "YY_Abort");
  end YY_Abort;

  procedure YY_Terminate is
  begin
    Put_Line (Current_Error, "YY_Terminate");
  end YY_Terminate;

  procedure RC_Comment (s : String) is
  begin
    null;
  end RC_Comment;

  procedure Reset_Globals is
  begin
    has_input := False;
    source_name := U ("");
    GWen_proj := U ("");
    linenum := 0;
    --
    base_unit_x := 6;   --  usual value, overriden with option -x
    base_unit_y := 13;  --  usual value, overriden with option -y
    separate_items := False;
    generate_test  := False;
    initialize_controls := False;
    --
    first_include := True;
    dialog_top := 0;
    --
    --  Initialize the symbol dictionaries with common symbols (See GWindows.Constants)
    Insert_Common ("IDOK",     1);
    Insert_Common ("IDCANCEL", 2);
    Insert_Common ("IDABORT",  3);
    Insert_Common ("IDRETRY",  4);
    Insert_Common ("IDIGNORE", 5);
    Insert_Common ("IDYES",    6);
    Insert_Common ("IDNO",     7);
    Insert_Common ("IDCLOSE",  8);
    Insert_Common ("IDHELP",   9);
  end Reset_Globals;

end RC_Help;
