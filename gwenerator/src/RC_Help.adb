--------------------------------------------------------------------------
--  RC_Help.adb
--
--  Helper for the MS Windows Resource Compier script parser
--
--  Copyright (c) Gautier de Montmollin 2008..2009
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

-- with Ada.Command_Line;                  use Ada.Command_Line;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Characters.Handling;           use Ada.Characters.Handling;
with Ada.Strings.Fixed;                 use Ada.Strings, Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with Time_Log;
with Resource_Header;                   use Resource_Header;

package body RC_Help is

  function Ada_ify(s: String) return Unbounded_String is
    us: Unbounded_String:= U(s);
    i: Natural;
  begin
    loop
      i:= Index(us, "__");
      exit when i = 0;
      Delete(us, i, i);
    end loop;
    return us;
  end Ada_ify;

  function Combo_type_name(combo: Combo_type) return String is
  begin
    case combo is
      when no_drop => return "Combo_Box_Type";
      when drop_down => return "Drop_Down_Combo_Box_Type";
      when drop_down_list => return "Drop_Down_List_Box_Type";
    end case;
  end Combo_type_name;

  function Image( i: Long_Long_Integer ) return String is
  begin
    return Trim(Long_Long_Integer'Image(i),both);
  end Image;

  function Image(rect: Rect_type) return String is
  begin
    return
      Image(rect.x) & ',' &
      Image(rect.y) & ',' &
      Image(rect.w) & ',' &
      Image(rect.h);
  end Image;

  procedure iPut(to: Pkg_output; s: String; as_comment: Boolean);

  procedure iNew_Line(to: Pkg_output) is
  begin
    Ada.Text_IO.New_Line(Ada_files(to));
    iPut(to, "", as_comment=> False);
  end iNew_Line;

  procedure iPut(to: Pkg_output; s: String; as_comment: Boolean) is
    pr: Boolean:= False;
  begin
    if as_comment then
      for i in s'Range loop
        case s(i) is
          when ASCII.CR | ASCII.LF =>
            if not pr then -- avoid pairs
              iNew_Line(to);
              iPut(to, "-- ", as_comment => False);
            end if;
            pr:= True;
          when others =>
            Ada.Text_IO.Put(Ada_files(to), s(i));
            pr:= False;
        end case;
      end loop;
    else
      Ada.Text_IO.Put(Ada_files(to), s);
    end if;
  end iPut;

  procedure iPut_Line(to: Pkg_output; s: String; as_comment: Boolean) is
  begin
    iPut(to, s, as_comment);
    iNew_Line(to);
  end iPut_Line;

  procedure Ada_Put(to: Pkg_output; s: String) is
  begin
    iPut(to, s, as_comment => False);
  end;

  procedure Ada_Put_Line(to: Pkg_output; s: String) is
  begin
    iPut_Line(to, s, as_comment => False);
  end;

  procedure Ada_New_Line(to: Pkg_output) is
  begin
    iNew_Line(to);
  end;

  procedure Ada_Comment(to: Pkg_output; s: String) is
  begin
    iPut_Line(to, "-- " & s, as_comment => True);
  end;

  function Root_name(sn: String) return String is
  begin
    for i in reverse sn'Range loop
      if sn(i)='.' then -- skip file extension, if any
        return sn(sn'First..i-1);
      end if;
    end loop;
    return sn;
  end Root_Name;

  function RC_to_Package_name(
    rc_name     : String;
    has_input   : Boolean;
    as_file_name: Boolean
  ) return String
  is
    sep_child: constant array(Boolean) of Character:= ('.', '-');
    as_child: constant Boolean:= False;
    -- problem with making a the package a child: parent may not be a procedure
    sep: constant array(Boolean) of Character:= ('_', sep_child(as_file_name));
    suffix: constant String:= sep(as_child) & "Resource_GUI";
  begin
    if has_input then
      return Root_Name(rc_name) & suffix;
    else
      return "Input" & suffix;
    end if;
  end RC_to_Package_name;

  function pkg(as_file_name: Boolean:= False) return String is
  begin
    return RC_to_Package_name(S(source_name), has_input, as_file_name);
  end pkg;

  function Image_zeros(n: Natural) return String is
    im: constant String:= Integer'Image(n+10_000);
  begin
    return im(im'Last-3..im'Last);
  end Image_zeros;

  function Popup_num_to_Ada_ident(n: Natural) return String is
  begin
    if n = 0 then
      return "Main";
    else
      return "Popup_" & Image_zeros(n);
    end if;
  end Popup_num_to_Ada_ident;

  procedure New_static_item is
  begin
    static_counter:= static_counter + 1;
    last_Ada_ident:= U("Static_" & Image_zeros(static_counter));
  end New_static_item;

  ------------------------------------
  -- Management of resource symbols --
  ------------------------------------

  type Dir_node;
  type p_Dir_node is access Dir_node;

  type Entry_type is record
    RC_Ident    : Unbounded_String;
    Ada_constant: Unbounded_String;
    value       : Integer;
    common      : Boolean;  -- designates common Win32 ID's (See GWindows.Constants)
  end record;

  type Dir_node is record
    left, right : p_Dir_node;
    item        : Entry_type;
  end record;

  procedure Dispose is new Ada.Unchecked_Deallocation( Dir_node, p_Dir_node );

  procedure Delete( p: in out p_Dir_node ) is
  begin
    if p/=null then
       Delete(p.left);
       Delete(p.right);
       Dispose(p);
       p:= null;
    end if;
  end Delete;

  type Sorting_mode is (by_name, by_value);

  -- We need two dictionaries
  -- - by value, for the nice sorted output
  -- - by name, for avoiding duplicating names

  symbols_by_name, symbols_by_value: p_Dir_node:= null;

  procedure Insert( item :  in     Entry_type;
                    node :  in out p_Dir_node;
                    mode :  in     sorting_mode ) is
  begin
    if node = null then
      node:= new Dir_node'
        ( (item         => item,
           left         => null,
           right        => null
           )
        );
      if mode = by_name then
        -- This new name is unique, let's insert by value too
        Insert( item, symbols_by_value, by_value );
      end if;
    else
      case mode is
        when by_value =>
          if item.value >= node.item.value then
             -- '=': values may be duplicate (several constants with same values)
            Insert( item, node.right, mode );
          else
            Insert( item, node.left , mode );
          end if;
        when by_name =>
          if item.RC_ident > node.item.RC_ident then
            Insert( item, node.right, mode );
          elsif item.RC_ident < node.item.RC_ident then
            Insert( item, node.left , mode );
          else
            -- duplicated names are ignored
            null;
          end if;
      end case;
    end if;
  end Insert;

  procedure Insert_last_symbol is
  begin
    if not anonymous_item then
      Insert( (last_ident, last_Ada_constant, -1, False), symbols_by_name, by_name );
      -- In principle already inserted via resource.h, unless that one is missing
    end if;
  end Insert_last_symbol;

  procedure Insert_symbol(sym_name: String; value: Integer) is
  begin
    Insert( (U(sym_name), Ada_ify(sym_name), value, False), symbols_by_name, by_name );
  end Insert_symbol;

  procedure Insert_Common(symb: String; value: Integer) is
  begin
    Insert( (U(symb), U(symb), value, True), symbols_by_name, by_name );
  end Insert_Common;

  first_include: Boolean;

  procedure Treat_include(fn: String) is
    done: Boolean;
  begin
    if first_include then
      Convert_Header_File (fn, done);
      if done then
        first_include:= False; -- otherwise, we try with the next one, if any...
      end if;
    end if;
  end Treat_include;


  procedure Output_symbols is

    max_name, max_img: Integer:= 0;

    procedure Set_max( p: p_Dir_node ) is
    begin
      if p /= null then
        Set_max(p.left);
        --
        max_name:= Integer'Max(max_name, S(p.item.Ada_constant)'Length);
        max_img := Integer'Max(max_img,  Integer'Image(p.item.value)'Length);
        --
        Set_max(p.right);
      end if;
    end Set_max;

    procedure Output_node_and_sons( p: p_Dir_node ) is
    begin
      if p /= null then
        Output_node_and_sons(p.left);
        --
        declare
          nm: constant String:= S(p.item.Ada_constant);
          im: constant String:= Integer'Image(p.item.value);
        begin
          if p.item.common then
            null;  -- Do not output ID's already available in GWindows.Constants
          else
            Ada_Put_Line(to_spec,
               "  " & nm & (max_name - nm'Length) * ' ' &
               ": constant:= " & (max_img - im'Length) * ' ' & im & ';'
            );
          end if;
        end;
        Output_node_and_sons(p.right);
      end if;
    end Output_node_and_sons;

  begin
    Ada_Put_Line(to_spec, "  ------------------------------------------------");
    Ada_Put_Line(to_spec, "  -- Defined resource symbols --> Ada constants --");
    Ada_Put_Line(to_spec, "  ------------------------------------------------");
    Ada_New_Line(to_spec);
    Ada_Put_Line(to_spec, "  -- NB: only items with a defined symbol get a constant here");
    Ada_Put_Line(to_spec, "  -- These constants are needed for getting button and menu feedbacks.");
    Ada_New_Line(to_spec);
    Set_max(symbols_by_value);
    Output_node_and_sons(symbols_by_value);
  end Output_symbols;

  --

  procedure Blurb(to: Pkg_output) is
  begin
    Ada_Put_Line(to, "---------------------------------------------------------------------");
    Ada_Put_Line(to, "-- GUI contents of resource script file: " & S(source_name));
    Ada_Put_Line(to, "-- Transcription time: " & Time_Log);
    Ada_Put_Line(to, "--");
    Ada_Put_Line(to, "-- Translated by the RC2GW or GWenerator tools.");
    Ada_Put_Line(to, "-- URL: http://sf.net/projects/gnavi");
    Ada_Put_Line(to, "--");
    Ada_Put_Line(to, "-- This is automatically generated code. Do not edit this.");
    Ada_Put_Line(to, "-- Rework the resource instead, and re-run the translator.");
    Ada_Put_Line(to, "-- RC Grammar version: " & Grammar_Version);
    Ada_Put_Line(to, "---------------------------------------------------------------------");
    Ada_New_Line(to);
  end Blurb;

  procedure Ada_package_headers(eventual_child: String) is
  begin
    Blurb(to_spec);
    Blurb(to_body);
    --
    Ada_Put_Line(to_spec, "with GWindows.Base;                     use GWindows.Base;");
    Ada_Put_Line(to_spec, "with GWindows.Constants;                use GWindows.Constants;");
    Ada_Put_Line(to_spec, "with GWindows.Windows;                  use GWindows.Windows;");
    Ada_Put_Line(to_spec, "with GWindows.Buttons;                  use GWindows.Buttons;");
    Ada_Put_Line(to_spec, "with GWindows.Edit_Boxes;               use GWindows.Edit_Boxes;");
    Ada_Put_Line(to_spec, "with GWindows.List_Boxes;               use GWindows.List_Boxes;");
    Ada_Put_Line(to_spec, "with GWindows.Combo_Boxes;              use GWindows.Combo_Boxes;");
    Ada_Put_Line(to_spec, "with GWindows.Static_Controls;          use GWindows.Static_Controls;");
    Ada_Put_Line(to_spec, "with GWindows.Scroll_Bars;              use GWindows.Scroll_Bars;");
    Ada_Put_Line(to_spec, "with GWindows.Common_Controls;          use GWindows.Common_Controls;");
    Ada_Put_Line(to_spec, "with GWindows.Menus;                    use GWindows.Menus;");
    Ada_Put_Line(to_spec, "use GWindows;");
    Ada_New_Line(to_spec);
    Ada_Put_Line(to_spec, "package " & pkg & eventual_child &" is");
    Ada_New_Line(to_spec);
    --
    Ada_Put_Line(to_body, "with GWindows.Types;                    use GWindows.Types;");
    Ada_Put_Line(to_body, "with GWindows.Drawing;                  use GWindows.Drawing;");
    Ada_Put_Line(to_body, "with GWindows.Drawing_Objects;");
    Ada_Put_Line(to_body, "with Interfaces.C;                      use Interfaces.C;");
    Ada_Put_Line(to_body, "with System;");
    Ada_New_Line(to_body);
    if separate_items then
      Ada_Put_Line(to_body, "with " & pkg & ".Helpers; ");
      Ada_Put_Line(to_body, " use " & pkg & ".Helpers; ");
      Ada_Put_Line(to_body, "with " & pkg & ".Constants; ");
      Ada_Put_Line(to_body, " use " & pkg & ".Constants; ");
    end if;

    Ada_Put_Line(to_body, "package body " & pkg & eventual_child & " is");
    Ada_New_Line(to_body);
  end Ada_package_headers;

  procedure Create(on_pkg: pkg_output; name: String) is
  begin
    Create(Ada_files(on_pkg), Out_File, name);
    Put_Line(Current_Error, "Writing: " & name);
  end Create;

  procedure Open_if_separate(item: String; with_body: Boolean:= True) is
  begin
    if separate_items then
      Create(to_spec, pkg(as_file_name => True) & '-' & item & ".ads");
      if with_body then
        Create(to_body, pkg(as_file_name => True) & '-' & item & ".adb");
        Ada_package_headers(eventual_child => '.' & item);
      else
        Ada_Put_Line(to_spec, "package " & pkg & '.' & item & " is");
        Ada_New_Line(to_spec);
      end if;
    end if;
  end Open_if_separate;

  procedure Close_if_separate(item: String; with_body: Boolean:= True) is
  begin
    if separate_items then
      for to in pkg_output loop
        if to = to_spec or with_body then
          Ada_New_Line(to);
          Ada_Put_Line(to, "end " & pkg & '.' & item & ';');
          Close(Ada_files(to));
        end if;
      end loop;
    end if;
  end Close_if_separate;

  dialog_mem: array(1..10_000) of Unbounded_String;
  dialog_top: Natural:= 0;

  procedure Ada_Proc_Dialog(
    to          : Pkg_output;
    type_name   : String;
    title       : String
  )
  is
  begin
    -- First, finish defining type
    if to = to_spec then
      dialog_top:= dialog_top + 1;
      dialog_mem(dialog_top):= U(type_name);
      if empty_dialog_record then
        Ada_Put_Line(to_spec, "    null; -- empty!");
      end if;
      Ada_Put_Line(to_spec,
        "  end record; -- " & S(last_dialog_ident) & "_Type"
      );
      Ada_New_Line(to_spec);
    end if;

    Ada_Put_Line(to, "  -- Dialog at resource line" & Integer'Image(linenum));
    Ada_Put_Line(to, "  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog");
    Ada_New_Line(to);
    Ada_Put_Line(to, "  procedure Create_Full_Dialog");
    Ada_Put_Line(to, "     (Window      : in out " & type_name & ";");
    Ada_Put_Line(to, "      Parent      : in out GWindows.Base.Base_Window_Type'Class;");
    Ada_Put_Line(to, "      Title       : in     GString := " & title & ";");
    Ada_Put_Line(to, "      Left        : in     Integer := Use_Default; -- Default = as designed");
    Ada_Put_Line(to, "      Top         : in     Integer := Use_Default; -- Default = as designed");
    Ada_Put_Line(to, "      Width       : in     Integer := Use_Default; -- Default = as designed");
    Ada_Put_Line(to, "      Height      : in     Integer := Use_Default; -- Default = as designed");
    Ada_Put_Line(to, "      Help_Button : in     Boolean := False;");
    Ada_Put(to,      "      Is_Dynamic  : in     Boolean := False)");
    case to is
      when to_spec =>
        Ada_Put_Line(to, ";");
      when to_body =>
        Ada_New_Line(to_body);
        Ada_Put_Line(to_body, "  is");
        Ada_Put_Line(to_body, "    x,y,w,h: Integer;");
        Ada_Put_Line(to_body, "  begin");
        Ada_Coord_conv(last_dialog_rect);
        Ada_Put_Line(to_body, "    if Left   /= Use_Default then x:= Left;   end if;" );
        Ada_Put_Line(to_body, "    if Top    /= Use_Default then y:= Top;    end if;" );
        Ada_Put_Line(to_body, "    if Width  /= Use_Default then w:= Width;  end if;" );
        Ada_Put_Line(to_body, "    if Height /= Use_Default then h:= Height; end if;" );
        Ada_Put_Line(to_body,
           "    Create_As_Dialog("
        );
        Ada_Put_Line(to_body, "      Window => Window_Type(Window),");
        Ada_Put_Line(to_body, "      Parent => Parent,");
        Ada_Put_Line(to_body, "      Title  => Title,");
        Ada_Put_Line(to_body, "      Left   => x,");
        Ada_Put_Line(to_body, "      Top    => y,");
        Ada_Put_Line(to_body, "      Width  => w,");
        Ada_Put_Line(to_body, "      Height => h,");
        Ada_Put_Line(to_body, "      Help_Button => Help_Button,");
        Ada_Put_Line(to_body, "      Is_Dynamic  => Is_Dynamic");
        Ada_Put_Line(to_body, "    );");
        Ada_Put_Line(to_body, "    if Width = Use_Default then Client_Area_Width(Window, w); end if;");
        Ada_Put_Line(to_body, "    if Height = Use_Default then Client_Area_Height(Window, h); end if;");
        if style_switch(shell_font) then
          Ada_Put_Line(to_body, "    Use_GUI_Font(Window);");
        end if;
        Ada_Put_Line(to_body, "    Create_Contents(Window, True);");
        Ada_Put_Line(to_body, "  end Create_Full_Dialog; -- " &
                   S(last_dialog_ident) & "_Type" );
    end case;
    Ada_New_Line(to);
    Ada_Put_Line(to, "  --  b) Create all contents, not the window itself (must be");
    Ada_Put_Line(to, "  --      already created) -> can be used in/as any kind of window.");
    Ada_New_Line(to);
    Ada_Put_Line(to, "  procedure Create_Contents");
    Ada_Put_Line(to, "     ( Window      : in out " & type_name & ";");
    Ada_Put_Line(to, "       for_dialog  : in     Boolean; -- True: buttons do close the window");
    Ada_Put_Line(to, "       resize      : in     Boolean:= False -- optionnally resize Window as designed");
    Ada_Put(to,      "     )");
    if to = to_body then
      Ada_New_Line(to);
      Ada_Put_Line(to_body, "  is");
      Ada_Put_Line(to_body, "    x,y,w,h: Integer;");
      Ada_Put_Line(to_body, "  begin");
      Ada_Put_Line(to_body, "    if resize then");
      Ada_Coord_conv(last_dialog_rect);
      Ada_Put_Line(to_body, "      Move(Window, x,y);");
      Ada_Put_Line(to_body, "      Client_Area_Size(Window, w, h);" );
      Ada_Put_Line(to_body, "    end if;");
      if style_switch(shell_font) then
        Ada_Put_Line(to_body, "    Use_GUI_Font(Window);");
      end if;
    end if;
  end Ada_Proc_Dialog;

  procedure Ada_Proc_Menu(
    to       : Pkg_output;
    type_name: String
  )
  is
  begin
    Ada_Put_Line(to, "  -- Menu at line" & Integer'Image(linenum));
    Ada_Put_Line(to, "  procedure Create_Full_Menu");
    Ada_Put(to, "     (Menu        : in out " & type_name & ")");
  end Ada_Proc_Menu;

  procedure Ada_Helpers_spec(to: Pkg_output) is
  begin
    Ada_New_Line(to);
    Ada_Put_Line(to, "  procedure Dlg_to_Scn(");
    Ada_Put_Line(to, "    xd,yd,wd,hd:  in Integer;");
    Ada_Put_Line(to, "    xs,ys,ws,hs: out Integer);");
    Ada_New_Line(to);
    Ada_Put_Line(to, "  procedure Use_GUI_Font(Window: in out GWindows.Base.Base_Window_Type'Class);");
    Ada_New_Line(to);
    Ada_Put_Line(to, "  function Num_resource(id: Natural) return String;");
    Ada_New_Line(to);
  end Ada_Helpers_spec;

  procedure Ada_Begin is
  begin
    if separate_items then
      Create(to_spec, pkg(as_file_name => True) & "-Helpers.ads");
      Blurb(to_spec);
      Ada_Put_Line(to_spec, "with GWindows.Base;");
      Ada_New_Line(to_spec);
      Ada_Put_Line(to_spec, "package " & pkg & ".Helpers is");
      Ada_Helpers_spec(to_spec);
      Ada_Put_Line(to_spec, "end " & pkg & ".Helpers;");
      Close(Ada_files(to_spec));
      --
      Create(to_spec, pkg(as_file_name => True) & ".ads");
      Blurb(to_spec);
      Ada_Put_Line(to_spec, "-- Option separate_items selected (-s with RC2GW), then all items are in child packages.");
      Ada_Put_Line(to_spec, "-- Children are");
      Ada_Put_Line(to_spec, "--   1 per dialog or menu item");
      Ada_Put_Line(to_spec, "--   " & pkg & ".Constants for resource constants (ID's)");
      Ada_Put_Line(to_spec, "--   " & pkg & ".Helpers for internal helper routines");
      Ada_Put_Line(to_spec, "--   " & pkg & ".Version_info for constants stored in the VersionInfo part.");
      Ada_New_Line(to_spec);
      Ada_Put_Line(to_spec, "package " & pkg & " is");
      Ada_Put_Line(to_spec, "  -- empty: everything is in child packages.");
      Ada_Put_Line(to_spec, "end " & pkg & ';');
      Close(Ada_files(to_spec));
    else
      Create(to_spec, pkg(as_file_name => True) & ".ads");
      Create(to_body, pkg(as_file_name => True) & ".adb");
      Ada_package_headers(eventual_child => "");
      Ada_Put_Line(to_body, "  -- ** Generated code begins here \/ \/ \/.");
      Ada_New_Line(to_body);
    end if;
    --
    popup_stack(0):= 0; -- root is "Main" menu
    anonymous_dialog_counter:= 0;
    anonymous_menu_counter:= 0;
  end Ada_Begin;

  procedure Ada_Coord_conv(rect: Rect_type) is
  begin
     Ada_Put_Line(to_body,
       "    Dlg_to_Scn( " & -- Window_Type(Window),
       Long_Long_Integer'Image(rect.x) & ',' &
       Long_Long_Integer'Image(rect.y) & ',' &
       Long_Long_Integer'Image(rect.w) & ',' &
       Long_Long_Integer'Image(rect.h) & ", x,y,w,h);" );
  end Ada_Coord_conv;

  procedure Ada_normal_control_create(comma_text, extra: String:= ""; with_id: Boolean:= True) is
    function id_part return String is
    begin
      if with_id then
        return ", " & S(last_Ada_constant);
      else
        return "";
      end if;
    end id_part;

  begin
    Ada_Put_Line(to_body,
      "    Create( Window." & S(last_Ada_ident) & ", Window" &
      comma_text & ", x,y,w,h" & extra & id_part &
      ");"
    );
  end Ada_normal_control_create;

  procedure Ada_normal_control(type_name: String; comma_text, extra: String:= ""; with_id: Boolean:= True) is
  begin
    Ada_Coord_conv(last_rect);
    Ada_Put_Line(to_spec, "    " & S(last_Ada_ident) & ": " & type_name & ";" );
    Ada_normal_control_create(comma_text, extra, with_id);
  end Ada_normal_control;

  procedure Ada_very_normal_control(type_name: String) is
  begin
    Ada_normal_control(type_name, ", " & S(last_text));
  end Ada_very_normal_control;

  procedure Ada_button_control is
  begin
    if style_switch(state3) then
      Ada_very_normal_control("Three_State_Box_Type");
    elsif style_switch(checkbox) then
      Ada_very_normal_control("Check_Box_Type");
    elsif style_switch(radio) then
      Ada_very_normal_control("Radio_Button_Type");
    elsif style_switch(push) then
      Ada_Coord_conv(last_rect);
      -- Here it is a bit tricky, since, as expected,
      -- Dialog_Button's close the window and Button don't .
      -- If we want a "real", permanent, window, then we want
      -- the latter sort.
      --
      -- "Dialog" version of the button
      --
      Ada_Put(to_spec, "    " & S(last_Ada_ident) & ": ");
      if style_switch(default) then
        Ada_Put(to_spec, "Default_");
      end if;
      Ada_Put_Line(to_spec, "Dialog_Button_Type;    -- closes parent window after click" );
      Ada_Put_Line(to_body, "    -- Both versions of the button are created.");
      Ada_Put_Line(to_body, "    -- The more meaningful one is made visible, but this choice");
      Ada_Put_Line(to_body, "    -- can be reversed, for instance on a ""Browse"" button.");
      Ada_normal_control_create(", " & S(last_text));
      --
      -- "Window" version of the button
      --
      temp_ustr:= last_Ada_ident;
      last_Ada_ident:= U(S(last_Ada_ident) & "_permanent");
      Ada_Put(to_spec, "    " & S(last_Ada_ident) & ": ");
      if style_switch(default) then
        Ada_Put(to_spec, "Default_");
      end if;
      Ada_Put_Line(to_spec, "Button_Type; -- doesn't close parent window after click" );
      Ada_normal_control_create(", " & S(last_text));
      Ada_Put_Line(to_body, "    if for_dialog then -- hide the non-closing button");
      Ada_Put_Line(to_body, "      Window." & S(last_Ada_ident) & ".Hide;");
      Ada_Put_Line(to_body, "    else -- hide the closing button");
      Ada_Put_Line(to_body, "      Window." & S(temp_ustr) & ".Hide;");
      Ada_Put_Line(to_body, "    end if;");
    end if;
  end Ada_button_control;

  -- Control class is given as a string, not a token (e.g. "Button")
  procedure Identify_control_class(RC_String: String) is
  begin
    for c in Control_type loop
      if To_Upper(RC_String) = '"' & To_Upper(Control_type'Image(c)) & '"' then
        control:= c;
        exit;
      end if;
    end loop;
  end Identify_control_class;

  procedure Test_generation is
    pkg: constant String:= Root_Name(S(source_name));
  begin
    Create(to_body, pkg & "_Test_app.adb");
    Ada_Put_Line(to_body, "-- GWindows test application, generated by GWenerator");
    Ada_New_Line(to_body);
    Ada_Put_Line(to_body, "with GWindows.Base;               use GWindows.Base;");
    Ada_Put_Line(to_body, "with GWindows.Edit_Boxes;         use GWindows.Edit_Boxes;");
    Ada_Put_Line(to_body, "with GWindows.Windows;            use GWindows.Windows;");
    Ada_Put_Line(to_body, "with GWindows.Message_Boxes;      use GWindows.Message_Boxes;");
    Ada_Put_Line(to_body, "with GWindows.Application;");
    Ada_New_Line(to_body);
    Ada_Put_Line(to_body, "with " & pkg & "_Resource_GUI;");
    Ada_Put_Line(to_body, " use " & pkg & "_Resource_GUI;");
    Ada_New_Line(to_body);
    Ada_Put_Line(to_body, "procedure " & pkg & "_Test_app is");
    Ada_New_Line(to_body);
    Ada_Put_Line(to_body, "  pragma Linker_Options (""-mwindows"");");
    Ada_New_Line(to_body);
    for i in 1..dialog_top loop
      Ada_Put_Line(to_body,
        "  Dlg" & Trim(Integer'Image(i), both) &
        "      : " & pkg & "_Resource_GUI." & S(dialog_mem(i)) & ";"
      );
    end loop;
    Ada_Put_Line(to_body, "  Result    : Integer;");
    Ada_Put_Line(to_body, "  No_Parent : Window_Type;");
    Ada_New_Line(to_body);
    Ada_Put_Line(to_body, "begin");
    for i in 1..dialog_top loop
      declare
        dlg: constant String:=  "Dlg" & Trim(Integer'Image(i), both);
      begin
        Ada_Put_Line(to_body, "  Create_Full_Dialog (" & dlg & ", No_Parent);");
        Ada_Put_Line(to_body, "  Center(" & dlg & ");");
        Ada_Put_Line(to_body, "  Result := GWindows.Application.Show_Dialog (" & dlg & ");");
      end;
    end loop;
    Ada_Put_Line(to_body, "end " & pkg & "_Test_app;");
    Close(Ada_Files(to_body));
  end Test_generation;

  procedure YY_Accept is
  begin
    if separate_items then
      Create(to_spec, pkg(as_file_name => True) & "-Constants.ads");
      Blurb(to_spec);
      Ada_Put_Line(to_spec, "-- Constants from resource.h or equivalent");
      Ada_New_Line(to_spec);
      Ada_Put_Line(to_spec, "package " & pkg & ".Constants is");
      Output_symbols;
      Ada_Put_Line(to_spec, "end " & pkg & ".Constants;");
      Close(Ada_files(to_spec));
      --
      Create(to_body, pkg(as_file_name => True) & "-Helpers.adb");
      Ada_Put_Line(to_body, "with GWindows.Types;              use GWindows.Types;");
      Ada_Put_Line(to_body, "with GWindows.Drawing_Objects;");
      Ada_Put_Line(to_body, "with Interfaces.C;                      use Interfaces.C;");
      Ada_Put_Line(to_body, "with System;");
      Ada_New_Line(to_body);
      Ada_Put_Line(to_body, "package body " & pkg & ".Helpers is");
      Ada_Put_Line(to_body, "  use GWindows;");
    else
      Ada_New_Line(to_spec);
      Output_symbols;
      Ada_New_Line(to_spec);
      Ada_Put_Line(to_spec, "  -- ** Some helper utilities (spec).");
      Ada_Helpers_spec(to_spec);
      Ada_New_Line(to_body);
      Ada_Put_Line(to_body, "  -- ** Generated code ends here /\ /\ /\.");
      Ada_New_Line(to_body);
      Ada_Put_Line(to_body, "  -- ** Some helper utilities (body).");
    end if;
    Ada_New_Line(to_body);
    Ada_Put_Line(to_body, "  procedure Dlg_to_Scn( -- converts dialog coords to screen (pixel) coords.");
    Ada_Put_Line(to_body, "    xd,yd,wd,hd:  in Integer;");
    Ada_Put_Line(to_body, "    xs,ys,ws,hs: out Integer)");
    Ada_Put_Line(to_body, "  is");
    Ada_Put_Line(to_body, "    -- function GetDialogBaseUnits return Integer;");
    Ada_Put_Line(to_body, "    -- pragma Import (StdCall, GetDialogBaseUnits, ""GetDialogBaseUnits"");");
    Ada_Put_Line(to_body, "    -- baseunit, baseunitX, baseunitY: Integer;");
    Ada_Put_Line(to_body, "    baseunitX: constant:=" & Positive'Image(base_unit_x) & ';');
    Ada_Put_Line(to_body, "    baseunitY: constant:=" & Positive'Image(base_unit_y) & ';');
    Ada_Put_Line(to_body, "  begin");
    Ada_Put_Line(to_body, "    -- baseunit:= GetDialogBaseUnits; -- this gives X=8, Y=16 (SYSTEM font)");
    Ada_Put_Line(to_body, "    -- baseunitX:= baseunit mod (2 ** 16);");
    Ada_Put_Line(to_body, "    -- baseunitY:= baseunit  / (2 ** 16);");
    Ada_Put_Line(to_body, "    -- NB: the other way with MapDialogRect works only");
    Ada_Put_Line(to_body, "    --   by full moon, hence the use-defined units.");
    Ada_Put_Line(to_body, "    xs := (xd * baseunitX) / 4;");
    Ada_Put_Line(to_body, "    ws := (wd * baseunitX) / 4;");
    Ada_Put_Line(to_body, "    ys := (yd * baseunitY) / 8;");
    Ada_Put_Line(to_body, "    hs := (hd * baseunitY) / 8;");
    Ada_Put_Line(to_body, "  end Dlg_to_Scn;");
    Ada_New_Line(to_body);
    Ada_Put_Line(to_body, "  package Common_Fonts is");
    Ada_Put_Line(to_body, "    GUI_Font : GWindows.Drawing_Objects.Font_Type;");
    Ada_Put_Line(to_body, "    URL_Font : GWindows.Drawing_Objects.Font_Type;");
    Ada_Put_Line(to_body, "    -- ^ These fonts are created once, at startup");
    Ada_Put_Line(to_body, "    --   it avoid GUI resource leak under Windows 95/98/ME");
    Ada_Put_Line(to_body, "    procedure Create_Common_Fonts;");
    Ada_Put_Line(to_body, "    -- in initialisation part if this pkg becomes standalone");
    Ada_Put_Line(to_body, "  end Common_Fonts;");
    Ada_New_Line(to_body);
    Ada_Put_Line(to_body, "  procedure Use_GUI_Font(Window: in out GWindows.Base.Base_Window_Type'Class)");
    Ada_Put_Line(to_body, "  is");
    Ada_Put_Line(to_body, "  begin");
    Ada_Put_Line(to_body, "    --  Use Standard Windows GUI font instead of system font");
    Ada_Put_Line(to_body, "    GWindows.Base.Set_Font (Window, Common_Fonts.GUI_Font);");
    Ada_Put_Line(to_body, "  end Use_GUI_Font;");
    Ada_New_Line(to_body);
    Ada_Put_Line(to_body, "  function Num_resource(id: Natural) return String is");
    Ada_Put_Line(to_body, "    img: constant String:= Integer'Image(id);");
    Ada_Put_Line(to_body, "  begin");
    Ada_Put_Line(to_body, "    return '#' & img(img'first+1..img'Last);");
    Ada_Put_Line(to_body, "  end Num_resource;");
    Ada_New_Line(to_body);
    Ada_Put_Line(to_body, "  package body Common_Fonts is");
    Ada_New_Line(to_body);
    Ada_Put_Line(to_body, "    procedure Create_Common_Fonts is");
    Ada_New_Line(to_body);
    Ada_Put_Line(to_body, "     type Face_Name_Type is array(1..32) of GWindows.GChar_C;");
    Ada_New_Line(to_body);
    Ada_Put_Line(to_body, "     type LOGFONT is record");
    Ada_Put_Line(to_body, "       lfHeight: Interfaces.C.long;");
    Ada_Put_Line(to_body, "       lfWidth: Interfaces.C.long;");
    Ada_Put_Line(to_body, "       lfEscapement: Interfaces.C.long;");
    Ada_Put_Line(to_body, "       lfOrientation: Interfaces.C.long;");
    Ada_Put_Line(to_body, "       lfWeight: Interfaces.C.long;");
    Ada_Put_Line(to_body, "       lfItalic: Interfaces.C.char;");
    Ada_Put_Line(to_body, "       lfUnderline: Interfaces.C.char;");
    Ada_Put_Line(to_body, "       lfStrikeOut: Interfaces.C.char;");
    Ada_Put_Line(to_body, "       lfCharSet: Interfaces.C.char;");
    Ada_Put_Line(to_body, "       lfOutPrecision: Interfaces.C.char;");
    Ada_Put_Line(to_body, "       lfClipPrecision: Interfaces.C.char;");
    Ada_Put_Line(to_body, "       lfQuality: Interfaces.C.char;");
    Ada_Put_Line(to_body, "       lfPitchAndFamily: Interfaces.C.char;");
    Ada_Put_Line(to_body, "       lfFaceName: Face_Name_Type;");
    Ada_Put_Line(to_body, "     end record;");
    Ada_New_Line(to_body);
    Ada_Put_Line(to_body, "     Log_of_current_font: aliased LOGFONT;");
    Ada_New_Line(to_body);
    Ada_Put_Line(to_body, "     subtype PVOID   is System.Address;                      --  winnt.h");
    Ada_Put_Line(to_body, "     subtype LPVOID  is PVOID;                               --  windef.h");
    Ada_New_Line(to_body);
    Ada_Put_Line(to_body, "     function GetObject");
    Ada_Put_Line(to_body, "       (hgdiobj  : GWindows.Types.Handle  := GWindows.Drawing_Objects.Handle(GUI_Font);");
    Ada_Put_Line(to_body, "        cbBufferl: Interfaces.C.int       := LOGFONT'Size / 8;");
    Ada_Put_Line(to_body, "        lpvObject: LPVOID                 := Log_of_Current_font'Address)");
    Ada_Put_Line(to_body, "       return Interfaces.C.int;");
    Ada_Put_Line(to_body, "     pragma Import (StdCall, GetObject,");
    Ada_Put_Line(to_body, "                      ""GetObject"" & Character_Mode_Identifier);");
    Ada_New_Line(to_body);
    Ada_Put_Line(to_body, "     function CreateFontIndirect");
    Ada_Put_Line(to_body, "       (lpvObject: LPVOID                 := Log_of_Current_font'Address)");
    Ada_Put_Line(to_body, "       return GWindows.Types.Handle;");
    Ada_Put_Line(to_body, "     pragma Import (StdCall, CreateFontIndirect,");
    Ada_Put_Line(to_body, "                      ""CreateFontIndirect"" & Character_Mode_Identifier);");
    Ada_New_Line(to_body);
    Ada_Put_Line(to_body, "    begin");
    Ada_Put_Line(to_body, "      GWindows.Drawing_Objects.Create_Stock_Font(");
    Ada_Put_Line(to_body, "        GUI_Font,");
    Ada_Put_Line(to_body, "        GWindows.Drawing_Objects.Default_GUI");
    Ada_Put_Line(to_body, "      );");
    Ada_Put_Line(to_body, "      if GetObject = 0 then");
    Ada_Put_Line(to_body, "        GWindows.Drawing_Objects.Create_Font(URL_Font,");
    Ada_Put_Line(to_body, "          ""MS Sans Serif"",");
    Ada_Put_Line(to_body, "          14, Underline => True);");
    Ada_Put_Line(to_body, "            -- !! ^ Not so nice (non-unsharpened font, size ~..., color ?)");
    Ada_Put_Line(to_body, "      else");
    Ada_Put_Line(to_body, "        Log_of_Current_font.lfUnderline:= Interfaces.C.Char'Val(1);");
    Ada_Put_Line(to_body, "        GWindows.Drawing_Objects.Handle(URL_font, CreateFontIndirect);");
    Ada_Put_Line(to_body, "      end if;");
    Ada_Put_Line(to_body, "    end Create_Common_Fonts;");
    Ada_New_Line(to_body);
    Ada_Put_Line(to_body, "  end Common_Fonts;");
    Ada_New_Line(to_body);
    Ada_Put_Line(to_body, "begin");
    Ada_Put_Line(to_body, "  Common_Fonts.Create_Common_Fonts;");
    for to in pkg_output loop
      if not (to = to_spec and separate_items) then
        Ada_New_Line(to);
        Ada_Put_Line(to, "  -- Last line of resource script file:" & Integer'Image(linenum));
        Ada_New_Line(to);
        if separate_items then
          Ada_Put_Line(to, "end " & pkg & ".Helpers;");
        else
          Ada_Put_Line(to, "end " & pkg & ';');
        end if;
        Close( Ada_files(to) );
      end if;
    end loop;
    Delete(symbols_by_name);
    Delete(symbols_by_value);
    if generate_test then
      Test_generation;
    end if;
  end YY_Accept;

  procedure YY_Abort is
  begin
    Put_Line(Current_Error, "YY_Abort");
  end;

  procedure YY_Terminate is
  begin
    Put_Line(Current_Error, "YY_Terminate");
  end;

  procedure RC_Comment(s: String) is
  begin
    null;
  end;

  procedure Reset_globals is
  begin
    has_input:= False;
    source_name:= U("");
    linenum:= 0;
    base_unit_x:= 6;  -- usual value, overriden with option -x
    base_unit_y:= 13; -- usual value, overriden with option -y
    separate_items:= False;
    generate_test:= False;
    first_include:= True;
    --
    -- Initialize the symbol dictionaries with common symbols (See GWindows.Constants)
    Insert_Common( "IDOK"    , 1);
    Insert_Common( "IDCANCEL", 2);
    Insert_Common( "IDABORT" , 3);
    Insert_Common( "IDRETRY" , 4);
    Insert_Common( "IDIGNORE", 5);
    Insert_Common( "IDYES"   , 6);
    Insert_Common( "IDNO"    , 7);
    Insert_Common( "IDCLOSE" , 8);
    Insert_Common( "IDHELP"  , 9);
  end;

end RC_Help;
