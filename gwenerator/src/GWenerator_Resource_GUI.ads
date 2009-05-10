---------------------------------------------------------------------
-- GUI contents of resource script file: GWenerator.rc
-- Transcription time: 2009/05/10   20:14:17
--
-- Translated by the RC2GW or GWenerator tools.
-- URL: http://sf.net/projects/gnavi
--
-- This is automatically generated code. Do not edit this.
-- Rework the resource instead, and re-run the translator.
-- RC Grammar version: 30-Apr-2009
---------------------------------------------------------------------

with GWindows.Base;                     use GWindows.Base;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Windows;                  use GWindows.Windows;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Edit_Boxes;               use GWindows.Edit_Boxes;
with GWindows.List_Boxes;               use GWindows.List_Boxes;
with GWindows.Combo_Boxes;              use GWindows.Combo_Boxes;
with GWindows.Static_Controls;          use GWindows.Static_Controls;
with GWindows.Scroll_Bars;              use GWindows.Scroll_Bars;
with GWindows.Common_Controls;          use GWindows.Common_Controls;
with GWindows.Menus;                    use GWindows.Menus;
use GWindows;

package GWenerator_Resource_GUI is

  type Main_Menu_Type is tagged record
    Main: Menu_Type; -- Root of the whole menu tree
    Popup_0001: Menu_Type;  -- level 1; title: "File"
    Popup_0002: Menu_Type;  -- level 1; title: "Options"
    Popup_0003: Menu_Type;  -- level 1; title: "Help"
  end record; -- Main_Menu_Type

  -- Menu at line 65
  procedure Create_Full_Menu
     (Menu        : in out Main_Menu_Type);

  type About_box_Type is new Window_type with record

    IDOK: Default_Dialog_Button_Type;    -- closes parent window after click
    IDOK_permanent: Default_Button_Type; -- doesn't close parent window after click
    Static_0001: GWindows.Static_Controls.Icon_Type;
    -- Label: IDC_STATIC
    -- Label: IDC_STATIC
    -- Label: IDC_STATIC
    -- Label: IDC_STATIC
    URL: Label_Type;
    RC_gr_ver: Label_Type;
  end record; -- About_box_Type

  -- Dialog at resource line 85
  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog

  procedure Create_Full_Dialog
     (Window      : in out About_box_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "About GWenerator";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --  b) Create all contents, not the window itself (must be
  --      already created) -> can be used in/as any kind of window.

  procedure Create_Contents
     ( Window      : in out About_box_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     );

  type GWen_properties_Type is new Window_type with record

    IDOK: Default_Dialog_Button_Type;    -- closes parent window after click
    IDOK_permanent: Default_Button_Type; -- doesn't close parent window after click
    IDCANCEL: Dialog_Button_Type;    -- closes parent window after click
    IDCANCEL_permanent: Button_Type; -- doesn't close parent window after click
    Edit_RC_File_Name: Edit_Box_Type;
    -- Label: IDC_STATIC
    Button_Browse_RC: Dialog_Button_Type;    -- closes parent window after click
    Button_Browse_RC_permanent: Button_Type; -- doesn't close parent window after click
    Listen_RC: Check_Box_Type;
    Auto_translate: Check_Box_Type;
    Static_0002: Group_Box_Type;
    Auto_build: Check_Box_Type;
    Edit_Main_Ada_File_Name: Edit_Box_Type;
    Button_Browse_Ada: Dialog_Button_Type;    -- closes parent window after click
    Button_Browse_Ada_permanent: Button_Type; -- doesn't close parent window after click
    Listen_Ada: Check_Box_Type;
    Static_0003: Group_Box_Type;
    Separate_items: Check_Box_Type;
    Basx: Edit_Box_Type;
    -- Label: IDC_STATIC
    Basy: Edit_Box_Type;
    -- Label: IDC_STATIC
    Use_base_defs: Check_Box_Type;
    Static_0006: Group_Box_Type;
    Static_0007: Group_Box_Type;
    Ada_cmd: Edit_Box_Type;
    -- Label: IDC_STATIC
  end record; -- GWen_properties_Type

  -- Dialog at resource line 117
  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog

  procedure Create_Full_Dialog
     (Window      : in out GWen_properties_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "GWen options";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --  b) Create all contents, not the window itself (must be
  --      already created) -> can be used in/as any kind of window.

  procedure Create_Contents
     ( Window      : in out GWen_properties_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     );

  type Main_dialog_Type is new Window_type with record

    Details_frame: Group_Box_Type;
    RC_to_GWindows_messages: List_Box_Type;
    GNATMake_messages: List_Box_Type;
    -- Label: IDC_STATIC
    Ada_comp_label: Label_Type;
    Button_Translate: Dialog_Button_Type;    -- closes parent window after click
    Button_Translate_permanent: Button_Type; -- doesn't close parent window after click
    Button_Build: Dialog_Button_Type;    -- closes parent window after click
    Button_Build_permanent: Button_Type; -- doesn't close parent window after click
    Bar_RC: Progress_Control_Type;
    Bar_Ada: Progress_Control_Type;
    Ear_Ada: Bitmap_Type;
    Ear_RC: Bitmap_Type;
    Show_Details: Check_Box_Type;
    Newer_RC: Label_Type;
    Newer_Ada: Label_Type;
    Static_0002: GWindows.Static_Controls.Icon_Type;
    Ada_file_icon: GWindows.Static_Controls.Icon_Type;
    Exe_file_icon: GWindows.Static_Controls.Icon_Type;
  end record; -- Main_dialog_Type

  -- Dialog at resource line 143
  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog

  procedure Create_Full_Dialog
     (Window      : in out Main_dialog_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "GWenerator - Link 1.gwen";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --  b) Create all contents, not the window itself (must be
  --      already created) -> can be used in/as any kind of window.

  procedure Create_Contents
     ( Window      : in out Main_dialog_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     );


  ------------------------------------------------
  -- Defined resource symbols --> Ada constants --
  ------------------------------------------------

  -- NB: only items with a defined symbol get a constant here
  -- These constants are needed for getting button and menu feedbacks.

  IDC_STATIC             : constant:=     -1;
  GWen_properties        : constant:=    105;
  Listen_32x32           : constant:=    107;
  Ada_file               : constant:=    116;
  Exe_file               : constant:=    118;
  Wheels_32x32           : constant:=    120;
  Ada_file_icon          : constant:=   1000;
  Listen_RC              : constant:=   1000;
  URL                    : constant:=   1000;
  Exe_file_icon          : constant:=   1001;
  RC_gr_ver              : constant:=   1001;
  Separate_items         : constant:=   1001;
  Details_frame          : constant:=   1002;
  Ada_cmd                : constant:=   1003;
  Ada_comp_label         : constant:=   1003;
  Basx                   : constant:=   1004;
  Basy                   : constant:=   1005;
  Ear_RC                 : constant:=   1006;
  Ear_Ada                : constant:=   1007;
  About_box              : constant:=   1033;
  Not_Listen_32x32       : constant:=   1033;
  RC_file                : constant:=   1034;
  gwenerator_icon        : constant:=   1035;
  Main_Menu              : constant:=   4108;
  Main_dialog            : constant:=   4108;
  GNATMake_messages      : constant:=   4109;
  RC_to_GWindows_messages: constant:=   4110;
  Button_Translate       : constant:=   4112;
  Button_Build           : constant:=   4114;
  Bar_RC                 : constant:=   4121;
  Bar_Ada                : constant:=   4122;
  Edit_RC_File_Name      : constant:=   4122;
  New_GWen               : constant:=  40000;
  Open_GWen              : constant:=  40001;
  Save_GWen              : constant:=  40002;
  GWen_Options           : constant:=  40003;
  GWenerator_Preferences : constant:=  40004;
  About                  : constant:=  40005;
  Show_Details           : constant:=  40005;
  Newer_RC               : constant:=  40006;
  Quit                   : constant:=  40006;
  Save_GWen_as           : constant:=  40007;
  Listen_Ada             : constant:=  40008;
  Newer_Ada              : constant:=  40008;
  Auto_translate         : constant:=  40009;
  Auto_build             : constant:=  40010;
  Edit_Main_Ada_File_Name: constant:=  40011;
  Button_Browse_Ada      : constant:=  40012;
  Button_Browse_RC       : constant:=  40013;
  Use_base_defs          : constant:=  40015;

  -- ** Some helper utilities (spec).

  procedure Dlg_to_Scn(
    xd,yd,wd,hd:  in Integer;
    xs,ys,ws,hs: out Integer);

  procedure Use_GUI_Font(Window: in out GWindows.Base.Base_Window_Type'Class);

  function Num_resource(id: Natural) return String;


  -- Last line of resource script file: 151

end GWenerator_Resource_GUI;
