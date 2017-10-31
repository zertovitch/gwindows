---------------------------------------------------------------------------
-- GUI contents of resource script file: GWenerator.rc
-- Transcription time: 2017/10/31  09:22:28
-- GWenerator project file: GWenerator.gwen
--
-- Translated by the RC2GW or by the GWenerator tool.
-- URL: http://sf.net/projects/gnavi
--
-- This file contains only automatically generated code. Do not edit this.
-- Rework the resource script instead, and re-run the translator.
-- RC Grammar version: 31-Oct-2017
---------------------------------------------------------------------------

with GWindows.Base;                     use GWindows.Base;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Windows;                  use GWindows.Windows;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Buttons.Graphic;          use GWindows.Buttons.Graphic;
with GWindows.Buttons.Owner_Drawn;      use GWindows.Buttons.Owner_Drawn;
with GWindows.Edit_Boxes;               use GWindows.Edit_Boxes;
with GWindows.List_Boxes;               use GWindows.List_Boxes;
with GWindows.Combo_Boxes;              use GWindows.Combo_Boxes;
with GWindows.Static_Controls;          use GWindows.Static_Controls;
with GWindows.Scroll_Bars;              use GWindows.Scroll_Bars;
with GWindows.Common_Controls;          use GWindows.Common_Controls;
with GWindows.Menus;                    use GWindows.Menus;
use GWindows;
with Interfaces.C;                      use Interfaces.C;

package GWenerator_Resource_GUI is

  type Main_Menu_Type is tagged record
    Main: Menu_Type; -- Root of the whole menu tree
    Popup_0001: Menu_Type;  -- level 1; title: "&File"
    Popup_0002: Menu_Type;  -- level 1; title: "&Actions"
    Popup_0003: Menu_Type;  -- level 1; title: "&Options"
    Popup_0004: Menu_Type;  -- level 1; title: "&Help"
  end record;  --  Main_Menu_Type

  -- Menu at line 74
  procedure Create_Full_Menu
     (Menu        : in out Main_Menu_Type);

  type About_box_Type is new Window_Type with record

    IDOK: Default_Dialog_Button_Type;    -- closes parent window after click
    IDOK_permanent: Default_Button_Type; -- doesn't close parent window after click
    Static_0001: Icon_Type;
    -- Label: IDC_STATIC
    -- Label: IDC_STATIC
    -- Label: IDC_STATIC
    -- Label: IDC_STATIC
    URL: Label_Type;
    RC_gramm_ver: Label_Type;
    GWen_ver: Label_Type;
    GNAT_URL: Label_Type;
    GNAVI_URL: Label_Type;
    ResEdit_URL: Label_Type;
    Static_0006: Group_Box_Type;
    GNAT_Version: Label_Type;
  end record; -- About_box_Type

  -- Dialog at resource line 101

  -- Pre-Create operation to switch off default styles
  -- or add ones that are not in usual GWindows Create parameters
  --
  procedure On_Pre_Create (Window    : in out About_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned);

  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
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
  --
  procedure Create_Contents
     ( Window      : in out About_box_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionally resize Window as designed
     );

  type GWen_properties_Type is new Window_Type with record

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
    Initialize_controls: Check_Box_Type;
    -- Label: IDC_STATIC
    RC_Compiler_list: Drop_Down_List_Box_Type;
  end record; -- GWen_properties_Type

  -- Dialog at resource line 137

  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
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
  --
  procedure Create_Contents
     ( Window      : in out GWen_properties_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionally resize Window as designed
     );

  type Main_dialog_Type is new Window_Type with record

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
    Static_0002: Icon_Type;
    Ada_file_icon: Icon_Type;
    Exe_file_icon: Icon_Type;
    Show_Ada_build: Check_Box_Type;
    Auto_build_lift_msg: Label_Type;
    Auto_build_lift_ico: Icon_Type;
    Static_0003: Icon_Type;
    Static_0004: Icon_Type;
    Ada_blue_3: Icon_Type;
    Static_0005: Icon_Type;
    More_less_details: Bitmap_Button_Type;
    More_less_build: Bitmap_Button_Type;
  end record; -- Main_dialog_Type

  -- Dialog at resource line 173

  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
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
  --
  procedure Create_Contents
     ( Window      : in out Main_dialog_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionally resize Window as designed
     );

  package Version_info is
    Authors: constant String:= "Gautier de Montmollin";
    FileDescription: constant String:= "GWenerator - Resource script (.rc) to GWindows Ada code translator";
    FileVersion: constant String:= "1.05";
    LegalCopyright: constant String:= "© 2008 .. 2017 G. de Montmollin (MIT license)";
    ProductName: constant String:= "GWenerator";
    Translation: constant:= 1033;
  end Version_info;

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
  Less_Horizontal        : constant:=    122;
  More_Horizontal        : constant:=    123;
  Less_Vertical          : constant:=    124;
  More_Vertical          : constant:=    125;
  Warning_icon           : constant:=    127;
  Arrow_icon             : constant:=    129;
  Ada_file_icon          : constant:=   1000;
  Listen_RC              : constant:=   1000;
  URL                    : constant:=   1000;
  Exe_file_icon          : constant:=   1001;
  GNAT_Version           : constant:=   1001;
  Initialize_controls    : constant:=   1001;
  Details_frame          : constant:=   1002;
  GWen_ver               : constant:=   1002;
  Separate_items         : constant:=   1002;
  Ada_cmd                : constant:=   1003;
  Ada_comp_label         : constant:=   1003;
  Basx                   : constant:=   1004;
  RC_gramm_ver           : constant:=   1004;
  Basy                   : constant:=   1005;
  Show_Ada_build         : constant:=   1005;
  Ear_RC                 : constant:=   1006;
  Ear_Ada                : constant:=   1007;
  RC_Compiler_list       : constant:=   1007;
  Auto_build_lift_msg    : constant:=   1008;
  Auto_build_lift_ico    : constant:=   1010;
  Ada_blue_3             : constant:=   1011;
  More_less_details      : constant:=   1012;
  About_box              : constant:=   1033;
  Not_Listen_32x32       : constant:=   1033;
  RC_file                : constant:=   1034;
  gwenerator_icon        : constant:=   1035;
  GNAT_URL               : constant:=   1036;
  GNAVI_URL              : constant:=   1037;
  ResEdit_URL            : constant:=   1038;
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
  Generate_test_app      : constant:=  40008;
  Listen_Ada             : constant:=  40008;
  Newer_Ada              : constant:=  40008;
  Auto_translate         : constant:=  40009;
  Compile_resource_only  : constant:=  40009;
  Auto_build             : constant:=  40010;
  Start_main_app         : constant:=  40010;
  Edit_Main_Ada_File_Name: constant:=  40011;
  Button_Browse_Ada      : constant:=  40012;
  Button_Browse_RC       : constant:=  40013;
  Use_base_defs          : constant:=  40015;
  More_less_build        : constant:=  40016;

  -- ** Some helper utilities (spec).

  procedure Dlg_to_Scn(
    xd,yd,wd,hd:  in Integer;
    xs,ys,ws,hs: out Integer);

  procedure Use_GUI_Font(Window: in out GWindows.Base.Base_Window_Type'Class);

  function Num_resource(id: Natural) return GString;  --  Just turn 123 into "#123".

  -- Last line of resource script file: 264

end GWenerator_Resource_GUI;
