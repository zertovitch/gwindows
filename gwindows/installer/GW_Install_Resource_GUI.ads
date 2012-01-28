---------------------------------------------------------------------------
-- GUI contents of resource script file: GW_Install.rc
-- Transcription time: 2012/01/28  10:22:04
--
-- Translated by the RC2GW or by the GWenerator tool.
-- URL: http://sf.net/projects/gnavi
--
-- This file contains only automatically generated code. Do not edit this.
-- Rework the resource script instead, and re-run the translator.
-- RC Grammar version: 24-Feb-2010
---------------------------------------------------------------------------

with GWindows.Base;                     use GWindows.Base;
with GWindows.Constants;                use GWindows.Constants;
with GWindows.Windows;                  use GWindows.Windows;
with GWindows.Buttons;                  use GWindows.Buttons;
with GWindows.Buttons.Graphic;          use GWindows.Buttons.Graphic;
with GWindows.Buttons.Owner_drawn;      use GWindows.Buttons.Owner_drawn;
with GWindows.Edit_Boxes;               use GWindows.Edit_Boxes;
with GWindows.List_Boxes;               use GWindows.List_Boxes;
with GWindows.Combo_Boxes;              use GWindows.Combo_Boxes;
with GWindows.Static_Controls;          use GWindows.Static_Controls;
with GWindows.Scroll_Bars;              use GWindows.Scroll_Bars;
with GWindows.Common_Controls;          use GWindows.Common_Controls;
with GWindows.Menus;                    use GWindows.Menus;
use GWindows;
with Interfaces.C;                      use Interfaces.C;

package GW_Install_Resource_GUI is

  type Conflict_dialog_Type is new Window_type with record

    IDOK: Default_Dialog_Button_Type;    -- closes parent window after click
    IDOK_permanent: Default_Button_Type; -- doesn't close parent window after click
    IDCANCEL: Dialog_Button_Type;    -- closes parent window after click
    IDCANCEL_permanent: Button_Type; -- doesn't close parent window after click
    -- Label: IDC_STATIC
    Installed_version: List_Box_Type;
    Installer_Version: List_Box_Type;
    -- Label: IDC_STATIC
    -- Label: IDC_STATIC
  end record; -- Conflict_dialog_Type

  -- Dialog at resource line 36

  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Conflict_dialog_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Version check";
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
     ( Window      : in out Conflict_dialog_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     );

  type Install_dialog_Type is new Window_type with record

    IDOK: Default_Dialog_Button_Type;    -- closes parent window after click
    IDOK_permanent: Default_Button_Type; -- doesn't close parent window after click
    IDCANCEL: Dialog_Button_Type;    -- closes parent window after click
    IDCANCEL_permanent: Button_Type; -- doesn't close parent window after click
    -- Label: IDC_STATIC
    -- Label: IDC_STATIC
    IDC_EDIT1: Edit_Box_Type;
    Directory_select_button: Dialog_Button_Type;    -- closes parent window after click
    Directory_select_button_permanent: Button_Type; -- doesn't close parent window after click
    UNICODE_choice: Radio_Button_Type;
    ANSI_choice: Radio_Button_Type;
    Static_0003: Group_Box_Type;
    Static_0004: Bitmap_Type;
    -- Label: IDC_STATIC
    -- Label: IDC_STATIC
    -- Label: IDC_STATIC
  end record; -- Install_dialog_Type

  -- Dialog at resource line 59

  --  a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Install_dialog_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "GWindows installer";
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
     ( Window      : in out Install_dialog_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     );

  package Version_info is
    Authors: constant String:= "Gautier de Montmollin";
    FileDescription: constant String:= "GWindows Installer";
    FileVersion: constant String:= "0.9";
    LegalCopyright: constant String:= "© 2012 G. de Montmollin (MIT license)";
    ProductName: constant String:= "GWindows Installer";
    Translation: constant:= 1033;
  end Version_info;


  ------------------------------------------------
  -- Defined resource symbols --> Ada constants --
  ------------------------------------------------

  -- NB: only items with a defined symbol get a constant here
  -- These constants are needed for getting button and menu feedbacks.

  IDC_STATIC             : constant:=     -1;
  Install_dialog         : constant:=    131;
  GNAVI_Logo             : constant:=    133;
  Conflict_dialog        : constant:=    135;
  Directory_select_button: constant:=   1000;
  Installed_version      : constant:=   1003;
  IDC_EDIT1              : constant:=  40015;
  Installer_Version      : constant:=  40018;
  ANSI_choice            : constant:=  40019;
  UNICODE_choice         : constant:=  40021;

  -- ** Some helper utilities (spec).

  procedure Dlg_to_Scn(
    xd,yd,wd,hd:  in Integer;
    xs,ys,ws,hs: out Integer);

  procedure Use_GUI_Font(Window: in out GWindows.Base.Base_Window_Type'Class);

  function Num_resource(id: Natural) return String;


  -- Last line of resource script file: 101

end GW_Install_Resource_GUI;
