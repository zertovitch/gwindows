---------------------------------------------------------------------------
--  GUI contents of resource script file: Dialog.rc
--  Transcription time: 2024/06/04  02:07:33
--  GWenerator project file: dialog.gwen
--
--  Translated by the RC2GW or by the GWenerator tool.
--  URL: http://sf.net/projects/gnavi
--
--  This file contains only automatically generated code. Do not edit this.
--  Rework the resource script instead, and re-run the translator.
--  RC Grammar version: 29-Jul-2022
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

pragma Warnings ("U");  --  turn off warnings for unused entity

package Dialog_Resource_GUI is

  type Nice_Dialog_Type is new Window_Type with record

    Group_Frame : Group_Box_Type;
    On_Button : Radio_Button_Type;
    Off_Button : Radio_Button_Type;
    IDCANCEL : Dialog_Button_Type;    --  Closes parent window after click
    IDCANCEL_permanent : Button_Type;  --  Doesn't close parent window after click
    IDOK : Default_Dialog_Button_Type;    --  Closes parent window after click
    IDOK_permanent : Default_Button_Type;  --  Doesn't close parent window after click
  end record; -- Nice_Dialog_Type

  --  Dialog at resource line 26

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Nice_Dialog_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Dialog";
      Left        : in     Integer := Use_Default;  --  Default = as designed
      Top         : in     Integer := Use_Default;  --  Default = as designed
      Width       : in     Integer := Use_Default;  --  Default = as designed
      Height      : in     Integer := Use_Default;  --  Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False);

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
      (Window      : in out Nice_Dialog_Type;
       for_dialog  : in     Boolean;          --  True: buttons do close the window
       resize      : in     Boolean := False  --  optionally resize Window as designed
     );


  --------------------------------------------------
  --  Defined resource symbols --> Ada constants  --
  --------------------------------------------------

  --  NB: only items with a defined symbol get a constant here
  --  These constants are needed for getting button and menu feedbacks.

  IDC_STATIC  : constant :=     -1;
  Nice_Dialog : constant :=    100;
  Off_Button  : constant :=  40000;
  On_Button   : constant :=  40001;
  Group_Frame : constant :=  40002;

  --  ** Some helper utilities (spec).

  procedure Dlg_to_Scn
    (xd, yd, wd, hd :  in Integer;
     xs, ys, ws, hs : out Integer);

  procedure Use_GUI_Font (Window : in out GWindows.Base.Base_Window_Type'Class);

  function Num_resource (id : Natural) return GString;  --  Just turn 123 into "#123".

  --  Last line of resource script file: 27

end Dialog_Resource_GUI;
