---------------------------------------------------------------------
-- GUI contents of resource script file: GWenerator.rc
-- Transcription time: 2009/06/04   23:12:14
--
-- Translated by the RC2GW or GWenerator tools.
-- URL: http://sf.net/projects/gnavi
--
-- This is automatically generated code. Do not edit this.
-- Rework the resource instead, and re-run the translator.
-- RC Grammar version: 4-Jun-2009
---------------------------------------------------------------------

with GWindows.Types;                    use GWindows.Types;
with GWindows.Drawing;                  use GWindows.Drawing;
with GWindows.Drawing_Objects;
with Interfaces.C;                      use Interfaces.C;
with System;

package body GWenerator_Resource_GUI is

  -- ** Generated code begins here \/ \/ \/.


  -- Menu at line 29
  procedure Create_Full_Menu
     (Menu        : in out Main_Menu_Type)
  is
  begin
    Menu.Main:= Create_Menu;
    Menu.Popup_0001:= Create_Popup;
    Append_Menu(Menu.Main, "&File", Menu.Popup_0001);
    Append_Item(Menu.Popup_0001, "&New...", New_GWen);
    Append_Item(Menu.Popup_0001, "&Open...", Open_GWen);
    Append_Item(Menu.Popup_0001, "&Save", Save_GWen);
    Append_Item(Menu.Popup_0001, "Save &as...", Save_GWen_as);
    Append_Separator(Menu.Popup_0001);
    Append_Item(Menu.Popup_0001, "&Quit", Quit);
    Menu.Popup_0002:= Create_Popup;
    Append_Menu(Menu.Main, "&Tools", Menu.Popup_0002);
    Append_Item(Menu.Popup_0002, "&Generate test application", Generate_test_app);
    Menu.Popup_0003:= Create_Popup;
    Append_Menu(Menu.Main, "&Options", Menu.Popup_0003);
    Append_Item(Menu.Popup_0003, "&Options for this GWen", GWen_Options);
    Append_Item(Menu.Popup_0003, "GWenerator &preferences", GWenerator_Preferences);
    Menu.Popup_0004:= Create_Popup;
    Append_Menu(Menu.Main, "&Help", Menu.Popup_0004);
    Append_Item(Menu.Popup_0004, "&About", About);
  end Create_Full_Menu; -- Main_Menu_Type


  -- Dialog at resource line 61
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
      Is_Dynamic  : in     Boolean := False)
  is
    x,y,w,h: Integer;
  begin
    Dlg_to_Scn(  0, 0, 218, 129, x,y,w,h);
    if Left   /= Use_Default then x:= Left;   end if;
    if Top    /= Use_Default then y:= Top;    end if;
    if Width  /= Use_Default then w:= Width;  end if;
    if Height /= Use_Default then h:= Height; end if;
    Create_As_Dialog(
      Window => Window_Type(Window),
      Parent => Parent,
      Title  => Title,
      Left   => x,
      Top    => y,
      Width  => w,
      Height => h,
      Help_Button => Help_Button,
      Is_Dynamic  => Is_Dynamic
    );
    if Width = Use_Default then Client_Area_Width(Window, w); end if;
    if Height = Use_Default then Client_Area_Height(Window, h); end if;
    Use_GUI_Font(Window);
    Create_Contents(Window, True);
  end Create_Full_Dialog; -- About_box_Type

  --  b) Create all contents, not the window itself (must be
  --      already created) -> can be used in/as any kind of window.

  procedure Create_Contents
     ( Window      : in out About_box_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     )
  is
    x,y,w,h: Integer;
  begin
    if resize then
    Dlg_to_Scn(  0, 0, 218, 129, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn(  85, 110, 50, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.IDOK, Window, "Close", x,y,w,h, IDOK);
    Create( Window.IDOK_permanent, Window, "Close", x,y,w,h, IDOK);
    if for_dialog then -- hide the non-closing button
      Window.IDOK_permanent.Hide;
    else -- hide the closing button
      Window.IDOK.Hide;
    end if;
    Dlg_to_Scn(  5, 5, 21, 20, x,y,w,h);
    Create( Window.Static_0001, Window, Num_resource(gwenerator_icon), x,y,w,h);
    Dlg_to_Scn(  35, 10, 165, 8, x,y,w,h);
    Create_label( Window, "GWenerator - a code generator for GWindows", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  35, 25, 150, 8, x,y,w,h);
    Create_label( Window, "Copyright © Gautier de Montmollin 2008..2009", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  35, 40, 100, 8, x,y,w,h);
    Create_label( Window, "MIT Open Source License", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  35, 55, 30, 8, x,y,w,h);
    Create_label( Window, "Internet:", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  70, 55, 90, 8, x,y,w,h);
    Create( Window.URL, Window, "http://sf.net/projects/gnavi", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, URL);
    Dlg_to_Scn(  35, 70, 170, 8, x,y,w,h);
    Create( Window.RC_gramm_ver, Window, "RC Grammar version: ", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, RC_gramm_ver);
    Dlg_to_Scn(  35, 85, 170, 8, x,y,w,h);
    Create( Window.GWen_ver, Window, "GWenerator version: ", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, GWen_ver);
  end Create_Contents; -- About_box_Type


  -- Dialog at resource line 77
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
      Is_Dynamic  : in     Boolean := False)
  is
    x,y,w,h: Integer;
  begin
    Dlg_to_Scn(  0, 0, 314, 231, x,y,w,h);
    if Left   /= Use_Default then x:= Left;   end if;
    if Top    /= Use_Default then y:= Top;    end if;
    if Width  /= Use_Default then w:= Width;  end if;
    if Height /= Use_Default then h:= Height; end if;
    Create_As_Dialog(
      Window => Window_Type(Window),
      Parent => Parent,
      Title  => Title,
      Left   => x,
      Top    => y,
      Width  => w,
      Height => h,
      Help_Button => Help_Button,
      Is_Dynamic  => Is_Dynamic
    );
    if Width = Use_Default then Client_Area_Width(Window, w); end if;
    if Height = Use_Default then Client_Area_Height(Window, h); end if;
    Use_GUI_Font(Window);
    Create_Contents(Window, True);
  end Create_Full_Dialog; -- GWen_properties_Type

  --  b) Create all contents, not the window itself (must be
  --      already created) -> can be used in/as any kind of window.

  procedure Create_Contents
     ( Window      : in out GWen_properties_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     )
  is
    x,y,w,h: Integer;
  begin
    if resize then
    Dlg_to_Scn(  0, 0, 314, 231, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn(  197, 212, 50, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.IDOK, Window, "OK", x,y,w,h, IDOK);
    Create( Window.IDOK_permanent, Window, "OK", x,y,w,h, IDOK);
    if for_dialog then -- hide the non-closing button
      Window.IDOK_permanent.Hide;
    else -- hide the closing button
      Window.IDOK.Hide;
    end if;
    Dlg_to_Scn(  252, 212, 50, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.IDCANCEL, Window, "Cancel", x,y,w,h, IDCANCEL);
    Create( Window.IDCANCEL_permanent, Window, "Cancel", x,y,w,h, IDCANCEL);
    if for_dialog then -- hide the non-closing button
      Window.IDCANCEL_permanent.Hide;
    else -- hide the closing button
      Window.IDCANCEL.Hide;
    end if;
    Dlg_to_Scn(  85, 19, 168, 15, x,y,w,h);
    Create( Window.Edit_RC_File_Name, Window, "", x,y,w,h, TRUE, Edit_RC_File_Name);
    Dlg_to_Scn(  22, 22, 60, 8, x,y,w,h);
    Create_label( Window, "File name", x,y,w,h, GWindows.Static_Controls.RIGHT, NONE);
    Dlg_to_Scn(  255, 19, 43, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.Button_Browse_RC, Window, "Browse...", x,y,w,h, Button_Browse_RC);
    Create( Window.Button_Browse_RC_permanent, Window, "Browse...", x,y,w,h, Button_Browse_RC);
    if for_dialog then -- hide the non-closing button
      Window.Button_Browse_RC_permanent.Hide;
    else -- hide the closing button
      Window.Button_Browse_RC.Hide;
    end if;
    Dlg_to_Scn(  22, 44, 93, 10, x,y,w,h);
    Create( Window.Listen_RC, Window, "Listen for newer version", x,y,w,h, Listen_RC);
    Dlg_to_Scn(  132, 44, 131, 10, x,y,w,h);
    Create( Window.Auto_translate, Window, "Automatically translate when newer", x,y,w,h, Auto_translate);
    Dlg_to_Scn(  12, 7, 290, 55, x,y,w,h);
    Create( Window.Static_0002, Window, "Resource file", x,y,w,h);
    Dlg_to_Scn(  132, 155, 131, 10, x,y,w,h);
    Create( Window.Auto_build, Window, "Automatically build when GUI newer", x,y,w,h, Auto_build);
    Dlg_to_Scn(  85, 130, 168, 15, x,y,w,h);
    Create( Window.Edit_Main_Ada_File_Name, Window, "", x,y,w,h, TRUE, Edit_Main_Ada_File_Name);
    Dlg_to_Scn(  255, 130, 43, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.Button_Browse_Ada, Window, "Browse...", x,y,w,h, Button_Browse_Ada);
    Create( Window.Button_Browse_Ada_permanent, Window, "Browse...", x,y,w,h, Button_Browse_Ada);
    if for_dialog then -- hide the non-closing button
      Window.Button_Browse_Ada_permanent.Hide;
    else -- hide the closing button
      Window.Button_Browse_Ada.Hide;
    end if;
    Dlg_to_Scn(  22, 155, 107, 10, x,y,w,h);
    Create( Window.Listen_Ada, Window, "Listen for newer GUI version", x,y,w,h, Listen_Ada);
    Dlg_to_Scn(  12, 118, 290, 55, x,y,w,h);
    Create( Window.Static_0003, Window, "Main application ( facultative )", x,y,w,h);
    Dlg_to_Scn(  22, 82, 151, 10, x,y,w,h);
    Create( Window.Separate_items, Window, "Produce a separate package for each item", x,y,w,h, Separate_items);
    Dlg_to_Scn(  65, 97, 20, 15, x,y,w,h);
    Create( Window.Basx, Window, "", x,y,w,h, TRUE, Basx);
    Dlg_to_Scn(  25, 97, 39, 8, x,y,w,h);
    Create_label( Window, "Base unit x:", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  102, 97, 20, 15, x,y,w,h);
    Create( Window.Basy, Window, "", x,y,w,h, TRUE, Basy);
    Dlg_to_Scn(  90, 97, 10, 8, x,y,w,h);
    Create_label( Window, " y:", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  130, 97, 55, 10, x,y,w,h);
    Create( Window.Use_base_defs, Window, "use defaults", x,y,w,h, Use_base_defs);
    Dlg_to_Scn(  12, 67, 294, 50, x,y,w,h);
    Create( Window.Static_0006, Window, "RC2GW - RC to GWindows transcription engine", x,y,w,h);
    Dlg_to_Scn(  12, 177, 288, 30, x,y,w,h);
    Create( Window.Static_0007, Window, "Ada builder command line ( facultative )", x,y,w,h);
    Dlg_to_Scn(  17, 187, 275, 14, x,y,w,h);
    Create( Window.Ada_cmd, Window, "", x,y,w,h, TRUE, Ada_cmd);
    Dlg_to_Scn(  20, 135, 62, 8, x,y,w,h);
    Create_label( Window, "App. source or exe", x,y,w,h, GWindows.Static_Controls.RIGHT, NONE);
  end Create_Contents; -- GWen_properties_Type


  -- Dialog at resource line 107
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
      Is_Dynamic  : in     Boolean := False)
  is
    x,y,w,h: Integer;
  begin
    Dlg_to_Scn(  0, 0, 403, 246, x,y,w,h);
    if Left   /= Use_Default then x:= Left;   end if;
    if Top    /= Use_Default then y:= Top;    end if;
    if Width  /= Use_Default then w:= Width;  end if;
    if Height /= Use_Default then h:= Height; end if;
    Create_As_Dialog(
      Window => Window_Type(Window),
      Parent => Parent,
      Title  => Title,
      Left   => x,
      Top    => y,
      Width  => w,
      Height => h,
      Help_Button => Help_Button,
      Is_Dynamic  => Is_Dynamic
    );
    if Width = Use_Default then Client_Area_Width(Window, w); end if;
    if Height = Use_Default then Client_Area_Height(Window, h); end if;
    Use_GUI_Font(Window);
    Create_Contents(Window, True);
  end Create_Full_Dialog; -- Main_dialog_Type

  --  b) Create all contents, not the window itself (must be
  --      already created) -> can be used in/as any kind of window.

  procedure Create_Contents
     ( Window      : in out Main_dialog_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     )
  is
    x,y,w,h: Integer;
  begin
    if resize then
    Dlg_to_Scn(  0, 0, 403, 246, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn(  5, 100, 390, 140, x,y,w,h);
    Create( Window.Details_frame, Window, "Details", x,y,w,h);
    Dlg_to_Scn(  12, 132, 185, 105, x,y,w,h);
    Create( Window.RC_to_GWindows_messages, Window, x,y,w,h, TRUE, RC_to_GWindows_messages);
    Dlg_to_Scn(  210, 132, 182, 105, x,y,w,h);
    Create( Window.GNATMake_messages, Window, x,y,w,h, TRUE, GNATMake_messages);
    Dlg_to_Scn(  10, 115, 88, 8, x,y,w,h);
    Create_label( Window, "Resource to Ada messages", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  212, 117, 84, 8, x,y,w,h);
    Create( Window.Ada_comp_label, Window, "Ada compilation messages", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, Ada_comp_label);
    Dlg_to_Scn(  67, 37, 100, 20, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.Button_Translate, Window, "Translate now", x,y,w,h, Button_Translate);
    Create( Window.Button_Translate_permanent, Window, "Translate now", x,y,w,h, Button_Translate);
    if for_dialog then -- hide the non-closing button
      Window.Button_Translate_permanent.Hide;
    else -- hide the closing button
      Window.Button_Translate.Hide;
    end if;
    Dlg_to_Scn(  245, 37, 100, 20, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.Button_Build, Window, "Build now", x,y,w,h, Button_Build);
    Create( Window.Button_Build_permanent, Window, "Build now", x,y,w,h, Button_Build);
    if for_dialog then -- hide the non-closing button
      Window.Button_Build_permanent.Hide;
    else -- hide the closing button
      Window.Button_Build.Hide;
    end if;
    Dlg_to_Scn(  67, 62, 100, 10, x,y,w,h);
    Create( Window.Bar_RC, Window, x,y,w,h);
    Dlg_to_Scn(  245, 62, 100, 10, x,y,w,h);
    Create( Window.Bar_Ada, Window, x,y,w,h);
    Dlg_to_Scn(  247, 12, 24, 22, x,y,w,h);
    Create( Window.Ear_Ada, Window, Num_resource(Listen_32x32), x,y,w,h);
    Dlg_to_Scn(  67, 12, 24, 22, x,y,w,h);
    Create( Window.Ear_RC, Window, Num_resource(Listen_32x32), x,y,w,h);
    Dlg_to_Scn(  5, 80, 56, 10, x,y,w,h);
    Create( Window.Show_Details, Window, "Show details", x,y,w,h, Show_Details);
    Dlg_to_Scn(  97, 12, 60, 15, x,y,w,h);
    Create( Window.Newer_RC, Window, "Resource file is newer!", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, Newer_RC);
    Dlg_to_Scn(  277, 12, 40, 20, x,y,w,h);
    Create( Window.Newer_Ada, Window, "Ada files are newer!", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, Newer_Ada);
    Dlg_to_Scn(  17, 37, 21, 20, x,y,w,h);
    Create( Window.Static_0002, Window, Num_resource(RC_file), x,y,w,h);
    Dlg_to_Scn(  197, 37, 21, 20, x,y,w,h);
    Create( Window.Ada_file_icon, Window, Num_resource(Ada_file), x,y,w,h);
    Dlg_to_Scn(  367, 37, 21, 20, x,y,w,h);
    Create( Window.Exe_file_icon, Window, Num_resource(Exe_file), x,y,w,h);
  end Create_Contents; -- Main_dialog_Type


  -- ** Generated code ends here /\ /\ /\.

  -- ** Some helper utilities (body).

  procedure Dlg_to_Scn( -- converts dialog coords to screen (pixel) coords.
    xd,yd,wd,hd:  in Integer;
    xs,ys,ws,hs: out Integer)
  is
    -- function GetDialogBaseUnits return Integer;
    -- pragma Import (StdCall, GetDialogBaseUnits, "GetDialogBaseUnits");
    -- baseunit, baseunitX, baseunitY: Integer;
    baseunitX: constant:= 6;
    baseunitY: constant:= 13;
  begin
    -- baseunit:= GetDialogBaseUnits; -- this gives X=8, Y=16 (SYSTEM font)
    -- baseunitX:= baseunit mod (2 ** 16);
    -- baseunitY:= baseunit  / (2 ** 16);
    -- NB: the other way with MapDialogRect works only
    --   by full moon, hence the use-defined units.
    xs := (xd * baseunitX) / 4;
    ws := (wd * baseunitX) / 4;
    ys := (yd * baseunitY) / 8;
    hs := (hd * baseunitY) / 8;
  end Dlg_to_Scn;

  package Common_Fonts is
    GUI_Font : GWindows.Drawing_Objects.Font_Type;
    URL_Font : GWindows.Drawing_Objects.Font_Type;
    -- ^ These fonts are created once, at startup
    --   it avoid GUI resource leak under Windows 95/98/ME
    procedure Create_Common_Fonts;
    -- in initialisation part if this pkg becomes standalone
  end Common_Fonts;

  procedure Use_GUI_Font(Window: in out GWindows.Base.Base_Window_Type'Class)
  is
  begin
    --  Use Standard Windows GUI font instead of system font
    GWindows.Base.Set_Font (Window, Common_Fonts.GUI_Font);
  end Use_GUI_Font;

  function Num_resource(id: Natural) return String is
    img: constant String:= Integer'Image(id);
  begin
    return '#' & img(img'first+1..img'Last);
  end Num_resource;

  package body Common_Fonts is

    procedure Create_Common_Fonts is

     type Face_Name_Type is array(1..32) of GWindows.GChar_C;

     type LOGFONT is record
       lfHeight: Interfaces.C.long;
       lfWidth: Interfaces.C.long;
       lfEscapement: Interfaces.C.long;
       lfOrientation: Interfaces.C.long;
       lfWeight: Interfaces.C.long;
       lfItalic: Interfaces.C.char;
       lfUnderline: Interfaces.C.char;
       lfStrikeOut: Interfaces.C.char;
       lfCharSet: Interfaces.C.char;
       lfOutPrecision: Interfaces.C.char;
       lfClipPrecision: Interfaces.C.char;
       lfQuality: Interfaces.C.char;
       lfPitchAndFamily: Interfaces.C.char;
       lfFaceName: Face_Name_Type;
     end record;

     Log_of_current_font: aliased LOGFONT;

     subtype PVOID   is System.Address;                      --  winnt.h
     subtype LPVOID  is PVOID;                               --  windef.h

     function GetObject
       (hgdiobj  : GWindows.Types.Handle  := GWindows.Drawing_Objects.Handle(GUI_Font);
        cbBufferl: Interfaces.C.int       := LOGFONT'Size / 8;
        lpvObject: LPVOID                 := Log_of_Current_font'Address)
       return Interfaces.C.int;
     pragma Import (StdCall, GetObject,
                      "GetObject" & Character_Mode_Identifier);

     function CreateFontIndirect
       (lpvObject: LPVOID                 := Log_of_Current_font'Address)
       return GWindows.Types.Handle;
     pragma Import (StdCall, CreateFontIndirect,
                      "CreateFontIndirect" & Character_Mode_Identifier);

    begin
      GWindows.Drawing_Objects.Create_Stock_Font(
        GUI_Font,
        GWindows.Drawing_Objects.Default_GUI
      );
      if GetObject = 0 then
        GWindows.Drawing_Objects.Create_Font(URL_Font,
          "MS Sans Serif",
          14, Underline => True);
            -- !! ^ Not so nice (non-unsharpened font, size ~..., color ?)
      else
        Log_of_Current_font.lfUnderline:= Interfaces.C.Char'Val(1);
        GWindows.Drawing_Objects.Handle(URL_font, CreateFontIndirect);
      end if;
    end Create_Common_Fonts;

  end Common_Fonts;

begin
  Common_Fonts.Create_Common_Fonts;

  -- Last line of resource script file: 163

end GWenerator_Resource_GUI;
