---------------------------------------------------------------------------
-- GUI contents of resource script file: GWenerator.rc
-- Transcription time: 2012/05/23  14:16:36
--
-- Translated by the RC2GW or by the GWenerator tool.
-- URL: http://sf.net/projects/gnavi
--
-- This file contains only automatically generated code. Do not edit this.
-- Rework the resource script instead, and re-run the translator.
-- RC Grammar version: 23-May-2012
---------------------------------------------------------------------------

with GWindows.Types;                    use GWindows.Types;
with GWindows.Drawing;                  use GWindows.Drawing;
with GWindows.Drawing_Objects;
with System;

package body GWenerator_Resource_GUI is

  -- ** Generated code begins here \/ \/ \/.


  -- Menu at line 49
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
    Append_Menu(Menu.Main, "&Actions", Menu.Popup_0002);
    Append_Item(Menu.Popup_0002, "&Generate test application", Generate_test_app);
    Append_Item(Menu.Popup_0002, "&Start main application", Start_main_app);
    Append_Item(Menu.Popup_0002, "&Compile resource only", Compile_resource_only);
    Menu.Popup_0003:= Create_Popup;
    Append_Menu(Menu.Main, "&Options", Menu.Popup_0003);
    Append_Item(Menu.Popup_0003, "&Options for this GWen", GWen_Options);
    Append_Item(Menu.Popup_0003, "GWenerator &preferences", GWenerator_Preferences);
    Menu.Popup_0004:= Create_Popup;
    Append_Menu(Menu.Main, "&Help", Menu.Popup_0004);
    Append_Item(Menu.Popup_0004, "&About", About);
  end Create_Full_Menu; -- Main_Menu_Type


  -- Dialog at resource line 86

  -- Pre-Create operation to switch off default styles
  -- or add ones that are not in usual GWindows Create parameters
  --
  procedure On_Pre_Create (Window    : in out About_box_Type;
                           dwStyle   : in out Interfaces.C.unsigned;
                           dwExStyle : in out Interfaces.C.unsigned)
  is
    pragma Warnings (Off, Window);
    pragma Warnings (Off, dwExStyle);
    WS_SYSMENU: constant:= 16#0008_0000#;
  begin
    dwStyle:= dwStyle and not WS_SYSMENU;
  end On_Pre_Create;

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
      Is_Dynamic  : in     Boolean := False)
  is
    x,y,w,h: Integer;
  begin
    Dlg_to_Scn(  0, 0, 229, 184, x,y,w,h);
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
  --
  procedure Create_Contents
     ( Window      : in out About_box_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     )
  is
    x,y,w,h: Integer;
  begin
    if resize then
    Dlg_to_Scn(  0, 0, 229, 184, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn(  91, 162, 50, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.IDOK, Window, "Close", x,y,w,h, ID => IDOK);
    Create( Window.IDOK_permanent, Window, "Close", x,y,w,h, ID => IDOK);
    if for_dialog then -- hide the non-closing button
      Hide(Window.IDOK_permanent);
    else -- hide the closing button
      Hide(Window.IDOK);
    end if;
    Dlg_to_Scn(  5, 5, 32, 30, x,y,w,h);
    Create( Window.Static_0001, Window, Num_resource(gwenerator_icon), x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  35, 10, 165, 8, x,y,w,h);
    Create_label( Window, "GWenerator: a code generator for GWindows", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  35, 25, 151, 8, x,y,w,h);
    Create_label( Window, "Copyright © Gautier de Montmollin 2008..2012", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  35, 40, 100, 8, x,y,w,h);
    Create_label( Window, "MIT Open Source License", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  11, 55, 30, 8, x,y,w,h);
    Create_label( Window, "Internet:", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  70, 55, 120, 8, x,y,w,h);
    Create( Window.URL, Window, "http://sf.net/projects/gnavi", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => URL);
    Dlg_to_Scn(  11, 70, 170, 8, x,y,w,h);
    Create( Window.RC_gramm_ver, Window, "RC Grammar version: ", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => RC_gramm_ver);
    Dlg_to_Scn(  11, 85, 170, 8, x,y,w,h);
    Create( Window.GWen_ver, Window, "GWenerator version: ", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => GWen_ver);
    Dlg_to_Scn(  11, 100, 208, 8, x,y,w,h);
    Create_label( Window, "Software made with the following free open source components:", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  11, 116, 170, 8, x,y,w,h);
    Create( Window.GNAT_URL, Window, "GNAT", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => GNAT_URL);
    Dlg_to_Scn(  11, 131, 170, 8, x,y,w,h);
    Create( Window.GNAVI_URL, Window, "GNAVI", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => GNAVI_URL);
    Dlg_to_Scn(  11, 146, 170, 8, x,y,w,h);
    Create( Window.ResEdit_URL, Window, "ResEdit", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => ResEdit_URL);
  end Create_Contents; -- About_box_Type


  -- Dialog at resource line 109

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
      Is_Dynamic  : in     Boolean := False)
  is
    x,y,w,h: Integer;
  begin
    Dlg_to_Scn(  0, 0, 314, 282, x,y,w,h);
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
  --
  procedure Create_Contents
     ( Window      : in out GWen_properties_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     )
  is
    x,y,w,h: Integer;
  begin
    if resize then
    Dlg_to_Scn(  0, 0, 314, 282, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn(  195, 255, 50, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.IDOK, Window, "OK", x,y,w,h, ID => IDOK);
    Create( Window.IDOK_permanent, Window, "OK", x,y,w,h, ID => IDOK);
    if for_dialog then -- hide the non-closing button
      Hide(Window.IDOK_permanent);
    else -- hide the closing button
      Hide(Window.IDOK);
    end if;
    Dlg_to_Scn(  250, 255, 50, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.IDCANCEL, Window, "Cancel", x,y,w,h, ID => IDCANCEL);
    Create( Window.IDCANCEL_permanent, Window, "Cancel", x,y,w,h, ID => IDCANCEL);
    if for_dialog then -- hide the non-closing button
      Hide(Window.IDCANCEL_permanent);
    else -- hide the closing button
      Hide(Window.IDCANCEL);
    end if;
    Dlg_to_Scn(  83, 17, 168, 15, x,y,w,h);
    Create( Window.Edit_RC_File_Name, Window, "", x,y,w,h, Horizontal_Scroll => TRUE, Read_Only => FALSE, ID => Edit_RC_File_Name);
    Dlg_to_Scn(  20, 20, 60, 8, x,y,w,h);
    Create_label( Window, "File name", x,y,w,h, GWindows.Static_Controls.RIGHT, NONE);
    Dlg_to_Scn(  253, 17, 43, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.Button_Browse_RC, Window, "Browse...", x,y,w,h, ID => Button_Browse_RC);
    Create( Window.Button_Browse_RC_permanent, Window, "Browse...", x,y,w,h, ID => Button_Browse_RC);
    if for_dialog then -- hide the non-closing button
      Hide(Window.Button_Browse_RC_permanent);
    else -- hide the closing button
      Hide(Window.Button_Browse_RC);
    end if;
    Dlg_to_Scn(  20, 42, 93, 10, x,y,w,h);
    Create( Window.Listen_RC, Window, "Listen for newer version", x,y,w,h, ID => Listen_RC);
    Dlg_to_Scn(  130, 42, 131, 10, x,y,w,h);
    Create( Window.Auto_translate, Window, "Automatically translate when newer", x,y,w,h, ID => Auto_translate);
    Dlg_to_Scn(  10, 5, 290, 80, x,y,w,h);
    Create( Window.Static_0002, Window, "Resource file", x,y,w,h);
    Dlg_to_Scn(  130, 198, 131, 10, x,y,w,h);
    Create( Window.Auto_build, Window, "Automatically build when GUI newer", x,y,w,h, ID => Auto_build);
    Dlg_to_Scn(  83, 173, 168, 15, x,y,w,h);
    Create( Window.Edit_Main_Ada_File_Name, Window, "", x,y,w,h, Horizontal_Scroll => TRUE, Read_Only => FALSE, ID => Edit_Main_Ada_File_Name);
    Dlg_to_Scn(  253, 173, 43, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.Button_Browse_Ada, Window, "Browse...", x,y,w,h, ID => Button_Browse_Ada);
    Create( Window.Button_Browse_Ada_permanent, Window, "Browse...", x,y,w,h, ID => Button_Browse_Ada);
    if for_dialog then -- hide the non-closing button
      Hide(Window.Button_Browse_Ada_permanent);
    else -- hide the closing button
      Hide(Window.Button_Browse_Ada);
    end if;
    Dlg_to_Scn(  20, 198, 107, 10, x,y,w,h);
    Create( Window.Listen_Ada, Window, "Listen for newer GUI version", x,y,w,h, ID => Listen_Ada);
    Dlg_to_Scn(  10, 161, 290, 55, x,y,w,h);
    Create( Window.Static_0003, Window, "Main application ( facultative )", x,y,w,h);
    Dlg_to_Scn(  23, 110, 270, 10, x,y,w,h);
    Create( Window.Separate_items, Window, "Produce a separate package for each item", x,y,w,h, ID => Separate_items);
    Dlg_to_Scn(  63, 125, 20, 15, x,y,w,h);
    Create( Window.Basx, Window, "", x,y,w,h, Horizontal_Scroll => TRUE, Read_Only => FALSE, ID => Basx);
    Dlg_to_Scn(  23, 125, 39, 8, x,y,w,h);
    Create_label( Window, "Base unit x:", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  100, 125, 20, 15, x,y,w,h);
    Create( Window.Basy, Window, "", x,y,w,h, Horizontal_Scroll => TRUE, Read_Only => FALSE, ID => Basy);
    Dlg_to_Scn(  88, 125, 10, 8, x,y,w,h);
    Create_label( Window, " y:", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  128, 125, 55, 10, x,y,w,h);
    Create( Window.Use_base_defs, Window, "use defaults", x,y,w,h, ID => Use_base_defs);
    Dlg_to_Scn(  10, 97, 290, 60, x,y,w,h);
    Create( Window.Static_0006, Window, "RC2GW - RC to GWindows transcription engine", x,y,w,h);
    Dlg_to_Scn(  10, 220, 288, 30, x,y,w,h);
    Create( Window.Static_0007, Window, "Ada builder command line ( facultative )", x,y,w,h);
    Dlg_to_Scn(  15, 230, 275, 14, x,y,w,h);
    Create( Window.Ada_cmd, Window, "", x,y,w,h, Horizontal_Scroll => TRUE, Read_Only => FALSE, ID => Ada_cmd);
    Dlg_to_Scn(  18, 178, 60, 8, x,y,w,h);
    Create_label( Window, "Executable", x,y,w,h, GWindows.Static_Controls.RIGHT, NONE);
    Dlg_to_Scn(  23, 140, 275, 10, x,y,w,h);
    Create( Window.Initialize_controls, Window, "Initialize some controls with fake contents (analogy to pragma Initialize_Scalars)", x,y,w,h, ID => Initialize_controls);
    Dlg_to_Scn(  20, 65, 155, 8, x,y,w,h);
    Create_label( Window, "Invoking resource compiler from GWenerator...", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  180, 63, 110, 15, x,y,w,h);
    Create( Window.RC_Compiler_list, Window, "", x,y,w,h, FALSE, ID => RC_Compiler_list);
  end Create_Contents; -- GWen_properties_Type


  -- Dialog at resource line 145

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
  --
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
    Dlg_to_Scn(  5, 105, 390, 135, x,y,w,h);
    Create( Window.Details_frame, Window, "Details", x,y,w,h);
    Dlg_to_Scn(  12, 132, 185, 105, x,y,w,h);
    Create( Window.RC_to_GWindows_messages, Window, x,y,w,h, FALSE, ID => RC_to_GWindows_messages);
    Dlg_to_Scn(  210, 132, 182, 105, x,y,w,h);
    Create( Window.GNATMake_messages, Window, x,y,w,h, FALSE, ID => GNATMake_messages);
    Dlg_to_Scn(  10, 117, 88, 8, x,y,w,h);
    Create_label( Window, "Resource to Ada messages", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  212, 117, 115, 8, x,y,w,h);
    Create( Window.Ada_comp_label, Window, "Ada compilation and build messages", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => Ada_comp_label);
    Dlg_to_Scn(  67, 37, 100, 20, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.Button_Translate, Window, "Translate now", x,y,w,h, ID => Button_Translate);
    Create( Window.Button_Translate_permanent, Window, "Translate now", x,y,w,h, ID => Button_Translate);
    if for_dialog then -- hide the non-closing button
      Hide(Window.Button_Translate_permanent);
    else -- hide the closing button
      Hide(Window.Button_Translate);
    end if;
    Dlg_to_Scn(  245, 37, 100, 20, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.Button_Build, Window, "Build now", x,y,w,h, ID => Button_Build);
    Create( Window.Button_Build_permanent, Window, "Build now", x,y,w,h, ID => Button_Build);
    if for_dialog then -- hide the non-closing button
      Hide(Window.Button_Build_permanent);
    else -- hide the closing button
      Hide(Window.Button_Build);
    end if;
    Dlg_to_Scn(  67, 62, 100, 10, x,y,w,h);
    Create( Window.Bar_RC, Window, x,y,w,h, HORIZONTAL, TRUE);
    Dlg_to_Scn(  245, 62, 100, 10, x,y,w,h);
    Create( Window.Bar_Ada, Window, x,y,w,h, HORIZONTAL, TRUE);
    Dlg_to_Scn(  245, 12, 23, 21, x,y,w,h);
    Create( Window.Ear_Ada, Window, Num_resource(Listen_32x32), x,y,w,h, GWindows.Static_Controls.LEFT, HALF_SUNKEN);
    Dlg_to_Scn(  67, 12, 23, 21, x,y,w,h);
    Create( Window.Ear_RC, Window, Num_resource(Listen_32x32), x,y,w,h, GWindows.Static_Controls.LEFT, HALF_SUNKEN);
    Dlg_to_Scn(  5, 80, 56, 10, x,y,w,h);
    Create( Window.Show_Details, Window, "Show details", x,y,w,h, ID => Show_Details);
    Dlg_to_Scn(  97, 12, 60, 15, x,y,w,h);
    Create( Window.Newer_RC, Window, "Resource file is newer!", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => Newer_RC);
    Dlg_to_Scn(  277, 12, 40, 20, x,y,w,h);
    Create( Window.Newer_Ada, Window, "Ada files are newer!", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => Newer_Ada);
    Dlg_to_Scn(  17, 37, 21, 20, x,y,w,h);
    Create( Window.Static_0002, Window, Num_resource(RC_file), x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  197, 37, 21, 20, x,y,w,h);
    Create( Window.Ada_file_icon, Window, Num_resource(Ada_file), x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  367, 37, 21, 20, x,y,w,h);
    Create( Window.Exe_file_icon, Window, Num_resource(Exe_file), x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  180, 80, 45, 10, x,y,w,h);
    Create( Window.Show_Ada_build, Window, "Ada build", x,y,w,h, ID => Show_Ada_build);
    Dlg_to_Scn(  275, 80, 120, 25, x,y,w,h);
    Create( Window.Auto_build_lift_msg, Window, "Last build failed or was stopped. Auto build is temporarily disabled.", x,y,w,h, GWindows.Static_Controls.LEFT, NONE, ID => Auto_build_lift_msg);
    Dlg_to_Scn(  250, 80, 11, 10, x,y,w,h);
    Create( Window.Auto_build_lift_ico, Window, Num_resource(Warning_icon), x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  40, 40, 21, 20, x,y,w,h);
    Create( Window.Static_0003, Window, Num_resource(Arrow_icon), x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  170, 40, 21, 20, x,y,w,h);
    Create( Window.Static_0004, Window, Num_resource(Arrow_icon), x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  220, 40, 21, 20, x,y,w,h);
    Create( Window.Ada_blue_3, Window, Num_resource(Arrow_icon), x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  345, 40, 21, 20, x,y,w,h);
    Create( Window.Static_0005, Window, Num_resource(Arrow_icon), x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  5, 93, 10, 9, x,y,w,h);
    Create( Window.More_less_details, Window, "", x,y,w,h, ID => More_less_details);
    Dlg_to_Scn(  215, 93, 10, 9, x,y,w,h);
    Create( Window.More_less_build, Window, "", x,y,w,h, ID => More_less_build);
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

  -- Last line of resource script file: 263

end GWenerator_Resource_GUI;
