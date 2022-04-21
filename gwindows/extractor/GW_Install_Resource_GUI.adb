---------------------------------------------------------------------------
--  GUI contents of resource script file: GW_Install.rc
--  Transcription time: 2022/01/29  20:34:34
--  GWenerator project file: GW_Install.gwen
--
--  Translated by the RC2GW or by the GWenerator tool.
--  URL: http://sf.net/projects/gnavi
--
--  This file contains only automatically generated code. Do not edit this.
--  Rework the resource script instead, and re-run the translator.
--  RC Grammar version: 14-Apr-2021
---------------------------------------------------------------------------

with GWindows.Types;                    use GWindows.Types;
with GWindows.Drawing;                  use GWindows.Drawing;
with GWindows.Drawing_Objects;
with GWindows.GStrings;                 use GWindows.GStrings;
with System;

pragma Warnings ("U");  --  turn off warnings for unused entity

package body GW_Install_Resource_GUI is

  --  ** Generated code begins here \/ \/ \/.

  --  Dialog at resource line 36

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
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
      Is_Dynamic  : in     Boolean := False)
  is
    x, y, w, h: Integer;
  begin
    Dlg_to_Scn ( 0, 0, 366, 223, x,y,w,h);
    if Left   /= Use_Default then x := Left;   end if;
    if Top    /= Use_Default then y := Top;    end if;
    if Width  /= Use_Default then w := Width;  end if;
    if Height /= Use_Default then h := Height; end if;
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
  end Create_Full_Dialog; -- Conflict_dialog_Type

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out Conflict_dialog_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionally resize Window as designed
     )
  is
    x,y,w,h : Integer;
  begin
    if resize then
    Dlg_to_Scn ( 0, 0, 366, 223, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn ( 235, 201, 49, 14, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.IDOK, Window, "Continue", x,y,w,h, ID => IDOK);
    Create (Window.IDOK_permanent, Window, "Continue", x,y,w,h, ID => IDOK);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.IDOK_permanent);
    else  --  Hide the closing button
      Hide (Window.IDOK);
    end if;
    Dlg_to_Scn ( 291, 201, 50, 14, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.IDCANCEL, Window, "Cancel", x,y,w,h, ID => IDCANCEL);
    Create (Window.IDCANCEL_permanent, Window, "Cancel", x,y,w,h, ID => IDCANCEL);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.IDCANCEL_permanent);
    else  --  Hide the closing button
      Hide (Window.IDCANCEL);
    end if;
    Dlg_to_Scn ( 7, 7, 342, 11, x,y,w,h);
    Create_Label (Window, "The GWindows framework is already installed in that location. Please check the versions.", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 13, 36, 159, 157, x,y,w,h);
    Create (Window.Installed_version, Window, x,y,w,h, True, ID => Installed_version);
    Dlg_to_Scn ( 185, 36, 159, 157, x,y,w,h);
    Create (Window.Installer_Version, Window, x,y,w,h, True, ID => Installer_Version);
    Dlg_to_Scn ( 14, 22, 105, 8, x,y,w,h);
    Create_Label (Window, "Installed version:", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 185, 23, 133, 8, x,y,w,h);
    Create_Label (Window, "Version to be installed:", x,y,w,h, GWindows.Static_Controls.Left, None);
  end Create_Contents;  --  Conflict_dialog_Type

  --  Dialog at resource line 53

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Goodbye_dialog_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "GWindows installation complete";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False)
  is
    x, y, w, h: Integer;
  begin
    Dlg_to_Scn ( 0, 0, 321, 247, x,y,w,h);
    if Left   /= Use_Default then x := Left;   end if;
    if Top    /= Use_Default then y := Top;    end if;
    if Width  /= Use_Default then w := Width;  end if;
    if Height /= Use_Default then h := Height; end if;
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
  end Create_Full_Dialog; -- Goodbye_dialog_Type

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out Goodbye_dialog_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionally resize Window as designed
     )
  is
    x,y,w,h : Integer;
  begin
    if resize then
    Dlg_to_Scn ( 0, 0, 321, 247, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn ( 103, 220, 116, 20, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.IDOK, Window, "Almost done", x,y,w,h, ID => IDOK);
    Create (Window.IDOK_permanent, Window, "Almost done", x,y,w,h, ID => IDOK);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.IDOK_permanent);
    else  --  Hide the closing button
      Hide (Window.IDOK);
    end if;
    Dlg_to_Scn ( 46, 19, 106, 8, x,y,w,h);
    Create_Label (Window, "Installation successful!", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 27, 43, 259, 8, x,y,w,h);
    Create_Label (Window, "Note that you can choose at any time the ANSI or the Unicode version", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 27, 54, 233, 8, x,y,w,h);
    Create_Label (Window, "by running ansi.cmd or unicode.cmd in the gwindows folder, or", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 27, 65, 223, 8, x,y,w,h);
    Create_Label (Window, "by re-running this installation program.", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 15, 12, 21, 20, x,y,w,h);
    Create (Window.Static_0005, Window, Num_resource (Success_icon), x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 15, 84, 179, 8, x,y,w,h);
    Create_Label (Window, "You may want to visit the following web sites:", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 23, 100, 87, 28, x,y,w,h);
    Create (Window.Static_0007, Window, Num_resource (GNAT_Logo), x,y,w,h, GWindows.Static_Controls.Left, Half_Sunken);
    Dlg_to_Scn ( 125, 101, 154, 8, x,y,w,h);
    Create (Window.GNAT_URL, Window, "GNAT Ada compiler (GNAT GPL and GNAT Pro)", x,y,w,h, GWindows.Static_Controls.Left, None, ID => GNAT_URL);
    Dlg_to_Scn ( 125, 112, 78, 8, x,y,w,h);
    Create (Window.MinGW_URL, Window, "GNAT FSF, with MinGW", x,y,w,h, GWindows.Static_Controls.Left, None, ID => MinGW_URL);
    Dlg_to_Scn ( 23, 136, 125, 33, x,y,w,h);
    Create (Window.Static_0008, Window, Num_resource (GNAVI_Logo), x,y,w,h, GWindows.Static_Controls.Left, Half_Sunken);
    Dlg_to_Scn ( 161, 138, 74, 8, x,y,w,h);
    Create (Window.GNAVI_SF_URL, Window, "GNAVI at SourceForge", x,y,w,h, GWindows.Static_Controls.Left, None, ID => GNAVI_SF_URL);
    Dlg_to_Scn ( 161, 149, 83, 8, x,y,w,h);
    Create (Window.GNAVI_Discuss_URL, Window, "GNAVI Discuss mailing list", x,y,w,h, GWindows.Static_Controls.Left, None, ID => GNAVI_Discuss_URL);
    Dlg_to_Scn ( 161, 160, 35, 8, x,y,w,h);
    Create (Window.GNAVI_URL, Window, "GNAVI.org", x,y,w,h, GWindows.Static_Controls.Left, None, ID => GNAVI_URL);
    Dlg_to_Scn ( 23, 180, 71, 33, x,y,w,h);
    Create (Window.Static_0009, Window, Num_resource (ResEdit_Logo), x,y,w,h, GWindows.Static_Controls.Left, Half_Sunken);
    Dlg_to_Scn ( 103, 178, 190, 9, x,y,w,h);
    Create (Window.ResEdit_URL, Window, "ResEdit - a free graphic resource editor", x,y,w,h, GWindows.Static_Controls.Left, None, ID => ResEdit_URL);
    Dlg_to_Scn ( 105, 190, 214, 9, x,y,w,h);
    Create_Label (Window, "To be used with the .RC to Ada translator (GWenerator) and", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 105, 201, 214, 9, x,y,w,h);
    Create_Label (Window, " the GWindows framework. A version is included with GWenerator", x,y,w,h, GWindows.Static_Controls.Left, None);
  end Create_Contents;  --  Goodbye_dialog_Type

  --  Dialog at resource line 81

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Goodbye_dialog_2_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "GWindows installation complete";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False)
  is
    x, y, w, h: Integer;
  begin
    Dlg_to_Scn ( 0, 0, 303, 216, x,y,w,h);
    if Left   /= Use_Default then x := Left;   end if;
    if Top    /= Use_Default then y := Top;    end if;
    if Width  /= Use_Default then w := Width;  end if;
    if Height /= Use_Default then h := Height; end if;
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
  end Create_Full_Dialog; -- Goodbye_dialog_2_Type

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out Goodbye_dialog_2_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionally resize Window as designed
     )
  is
    x,y,w,h : Integer;
  begin
    if resize then
    Dlg_to_Scn ( 0, 0, 303, 216, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn ( 93, 188, 107, 20, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.IDOK, Window, "Done", x,y,w,h, ID => IDOK);
    Create (Window.IDOK_permanent, Window, "Done", x,y,w,h, ID => IDOK);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.IDOK_permanent);
    else  --  Hide the closing button
      Hide (Window.IDOK);
    end if;
    Dlg_to_Scn ( 17, 9, 21, 20, x,y,w,h);
    Create (Window.Static_0001, Window, Num_resource (Success_icon), x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 58, 14, 145, 8, x,y,w,h);
    Create_Label (Window, "Ready for starting with GWindows.", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 39, 35, 254, 41, x,y,w,h);
    Create( Window.Static_0003, Window, "GWindows", x, y, w, h);
    Dlg_to_Scn ( 49, 49, 156, 8, x,y,w,h);
    Create (Window.Open_folder, Window, "Open the GWindows folder", x,y,w,h, ID => Open_folder);
    Dlg_to_Scn ( 49, 63, 200, 8, x,y,w,h);
    Create (Window.Open_user_guide, Window, "Open the GWindows User Guide", x,y,w,h, ID => Open_user_guide);
    Dlg_to_Scn ( 39, 79, 254, 54, x,y,w,h);
    Create( Window.Static_0004, Window, "GWenerator", x, y, w, h);
    Dlg_to_Scn ( 49, 93, 212, 8, x,y,w,h);
    Create (Window.Build_gwenerator, Window, "Build the GWenerator tools now", x,y,w,h, ID => Build_gwenerator);
    Dlg_to_Scn ( 49, 106, 146, 8, x,y,w,h);
    Create (Window.Open_gwenerator_folder, Window, "Open the GWenerator folder", x,y,w,h, ID => Open_gwenerator_folder);
    Dlg_to_Scn ( 49, 120, 174, 8, x,y,w,h);
    Create (Window.Open_gwenerator_doc, Window, "Open the GWenerator documentation", x,y,w,h, ID => Open_gwenerator_doc);
    Dlg_to_Scn ( 39, 137, 254, 41, x,y,w,h);
    Create( Window.Static_0005, Window, "GNATCOM", x, y, w, h);
    Dlg_to_Scn ( 49, 150, 220, 8, x,y,w,h);
    Create (Window.Build_gnatcom, Window, "Build the GNATCOM tools now", x,y,w,h, ID => Build_gnatcom);
    Dlg_to_Scn ( 49, 164, 228, 8, x,y,w,h);
    Create (Window.Open_gnatcom_folder, Window, "Open the GNATCOM folder", x,y,w,h, ID => Open_gnatcom_folder);
  end Create_Contents;  --  Goodbye_dialog_2_Type

  --  Dialog at resource line 104

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Main_install_dialog_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "GWindows installer, version ";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False)
  is
    x, y, w, h: Integer;
  begin
    Dlg_to_Scn ( 0, 0, 314, 239, x,y,w,h);
    if Left   /= Use_Default then x := Left;   end if;
    if Top    /= Use_Default then y := Top;    end if;
    if Width  /= Use_Default then w := Width;  end if;
    if Height /= Use_Default then h := Height; end if;
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
  end Create_Full_Dialog; -- Main_install_dialog_Type

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out Main_install_dialog_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionally resize Window as designed
     )
  is
    x,y,w,h : Integer;
  begin
    if resize then
    Dlg_to_Scn ( 0, 0, 314, 239, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn ( 11, 0, 290, 43, x,y,w,h);
    Create( Window.Static_0001, Window, "", x, y, w, h);
    Dlg_to_Scn ( 192, 6, 80, 8, x,y,w,h);
    Create_Label (Window, "Open Source Visual", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 176, 19, 109, 8, x,y,w,h);
    Create_Label (Window, "Rapid Application Development", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 192, 32, 80, 8, x,y,w,h);
    Create_Label (Window, "for Microsoft Windows", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 9, 47, 284, 8, x,y,w,h);
    Create (Window.Setup_title, Window, "This is the GWindows programming framework's setup, version ", x,y,w,h, GWindows.Static_Controls.Left, None, ID => Setup_title);
    Dlg_to_Scn ( 9, 60, 292, 52, x,y,w,h);
    Create( Window.Static_0005, Window, "Location", x, y, w, h);
    Dlg_to_Scn ( 36, 72, 261, 8, x,y,w,h);
    Create_Label (Window, "Please specify a directory where you want to install the GWindows framework.", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 36, 99, 223, 8, x,y,w,h);
    Create_Label (Window, "NB: you can install GWindows at several places.", x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 36, 83, 215, 14, x,y,w,h);
    Create (Window.Directory_edit, Window, "", x,y,w,h, Horizontal_Scroll => True, Read_Only => False, ID => Directory_edit);
    Dlg_to_Scn ( 258, 82, 35, 19, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.Directory_select_button, Window, "Pick...", x,y,w,h, ID => Directory_select_button);
    Create (Window.Directory_select_button_permanent, Window, "Pick...", x,y,w,h, ID => Directory_select_button);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.Directory_select_button_permanent);
    else  --  Hide the closing button
      Hide (Window.Directory_select_button);
    end if;
    Dlg_to_Scn ( 25, 153, 265, 8, x,y,w,h);
    Create (Window.UNICODE_choice, Window, "Unicode (16-bit International characters), Ada: Wide_Character / Wide_String", x,y,w,h, ID => UNICODE_choice);
    Dlg_to_Scn ( 25, 136, 212, 8, x,y,w,h);
    Create (Window.ANSI_choice, Window, "ANSI (8-bit Western characters), Ada: Character / String", x,y,w,h, ID => ANSI_choice);
    Dlg_to_Scn ( 9, 120, 293, 52, x,y,w,h);
    Create( Window.Static_0008, Window, "Character and string encoding. Please choose (choice can be reversed later):", x, y, w, h);
    Dlg_to_Scn ( 36, 6, 125, 33, x,y,w,h);
    Create (Window.Static_0009, Window, Num_resource (GNAVI_Logo), x,y,w,h, GWindows.Static_Controls.Left, Half_Sunken);
    Dlg_to_Scn ( 9, 184, 224, 8, x,y,w,h);
    Create (Window.GNATCOM_check, Window, "Install GNATCOM - mandatory for GWindows", x,y,w,h, ID => GNATCOM_check);
    Disable (Window.GNATCOM_check);
    Disable (Window.GNATCOM_check);
    Dlg_to_Scn ( 9, 200, 223, 25, x,y,w,h);
    Create (Window.GWen_check, Window, "Install the GWenerator Ada code generator (processes ANSI resources only, but can be used for Unicode implementations)", x,y,w,h, ID => GWen_check);
    Multi_Line (Window.GWen_check);
    Dlg_to_Scn ( 245, 192, 60, 19, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.IDOK, Window, "Install", x,y,w,h, ID => IDOK);
    Create (Window.IDOK_permanent, Window, "Install", x,y,w,h, ID => IDOK);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.IDOK_permanent);
    else  --  Hide the closing button
      Hide (Window.IDOK);
    end if;
    Dlg_to_Scn ( 245, 216, 60, 19, x,y,w,h);
    --  Both versions of the button are created.
    --  The more meaningful one is made visible, but this choice
    --  can be reversed, for instance on a "Browse" button.
    Create (Window.IDCANCEL, Window, "Cancel", x,y,w,h, ID => IDCANCEL);
    Create (Window.IDCANCEL_permanent, Window, "Cancel", x,y,w,h, ID => IDCANCEL);
    if for_dialog then  --  Hide the non-closing button
      Hide (Window.IDCANCEL_permanent);
    else  --  Hide the closing button
      Hide (Window.IDCANCEL);
    end if;
  end Create_Contents;  --  Main_install_dialog_Type

  --  Dialog at resource line 132

  --    a) Create_As_Dialog & create all contents -> ready-to-use dialog
  --
  procedure Create_Full_Dialog
     (Window      : in out Unpack_dialog_Type;
      Parent      : in out GWindows.Base.Base_Window_Type'Class;
      Title       : in     GString := "Unpacking by Zip-Ada";
      Left        : in     Integer := Use_Default; -- Default = as designed
      Top         : in     Integer := Use_Default; -- Default = as designed
      Width       : in     Integer := Use_Default; -- Default = as designed
      Height      : in     Integer := Use_Default; -- Default = as designed
      Help_Button : in     Boolean := False;
      Is_Dynamic  : in     Boolean := False)
  is
    x, y, w, h: Integer;
  begin
    Dlg_to_Scn ( 0, 0, 296, 79, x,y,w,h);
    if Left   /= Use_Default then x := Left;   end if;
    if Top    /= Use_Default then y := Top;    end if;
    if Width  /= Use_Default then w := Width;  end if;
    if Height /= Use_Default then h := Height; end if;
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
  end Create_Full_Dialog; -- Unpack_dialog_Type

  --    b) Create all contents, not the window itself (must be
  --        already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out Unpack_dialog_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionally resize Window as designed
     )
  is
    x,y,w,h : Integer;
  begin
    if resize then
    Dlg_to_Scn ( 0, 0, 296, 79, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn ( 51, 39, 183, 17, x,y,w,h);
    Create (Window.Unpack_progress, Window, x,y,w,h, Horizontal, False);
    Dlg_to_Scn ( 9, 7, 277, 12, x,y,w,h);
    Create (Window.File_name, Window, "(name)", x,y,w,h, GWindows.Static_Controls.Left, None, ID => File_name);
    Dlg_to_Scn ( 16, 38, 21, 20, x,y,w,h);
    Create (Window.Static_0001, Window, Num_resource (Zip_icon), x,y,w,h, GWindows.Static_Controls.Left, None);
    Dlg_to_Scn ( 251, 36, 21, 20, x,y,w,h);
    Create (Window.Static_0002, Window, Num_resource (Ada_doc_icon), x,y,w,h, GWindows.Static_Controls.Left, None);
  end Create_Contents;  --  Unpack_dialog_Type

  --  ** Generated code ends here /\ /\ /\.

  --  ** Some helper utilities (body).

  procedure Dlg_to_Scn (  --  Converts dialog coords to screen (pixel) coordinates.
    xd,yd,wd,hd :  in Integer;
    xs,ys,ws,hs : out Integer)
  is
    --  function GetDialogBaseUnits return Integer;
    --  pragma Import (StdCall, GetDialogBaseUnits, "GetDialogBaseUnits");
    --  baseunit, baseunitX, baseunitY: Integer;
    baseunitX: constant:= 6;
    baseunitY: constant:= 13;
  begin
    --  baseunit := GetDialogBaseUnits; -- this gives X=8, Y=16 (SYSTEM font)
    --  baseunitX := baseunit mod (2 ** 16);
    --  baseunitY := baseunit  / (2 ** 16);
    --  NB: the other way with MapDialogRect works only
    --    by full moon, hence the user-defined units.
    xs := (xd * baseunitX) / 4;
    ws := (wd * baseunitX) / 4;
    ys := (yd * baseunitY) / 8;
    hs := (hd * baseunitY) / 8;
  end Dlg_to_Scn;

  package Common_Fonts is
    GUI_Font : GWindows.Drawing_Objects.Font_Type;
    URL_Font : GWindows.Drawing_Objects.Font_Type;
    --  ^ These fonts are created once, at startup
    --    it avoid GUI resource leak under Windows 95/98/ME
    procedure Create_Common_Fonts;
    --  in initialisation part if this pkg becomes standalone
  end Common_Fonts;

  procedure Use_GUI_Font (Window: in out GWindows.Base.Base_Window_Type'Class)
  is
  begin
    --  Use Standard Windows GUI font instead of system font
    GWindows.Base.Set_Font (Window, Common_Fonts.GUI_Font);
  end Use_GUI_Font;

  function Num_resource (id: Natural) return GString is
    img: constant String:= Integer'Image(id);
  begin
    return To_GString_From_String('#' & img(img'First+1..img'Last));
  end Num_resource;

  package body Common_Fonts is

    procedure Create_Common_Fonts is

     type Face_Name_Type is array (1 .. 32) of GWindows.GChar_C;

     type LOGFONT is record
       lfHeight : Interfaces.C.long;
       lfWidth  : Interfaces.C.long;
       lfEscapement  : Interfaces.C.long;
       lfOrientation : Interfaces.C.long;
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

     Log_of_current_font : aliased LOGFONT;

     subtype PVOID   is System.Address;                      --  winnt.h
     subtype LPVOID  is PVOID;                               --  windef.h

     function GetObject
       (hgdiobj  : GWindows.Types.Handle  := GWindows.Drawing_Objects.Handle(GUI_Font);
        cbBufferl: Interfaces.C.int       := LOGFONT'Size / 8;
        lpvObject: LPVOID                 := Log_of_current_font'Address)
       return Interfaces.C.int;
     pragma Import (StdCall, GetObject,
                      "GetObject" & Character_Mode_Identifier);

     function CreateFontIndirect
       (lpvObject: LPVOID                 := Log_of_current_font'Address)
       return GWindows.Types.Handle;
     pragma Import (StdCall, CreateFontIndirect,
                      "CreateFontIndirect" & Character_Mode_Identifier);

    begin
      GWindows.Drawing_Objects.Create_Stock_Font(
        GUI_Font,
        GWindows.Drawing_Objects.Default_GUI
      );
      if GetObject = 0 then
        GWindows.Drawing_Objects.Create_Font (URL_Font,
          "MS Sans Serif",
          14, Underline => True);
            --  !! ^ Not so nice (non-unsharpened font, size ~..., color ?)
      else
        Log_of_current_font.lfUnderline := Interfaces.C.char'Val(1);
        GWindows.Drawing_Objects.Handle (URL_Font, CreateFontIndirect);
      end if;
    end Create_Common_Fonts;

  end Common_Fonts;

begin
  Common_Fonts.Create_Common_Fonts;

  --  Last line of resource script file: 199

end GW_Install_Resource_GUI;
