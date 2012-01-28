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

with GWindows.Types;                    use GWindows.Types;
with GWindows.Drawing;                  use GWindows.Drawing;
with GWindows.Drawing_Objects;
with System;

package body GW_Install_Resource_GUI is

  -- ** Generated code begins here \/ \/ \/.


  -- Dialog at resource line 28

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
      Is_Dynamic  : in     Boolean := False)
  is
    x,y,w,h: Integer;
  begin
    Dlg_to_Scn(  0, 0, 366, 223, x,y,w,h);
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
  end Create_Full_Dialog; -- Conflict_dialog_Type

  --  b) Create all contents, not the window itself (must be
  --      already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out Conflict_dialog_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     )
  is
    x,y,w,h: Integer;
  begin
    if resize then
    Dlg_to_Scn(  0, 0, 366, 223, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn(  235, 201, 49, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.IDOK, Window, "Continue", x,y,w,h, IDOK);
    Create( Window.IDOK_permanent, Window, "Continue", x,y,w,h, IDOK);
    if for_dialog then -- hide the non-closing button
      Hide(Window.IDOK_permanent);
    else -- hide the closing button
      Hide(Window.IDOK);
    end if;
    Dlg_to_Scn(  291, 201, 50, 14, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.IDCANCEL, Window, "Cancel", x,y,w,h, IDCANCEL);
    Create( Window.IDCANCEL_permanent, Window, "Cancel", x,y,w,h, IDCANCEL);
    if for_dialog then -- hide the non-closing button
      Hide(Window.IDCANCEL_permanent);
    else -- hide the closing button
      Hide(Window.IDCANCEL);
    end if;
    Dlg_to_Scn(  7, 7, 342, 11, x,y,w,h);
    Create_label( Window, "The GWindows framework is already installed in that location. Please check the versions.", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  13, 36, 159, 157, x,y,w,h);
    Create( Window.Installed_version, Window, x,y,w,h, TRUE, Installed_version);
    Dlg_to_Scn(  185, 36, 159, 157, x,y,w,h);
    Create( Window.Installer_Version, Window, x,y,w,h, TRUE, Installer_Version);
    Dlg_to_Scn(  14, 22, 105, 8, x,y,w,h);
    Create_label( Window, "Installed version:", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  185, 23, 133, 8, x,y,w,h);
    Create_label( Window, "Version to be installed:", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
  end Create_Contents; -- Conflict_dialog_Type


  -- Dialog at resource line 45

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
      Is_Dynamic  : in     Boolean := False)
  is
    x,y,w,h: Integer;
  begin
    Dlg_to_Scn(  0, 0, 314, 239, x,y,w,h);
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
  end Create_Full_Dialog; -- Install_dialog_Type

  --  b) Create all contents, not the window itself (must be
  --      already created) -> can be used in/as any kind of window.
  --
  procedure Create_Contents
     ( Window      : in out Install_dialog_Type;
       for_dialog  : in     Boolean; -- True: buttons do close the window
       resize      : in     Boolean:= False -- optionnally resize Window as designed
     )
  is
    x,y,w,h: Integer;
  begin
    if resize then
    Dlg_to_Scn(  0, 0, 314, 239, x,y,w,h);
      Move(Window, x,y);
      Client_Area_Size(Window, w, h);
    end if;
    Use_GUI_Font(Window);
    Dlg_to_Scn(  240, 192, 60, 17, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.IDOK, Window, "Install", x,y,w,h, IDOK);
    Create( Window.IDOK_permanent, Window, "Install", x,y,w,h, IDOK);
    if for_dialog then -- hide the non-closing button
      Hide(Window.IDOK_permanent);
    else -- hide the closing button
      Hide(Window.IDOK);
    end if;
    Dlg_to_Scn(  240, 216, 60, 17, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.IDCANCEL, Window, "Cancel", x,y,w,h, IDCANCEL);
    Create( Window.IDCANCEL_permanent, Window, "Cancel", x,y,w,h, IDCANCEL);
    if for_dialog then -- hide the non-closing button
      Hide(Window.IDCANCEL_permanent);
    else -- hide the closing button
      Hide(Window.IDCANCEL);
    end if;
    Dlg_to_Scn(  13, 64, 267, 8, x,y,w,h);
    Create_label( Window, "Please specify a directory where you want to install the GWindows framework.", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  13, 79, 122, 8, x,y,w,h);
    Create_label( Window, "NB: you can install it at several places.", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  13, 94, 251, 17, x,y,w,h);
    Create( Window.IDC_EDIT1, Window, "", x,y,w,h, TRUE, IDC_EDIT1);
    Dlg_to_Scn(  273, 94, 29, 17, x,y,w,h);
    -- Both versions of the button are created.
    -- The more meaningful one is made visible, but this choice
    -- can be reversed, for instance on a "Browse" button.
    Create( Window.Directory_select_button, Window, "...", x,y,w,h, Directory_select_button);
    Create( Window.Directory_select_button_permanent, Window, "...", x,y,w,h, Directory_select_button);
    if for_dialog then -- hide the non-closing button
      Hide(Window.Directory_select_button_permanent);
    else -- hide the closing button
      Hide(Window.Directory_select_button);
    end if;
    Dlg_to_Scn(  24, 168, 265, 8, x,y,w,h);
    Create( Window.UNICODE_choice, Window, "Unicode (16-bit International characters), Ada: Wide_Character / Wide_String", x,y,w,h, UNICODE_choice);
    Dlg_to_Scn(  24, 150, 212, 8, x,y,w,h);
    Create( Window.ANSI_choice, Window, "ANSI (8-bit Western characters), Ada: Character / String", x,y,w,h, ANSI_choice);
    Dlg_to_Scn(  11, 134, 290, 52, x,y,w,h);
    Create( Window.Static_0003, Window, "Character encoding - please choose:", x,y,w,h);
    Dlg_to_Scn(  13, 14, 125, 33, x,y,w,h);
    Create( Window.Static_0004, Window, Num_resource(GNAVI_Logo), x,y,w,h, GWindows.Static_Controls.LEFT, HALF_SUNKEN);
    Dlg_to_Scn(  154, 13, 109, 8, x,y,w,h);
    Create_label( Window, "Open Source Visual", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  154, 26, 109, 8, x,y,w,h);
    Create_label( Window, "Rapid Application Development", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
    Dlg_to_Scn(  154, 39, 109, 8, x,y,w,h);
    Create_label( Window, "for Microsoft Windows", x,y,w,h, GWindows.Static_Controls.LEFT, NONE);
  end Create_Contents; -- Install_dialog_Type


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

  -- Last line of resource script file: 101

end GW_Install_Resource_GUI;
