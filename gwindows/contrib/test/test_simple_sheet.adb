--  Demo for the GWindows.Simple_Sheet package.
--
--  See: gwindows\contrib, gwindows\gwindows_contrib.gpr, gwindows\contrib\test

with GWindows.Application;
with GWindows.Base;
with GWindows.Buttons;
--  with GWindows.Message_Boxes;            use GWindows.Message_Boxes;
with GWindows.Windows.Main;             use GWindows.Windows.Main;
with GWindows.Simple_Sheet;             use GWindows.Simple_Sheet;
with GWindows.GStrings;                 use GWindows.GStrings;
with GWindows.Drawing_Objects;

--  Only needed for having a decent font (see Common_Fonts below).
with Interfaces.C;
with System;
with GWindows.Types;
with GWindows;

procedure Test_Simple_Sheet is

  Top       : GWindows.Windows.Main.Main_Window_Type;
  Sheet     : GWindows.Simple_Sheet.Simple_Sheet_Type;
  Button,
  Btn_Copy,
  Btn_Paste : GWindows.Buttons.Button_Type;

  rows          : constant Natural := 10;
  columns       : constant Natural := 10;
  row_height    : Positive := 20;
  column_width  : Positive := 75;
  row_growth    : Integer := 1;

  procedure Button_action (btn : in out GWindows.Base.Base_Window_Type'Class)
  is
    pragma Warnings (off, btn);
  begin
    if row_height >= 50 then
      row_growth := -1;
    elsif row_height <= 15 then
      row_growth := 1;
    end if;
    row_height := row_height + row_growth;
    Set_Row_Height (Sheet, row_height);
    Text (Top, "Cell (10, 10) contains: " & Get_Cell_Text (Sheet, 10, 10));
  end Button_action;

  procedure Copy_action (btn : in out GWindows.Base.Base_Window_Type'Class)
  is
    pragma Warnings (off, btn);
  begin
     Copy_to_Clipboard (Sheet);
  end Copy_action;

  procedure Paste_action (btn : in out GWindows.Base.Base_Window_Type'Class)
  is
    pragma Warnings (off, btn);
  begin
     Paste_from_Clipboard (Sheet);
  end Paste_action;

  ----------------------------------------------------------------------
  -- The following (Common_Fonts) is to get the usual GUI fonts !...  --
  -- The code below is taken from what GWenerator is generating.      --
  ----------------------------------------------------------------------

  package Common_Fonts is
    GUI_Font : GWindows.Drawing_Objects.Font_Type;
    URL_Font : GWindows.Drawing_Objects.Font_Type;
    --  ^ These fonts are created once, at startup
    --   it avoid GUI resource leak under Windows 95/98/ME
    procedure Create_Common_Fonts;
    --  in initialisation part if this pkg becomes standalone
  end Common_Fonts;

  procedure Use_GUI_Font (Window : in out GWindows.Base.Base_Window_Type'Class)
  is
  begin
    --  Use Standard Windows GUI font instead of system font
    GWindows.Base.Set_Font (Window, Common_Fonts.GUI_Font);
  end Use_GUI_Font;

  package body Common_Fonts is

    use Interfaces.C;

    procedure Create_Common_Fonts is

     type Face_Name_Type is array (1 .. 32) of GWindows.GChar_C;

     type LOGFONT is record
       lfHeight : Interfaces.C.long;
       lfWidth : Interfaces.C.long;
       lfEscapement : Interfaces.C.long;
       lfOrientation : Interfaces.C.long;
       lfWeight : Interfaces.C.long;
       lfItalic : Interfaces.C.char;
       lfUnderline : Interfaces.C.char;
       lfStrikeOut : Interfaces.C.char;
       lfCharSet : Interfaces.C.char;
       lfOutPrecision : Interfaces.C.char;
       lfClipPrecision : Interfaces.C.char;
       lfQuality : Interfaces.C.char;
       lfPitchAndFamily : Interfaces.C.char;
       lfFaceName : Face_Name_Type;
     end record;

     Log_of_current_font : aliased LOGFONT;

     subtype PVOID   is System.Address;                      --  winnt.h
     subtype LPVOID  is PVOID;                               --  windef.h

     function GetObject
       (hgdiobj   : GWindows.Types.Handle  :=
          GWindows.Drawing_Objects.Handle (GUI_Font);
        cbBufferl : Interfaces.C.int       := LOGFONT'Size / 8;
        lpvObject : LPVOID                 := Log_of_current_font'Address)
       return Interfaces.C.int;
     pragma Import (StdCall, GetObject,
                      "GetObject" & GWindows.Character_Mode_Identifier);

     function CreateFontIndirect
       (lpvObject : LPVOID                 := Log_of_current_font'Address)
       return GWindows.Types.Handle;
     pragma Import (StdCall, CreateFontIndirect,
                      "CreateFontIndirect" &
                      GWindows.Character_Mode_Identifier);

    begin
      GWindows.Drawing_Objects.Create_Stock_Font (
        GUI_Font,
        GWindows.Drawing_Objects.Default_GUI
      );
      if GetObject = 0 then
        GWindows.Drawing_Objects.Create_Font (URL_Font,
          "MS Sans Serif",
          14, Underline => True);
            --  !! ^ Not so nice (non-unsharpened font, size ~..., color ?)
      else
        Log_of_current_font.lfUnderline := Interfaces.C.char'Val (1);
        GWindows.Drawing_Objects.Handle (URL_Font, CreateFontIndirect);
      end if;
    end Create_Common_Fonts;

  end Common_Fonts;

begin
  Create (Top, "Hello World");
  Size (Top, 800, 300);
  Common_Fonts.Create_Common_Fonts;
  Use_GUI_Font (Top);
  --
  --  The spreadsheet is created here.
  --
  GWindows.Simple_Sheet.Create (
    Sheet          => Sheet,
    Parent         => Top,
    Top            => 0,
    Left           => 0,
    Width          => column_width * columns,
    Height         => row_height * rows,
    Rows           => rows,
    Columns        => columns,
    Column_Borders => False,
    Row_Borders    => False);

  Horizontal_Scroll_Bar (
    Window => Sheet,
    State  => True);

  --
  --  We fill the sheet with some contents.
  --
  for I in 1 .. 10 loop
    for J in 1 .. 10 loop

      if (I = J) and (I = 2 or I = 4 or I = 6 or I = 8) then
        null;
      else
        Set_Cell_Text
          (
            Sheet     => Sheet,
            Row       => I,
            Column    => J,
            Text      =>
              To_GString_From_String (
                Integer'Image (I) & "," & Integer'Image (J)),
            Read_Only => I = 1 or J = 1  --  Top row and Left column will look like headers
          );
      end if;

    end loop;
  end loop;

  --
  --  We add a few buttons *within* the spreadsheet!
  --
  GWindows.Buttons.Create
    (Button => Button,
    Parent => Get_Item_Within_Cell (Sheet => Sheet, Row => 2, Column => 2).all,
    Text => "Show (10, 10)",
    Left => 1,
    Top => 1,
    Width => column_width,
    Height => row_height);

  GWindows.Buttons.Create
    (Button => Btn_Copy,
    Parent => Get_Item_Within_Cell (Sheet => Sheet, Row => 4, Column => 4).all,
    Text => "Copy grid",
    Left => 1,
    Top => 1,
    Width => column_width,
    Height => row_height);

  GWindows.Buttons.Create
    (Button => Btn_Paste,
    Parent => Get_Item_Within_Cell (Sheet => Sheet, Row => 6, Column => 6).all,
    Text => "Paste grid",
    Left => 1,
    Top => 1,
    Width => column_width,
    Height => row_height);

  GWindows.Buttons.On_Click_Handler
    (Button, Button_action'Unrestricted_Access);
  GWindows.Buttons.On_Click_Handler
    (Btn_Copy, Copy_action'Unrestricted_Access);
  GWindows.Buttons.On_Click_Handler
    (Btn_Paste, Paste_action'Unrestricted_Access);

  --  Visible(Sheet);

  Visible (Top);
  GWindows.Application.Message_Loop;

end Test_Simple_Sheet;
