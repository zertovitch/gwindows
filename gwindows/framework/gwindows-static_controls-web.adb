with GWindows.Cursors;                  use GWindows.Cursors;
--  with GWindows.Message_Boxes;            use GWindows.Message_Boxes;
with GWindows.GStrings;                 use GWindows.GStrings;
with GWindows.Types;

with System;

package body GWindows.Static_Controls.Web is

  -----------
  -- Start --
  -----------

  procedure Start (
    File       : in String;
    Parameter  : in String := "";
    Minimized  : in Boolean := False
  )
  is

    C_Operation  : aliased Interfaces.C.char_array :=
                           Interfaces.C.To_C ("open");
    C_Executable : aliased Interfaces.C.char_array :=
                           Interfaces.C.To_C (File);
    C_Parameter  : aliased Interfaces.C.char_array :=
                           Interfaces.C.To_C (Parameter);
    --  Parts from Win32Ada:
    subtype PVOID is System.Address;
    subtype HANDLE is PVOID;                    --  winnt.h :144
    subtype HWND is HANDLE;                     --  windef.h :178
    subtype HINSTANCE is HANDLE;
    subtype INT is Interfaces.C.int;                  --  windef.h
    --
    Exe : HINSTANCE;
    pragma Warnings (Off, Exe);
    SW_SHOWNORMAL    : constant := 1;
    SW_SHOWMINIMIZED : constant := 2;
    sw : constant array (Boolean) of INT :=
      (SW_SHOWNORMAL,
       SW_SHOWMINIMIZED);
    function GetFocus return HWND;              --  winuser.h:2939
    pragma Import (Stdcall, GetFocus, "GetFocus");
    subtype CHAR is Interfaces.C.char;
    type PCCH is access constant CHAR;
    type PCHAR is access all CHAR;
    subtype LPCSTR is PCCH;
    subtype LPSTR is PCHAR;
    function ShellExecuteA
      (hwnd0 : HWND;
       lpOperation : LPCSTR;
       lpFile : LPCSTR;
       lpParameters : LPSTR;
       lpDirectory : LPCSTR;
       nShowCmd : INT)
      return HINSTANCE;               --  shellapi.h:54
    pragma Import (Stdcall, ShellExecuteA, "ShellExecuteA"); --  shellapi.h:54
    function ShellExecute
      (hwnd0 : HWND;
       lpOperation : LPCSTR;
       lpFile : LPCSTR;
       lpParameters : LPSTR;
       lpDirectory : LPCSTR;
       nShowCmd : INT)
      return HINSTANCE
      renames ShellExecuteA;                       --  shellapi.h:54
  begin
    Exe := ShellExecute
     (hwnd0        => GetFocus,
      lpOperation  => C_Operation (C_Operation'First)'Unchecked_Access,
      lpFile       => C_Executable (C_Executable'First)'Unchecked_Access,
      lpParameters => C_Parameter (C_Parameter'First)'Unchecked_Access,
      lpDirectory  => null,
      nShowCmd     => sw (Minimized));
  end Start;

  type URL_Access is access all URL_Type;

  use Interfaces.C;

  --  Overriden methods:
  procedure On_Click (Window : in out URL_Type) is
  begin
    Start (
      GWindows.GStrings.To_String (
        GWindows.GStrings.To_GString_From_Unbounded (Window.URL)
      )
    );
  end On_Click;

  ------------------------------
  -- "Pointing finger" cursor --
  ------------------------------
  cur_finger : Cursor_Type := 0;

  procedure On_Message
     (Window       : in out URL_Type;
      message      : in     Interfaces.C.unsigned;
      wParam       : in     GWindows.Types.Wparam;
      lParam       : in     GWindows.Types.Lparam;
      Return_Value : in out GWindows.Types.Lresult)
  is
    WM_MOUSEMOVE  : constant := 16#200#;
    IDC_HAND : constant := 32649;
  begin
    if message = WM_MOUSEMOVE then
      if cur_finger = 0 then
        cur_finger := Load_System_Cursor (IDC_HAND);
        if cur_finger = 0 then
          --  IDC_HAND not found: older system, wrong service pack,
          --  bad moon phase, etc.
          cur_finger := Load_Cursor ("Finger_cursor");
        end if;
      end if;
      if cur_finger /= 0 then
        Set_Cursor (cur_finger);
      end if;
    end if;
    --  Call parent method
    On_Message (
      Label_Type (Window),
      message,
      wParam,
      lParam,
      Return_Value
    );
  end On_Message;

  --
  GUI_Font : GWindows.Drawing_Objects.Font_Type;
  URL_Font : GWindows.Drawing_Objects.Font_Type;
  --  ^ These fonts are created once, at startup
  --    it avoids GUI resource leak under Windows 95/98/ME

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
                    "GetObject" & Character_Mode_Identifier);

   function CreateFontIndirect
     (lpvObject : LPVOID                 := Log_of_current_font'Address)
     return GWindows.Types.Handle;
   pragma Import (StdCall, CreateFontIndirect,
                    "CreateFontIndirect" & Character_Mode_Identifier);

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

   ----------------
   -- Create_URL --
   ----------------

   procedure Create
     (Static     : in out URL_Type;
      Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      URL        : in     GString; -- Address local or on the Internet
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Alignment  : in     Alignment_Type                       :=
        GWindows.Static_Controls.Left;
      Border     : in     Border_Type                          := None;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True;
      Is_Dynamic : in     Boolean                              := False)
   is
   begin
      Create (Static,
              Parent, Text, Left, Top, Width, Height,
              Alignment, Border,
              ID, Show,
              Is_Dynamic);
      Set_Font (Static, URL_Font);
      Static.URL := To_GString_Unbounded (URL);
   end Create;

   procedure Create_URL
     (Parent     : in out GWindows.Base.Base_Window_Type'Class;
      Text       : in     GString;
      URL        : in     GString;
      Left       : in     Integer;
      Top        : in     Integer;
      Width      : in     Integer;
      Height     : in     Integer;
      Alignment  : in     Alignment_Type                       :=
        GWindows.Static_Controls.Left;
      Border     : in     Border_Type                          := None;
      ID         : in     Integer                              := 0;
      Show       : in     Boolean                              := True)
   is
     Temp_URL : constant URL_Access := new URL_Type;
   begin
     Create (Temp_URL.all,
             Parent, Text, URL, Left, Top, Width, Height,
             Alignment, Border,
             ID, Show,
             Is_Dynamic => True);
   end Create_URL;

   procedure Create_and_Swap
      (To_Show    : in out URL_Type;
       To_Hide    : in out Label_Type;
       Parent     : in out GWindows.Base.Base_Window_Type'Class;
       URL        : in     GString;
       Alignment  : in     Alignment_Type := GWindows.Static_Controls.Left;
       Border     : in     Border_Type                          := None;
       Is_Dynamic : in     Boolean := False
      )
   is
   begin
      Create (To_Show,
              Parent,
              Text (To_Hide),
              URL,
              Left (To_Hide),
              Top (To_Hide),
              Width (To_Hide),
              Height (To_Hide),
              Alignment,
              Border,
              ID (To_Hide),
              True,
              Is_Dynamic
      );
      Hide (To_Hide);
   end Create_and_Swap;

begin
  Create_Common_Fonts;
end GWindows.Static_Controls.Web;
