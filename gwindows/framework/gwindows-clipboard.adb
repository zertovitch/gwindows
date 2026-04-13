------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                  G W I N D O W S . C L I P B O A R D                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
-- MIT License                                                              --
--                                                                          --
-- Permission is hereby granted, free of charge, to any person obtaining    --
-- a copy of this software and associated documentation files (the          --
-- "Software"), to deal in the Software without restriction, including      --
-- without limitation the rights to use, copy, modify, merge, publish,      --
-- distribute, sublicense, and/or sell copies of the Software, and to       --
-- permit persons to whom the Software is furnished to do so, subject to    --
-- the following conditions:                                                --
--                                                                          --
-- The above copyright notice and this permission notice shall be included  --
-- in all copies or substantial portions of the Software.                   --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,          --
-- EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF       --
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.   --
-- IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY     --
-- CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,     --
-- TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE        --
-- SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.                   --
--                                                                          --
-- More information about GWindows and the latest current release can       --
-- be located on the web at one of the following places:                    --
--   http://sf.net/projects/gnavi/                                          --
--   http://www.gnavi.org/gwindows                                          --
--   http://www.adapower.com/gwindows                                       --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Integer_Text_IO;
with Ada.Unchecked_Conversion;

with GWindows.GStrings; use GWindows.GStrings;
with GWindows.Types;

with System;

package body GWindows.Clipboard is

   procedure Set_Clipboard_Text_Unicode
      (Owner           : in Base.Base_Window_Type'Class;
       Text            : in Wide_String;
       Format_Position : in Copy_Format_Sequence);

   procedure Clipboard_Text
      (Owner           : in Base.Base_Window_Type'Class;
       Text            : in GString;
       Format_Position : in Copy_Format_Sequence := Single_Format)
   is
   begin
      case Character_Mode is
         when ANSI =>
            Set_Clipboard_Text (Owner, To_String (Text), Format_Position);
         when Unicode =>
            Set_Clipboard_Text_Unicode (Owner, To_Wide_String (Text), Format_Position);
      end case;
   end Clipboard_Text;

   procedure Clipboard_Text
      (Owner           : in Base.Base_Window_Type'Class;
       Text            : in GString_Unbounded;
       Format_Position : in Copy_Format_Sequence := Single_Format)
   is
   begin
     Clipboard_Text (Owner, To_GString_From_Unbounded (Text), Format_Position);
   end Clipboard_Text;

   function Get_Clipboard_Text_Unicode
      (Owner : in Base.Base_Window_Type'Class)
      return Wide_String;

   CF_TEXT        : constant :=  1;
   CF_UNICODETEXT : constant := 13;

   function Is_Clipboard_Format_Available
      (Format : in Natural)
      return Boolean;

   function Clipboard_Text
      (Owner : in Base.Base_Window_Type'Class)
      return GString
   is
   begin
      case Character_Mode is
         when ANSI =>
            return To_GString_From_String (Get_Clipboard_Text (Owner));
         when Unicode =>
            if Is_Clipboard_Format_Available (CF_UNICODETEXT) then
               return To_GString_From_Wide_String
                         (Get_Clipboard_Text_Unicode (Owner));
            else
               return To_GString_From_String (Get_Clipboard_Text (Owner));
            end if;
      end case;
   end Clipboard_Text;

   function Is_Clipboard_Text_Available
      return Boolean
   is
   begin
      case Character_Mode is
         when ANSI =>
           return Is_Clipboard_Format_Available (CF_TEXT);
         when Unicode =>
           return
             Is_Clipboard_Format_Available (CF_TEXT) or
             Is_Clipboard_Format_Available (CF_UNICODETEXT);
      end case;
   end Is_Clipboard_Text_Available;

   procedure Set_Clipboard_8_Bit_Chars
      (Owner           : in Base.Base_Window_Type'Class;
       Text            : in String;
       Format          : in Natural;
       Format_Position : in Copy_Format_Sequence);

   function Register_Clipboard_Format (Format_Name : String) return Natural;

   procedure Clipboard_HTML
      (Owner           : in Base.Base_Window_Type'Class;
       HTML            : in GString;
       Format_Position : in Copy_Format_Sequence := Single_Format)
   is
      CF_HTML : constant Natural := Register_Clipboard_Format ("HTML Format");
      Text : constant String := To_String (HTML);

      CR : constant Character := Character'Val (13);
      LF : constant Character := Character'Val (10);
      NL : constant String := CR & LF;

      function Img (X : Natural) return String is
         Res : String (1 .. 6);
      begin
         Ada.Integer_Text_IO.Put (Res, X);
         for I in Res'Range loop
           if Res (I) = ' ' then
             Res (I) := '0';
           end if;
         end loop;
         return Res;
      end Img;

      use Ada.Strings.Fixed;

      --  Now, things are of course not that simple, we need to wrap the HTML
      --  code into a wrapper.
      --
      --  https://learn.microsoft.com/fr-fr/windows/win32/dataxchg/html-clipboard-format

      Wrapped_1 : constant String :=
        "Version:0.9" & NL &
        "StartHTML:-1" & NL &
        "EndHTML:-1" & NL &
        "StartFragment:xxxxxx" & NL &
        "EndFragment:yyyyyy" & NL;

      start_index : constant Natural := Index (Wrapped_1, "xxxxxx");

      Wrapped_2 : constant String :=
        Replace_Slice (Wrapped_1, start_index, start_index + 5, Img (Wrapped_1'Length)) &
        Text & NL;

      end_index : constant Natural := Index (Wrapped_2, "yyyyyy");

      Wrapped_3 : constant String :=
        Replace_Slice (Wrapped_2, end_index, end_index + 5, Img (Wrapped_2'Length));

   begin
      if CF_HTML > 0 then
         Set_Clipboard_8_Bit_Chars
            (Owner           => Owner,
             Text            => Wrapped_3,
             Format          => CF_HTML,
             Format_Position => Format_Position);
      end if;
   end Clipboard_HTML;

   procedure Clipboard_HTML
      (Owner           : in Base.Base_Window_Type'Class;
       HTML            : in GString_Unbounded;
       Format_Position : in Copy_Format_Sequence := Single_Format)
   is
   begin
      Clipboard_HTML (Owner, To_GString_From_Unbounded (HTML), Format_Position);
   end Clipboard_HTML;

   ---------------------------------------------------
   --  Here is the low-level part of this package.  --
   ---------------------------------------------------

   function Register_Clipboard_Format (Format_Name : String) return Natural
   is

      S : String := Format_Name & Character'Val (0);

      function RegisterClipboardFormat_C
        (lpszFormat : System.Address   := S (S'First)'Address)
        return Natural;
      pragma Import (StdCall, RegisterClipboardFormat_C, "RegisterClipboardFormatA");

   begin
      return RegisterClipboardFormat_C;
   end Register_Clipboard_Format;

   subtype HGlobal is Interfaces.C.long;
   subtype LPVOID is GWindows.Types.Handle;

--   GHND           : constant := 16#0042#;
--   GMEM_FIXED     : constant := 16#0000#;
   GMEM_MOVEABLE  : constant := 16#0002#;
--   GMEM_ZEROPOINT : constant := 16#0040#;
--   GPTR           : constant := 16#0040#;

   --  GlobalLock gives a pointer to a piece of memory allocated
   --  outside the Ada run-time environment.
   --  This means the range of the type is not known.
   --  Connecting the pointer to a record with an image definition helps.
   --  Don't know the size on forhand, point to a block of large enough memory
   --  Never use Global_Alloc_Type directly, always use Global_Alloc_Ptr !!
   type Memory_Byte is mod 256;
   type Memory_Byte_Arr is array (Natural range <>) of Memory_Byte;
   type Global_Alloc_Type is
   record
      Data : Memory_Byte_Arr (0 .. Natural'Last);
   end record;
   type Global_Alloc_Ptr is access all Global_Alloc_Type;

   function Global_Alloc
      (Flags : Integer;
       Size  : Integer)
      return HGlobal
   is
      function GlobalAlloc
         (uFlags   : Integer := Flags;
          dwBytes  : Integer := Size)
         return Interfaces.C.long;
      pragma Import (StdCall, GlobalAlloc, "GlobalAlloc");

   begin
      return GlobalAlloc;
   end Global_Alloc;

   function Global_Lock
      (Data : in HGlobal)
      return Global_Alloc_Ptr
   is
      function To_Ptr is new
         Ada.Unchecked_Conversion (LPVOID, Global_Alloc_Ptr);

      function GlobalLock
         (hMem   : Interfaces.C.long := Data)
         return LPVOID;
      pragma Import (StdCall, GlobalLock, "GlobalLock");

   begin
      return To_Ptr (GlobalLock);
   end Global_Lock;

   function Global_Unlock
      (Data : in HGlobal)
      return Boolean
   is
      function GlobalUnlock
         (hMem   : Interfaces.C.long := Data)
         return Boolean;
      pragma Import (StdCall, GlobalUnlock, "GlobalUnlock");

   begin
      return GlobalUnlock;
   end Global_Unlock;

   function Open_Clipboard
      (Owner : in Base.Base_Window_Type'Class)
      return Boolean
   is
      function OpenClipboard
        (hWndNewOwner  : GWindows.Types.Handle := Owner.Handle)
        return Boolean;
      pragma Import (StdCall, OpenClipboard, "OpenClipboard");

   begin
      return OpenClipboard;
   end Open_Clipboard;

   function Close_Clipboard
      return Boolean
   is
      function CloseClipboard
        return Boolean;
      pragma Import (StdCall, CloseClipboard, "CloseClipboard");
   begin
      return CloseClipboard;
   end Close_Clipboard;

   function Get_Clipboard_Data
      (Format : in Natural)
      return HGlobal
   is
      function GetClipboardData
        (Frmt : Natural := Format)
        return HGlobal;
      pragma Import (StdCall, GetClipboardData, "GetClipboardData");

   begin
      return GetClipboardData;
   end Get_Clipboard_Data;

   function Set_Clipboard_Data
      (Format : in Natural;
       Data   : in Global_Alloc_Ptr)
      return Boolean
   is
      function To_Ptr is new
         Ada.Unchecked_Conversion (LPVOID, Global_Alloc_Ptr);
      function To_lpVoid is new
         Ada.Unchecked_Conversion (Global_Alloc_Ptr, LPVOID);

      function SetClipboardData
        (Frmt : Natural       := Format;
         Dat  : LPVOID  := To_lpVoid (Data))
        return LPVOID;
      pragma Import (StdCall, SetClipboardData, "SetClipboardData");
   begin
      return To_Ptr (SetClipboardData) /= null;
   end Set_Clipboard_Data;

   function Empty_Clipboard
      return Boolean
   is
      function EmptyClipboard
        return Boolean;
      pragma Import (StdCall, EmptyClipboard, "EmptyClipboard");
   begin
      return EmptyClipboard;
   end Empty_Clipboard;

   function Is_Clipboard_Format_Available
      (Format : in Natural)
      return Boolean
   is
      function IsClipboardFormatAvailable
        (Frmt : Natural := Format)
        return Boolean;
      pragma Import (StdCall, IsClipboardFormatAvailable,
         "IsClipboardFormatAvailable");
   begin
      return IsClipboardFormatAvailable;
   end Is_Clipboard_Format_Available;

   --------------------------------------------
   --  Now, the really serious stuff...      --
   --                                        --
   --  1) Set / Get with 8-bit ANSI strings  --
   --------------------------------------------

   procedure Set_Clipboard_8_Bit_Chars
      (Owner           : in Base.Base_Window_Type'Class;
       Text            : in String;
       Format          : in Natural;
       Format_Position : in Copy_Format_Sequence)
   is
      function To_Byte is new
         Ada.Unchecked_Conversion (Character, Memory_Byte);

      Dmp  : Boolean;
      pragma Unreferenced (Dmp);
      Mem  : HGlobal;
      Data : Global_Alloc_Ptr;
      Idx  : Natural := 0;
   begin
      if Format_Position in Single_Format | First_Format then
         Dmp := Open_Clipboard (Owner);
      end if;
      Mem := Global_Alloc (GMEM_MOVEABLE, Text'Length + 1);
      Data := Global_Lock (Mem);
      for I in Text'Range loop
         Data.Data (Idx) := To_Byte (Text (I));
         Idx := Idx + 1;
      end loop;
      Data.Data (Idx) := 0;
      Dmp := Global_Unlock (Mem);
      if Format_Position in Single_Format | First_Format then
         Dmp := Empty_Clipboard;
      end if;
      Dmp := Set_Clipboard_Data (Format, Data);
      if Format_Position in Single_Format | Last_Format then
         Dmp := Close_Clipboard;
      end if;
   end Set_Clipboard_8_Bit_Chars;

   procedure Set_Clipboard_Text
      (Owner           : in Base.Base_Window_Type'Class;
       Text            : in String;
       Format_Position : in Copy_Format_Sequence := Single_Format)
   is
   begin
      Set_Clipboard_8_Bit_Chars
         (Owner           => Owner,
          Text            => Text,
          Format          => CF_TEXT,
          Format_Position => Format_Position);
   end Set_Clipboard_Text;

   procedure Set_Clipboard_Text
      (Owner           : in Base.Base_Window_Type'Class;
       Text            : in Ada.Strings.Unbounded.Unbounded_String;
       Format_Position : in Copy_Format_Sequence := Single_Format)
   is
   begin
      Set_Clipboard_Text
         (Owner           => Owner,
          Text            => Ada.Strings.Unbounded.To_String (Text),
          Format_Position => Format_Position);
   end Set_Clipboard_Text;

   function Get_Clipboard_Text
      (Owner : in Base.Base_Window_Type'Class)
      return String
   is
      Dmp  : Boolean;
      pragma Unreferenced (Dmp);
      Len  : Integer := 0;
      Mem  : HGlobal;
      Data : Global_Alloc_Ptr;
   begin
      if Is_Clipboard_Format_Available (CF_TEXT) then
         Dmp := Open_Clipboard (Owner);
         Mem := Get_Clipboard_Data (CF_TEXT);
         Data := Global_Lock (Mem);
         --  text is C style, thus ending with 0
         while Data.Data (Len) /= 0 loop
            Len := Len + 1;
         end loop;
         declare
            function To_Char is new
               Ada.Unchecked_Conversion (Memory_Byte, Character);
            Txt : String (1 .. Len);
         begin
            for I in Txt'Range loop
               Txt (I) := To_Char (Data.Data (I - 1));
            end loop;
            Dmp := Global_Unlock (Mem);
            Dmp := Close_Clipboard;
            return Txt;
         end;
      end if;
      return "";
   end Get_Clipboard_Text;

   function Is_Clipboard_Text
      return Boolean
   is
   begin
      return Is_Clipboard_Format_Available (CF_TEXT);
   end Is_Clipboard_Text;

   ------------------------------------------------
   --  2) Set / Get with 16-bit UNICODE strings  --
   ------------------------------------------------

   subtype Byte_Pair is Memory_Byte_Arr (0 .. 1);

   procedure Set_Clipboard_Text_Unicode
      (Owner           : in Base.Base_Window_Type'Class;
       Text            : in Wide_String;
       Format_Position : in Copy_Format_Sequence)
   is
      function To_Byte_Pair is new
         Ada.Unchecked_Conversion (Wide_Character, Byte_Pair);

      Dmp  : Boolean;
      pragma Unreferenced (Dmp);
      Mem  : HGlobal;
      Data : Global_Alloc_Ptr;
      Idx  : Natural := 0;
   begin
      if Format_Position in Single_Format | First_Format then
         Dmp := Open_Clipboard (Owner);
      end if;
      Mem := Global_Alloc (GMEM_MOVEABLE, Text'Length * 2 + 2);
      Data := Global_Lock (Mem);
      for I in Text'Range loop
         Data.Data (Idx .. Idx + 1) := To_Byte_Pair (Text (I));
         Idx := Idx + 2;
      end loop;
      Data.Data (Idx .. Idx + 1) := (0, 0);
      Dmp := Global_Unlock (Mem);
      if Format_Position in Single_Format | First_Format then
         Dmp := Empty_Clipboard;
      end if;
      Dmp := Set_Clipboard_Data (CF_UNICODETEXT, Data);
      if Format_Position in Single_Format | Last_Format then
         Dmp := Close_Clipboard;
      end if;
   end Set_Clipboard_Text_Unicode;

   function Get_Clipboard_Text_Unicode
      (Owner : in Base.Base_Window_Type'Class)
      return Wide_String
   is
      Dmp  : Boolean;
      pragma Unreferenced (Dmp);
      Len  : Integer := 0;
      Mem  : HGlobal;
      Data : Global_Alloc_Ptr;
   begin
      if Is_Clipboard_Format_Available (CF_UNICODETEXT) then
         Dmp := Open_Clipboard (Owner);
         Mem := Get_Clipboard_Data (CF_UNICODETEXT);
         Data := Global_Lock (Mem);
         --  text is C style, thus ending with 0
         while Data.Data (Len .. Len + 1) /= (0, 0) loop
            Len := Len + 2;
         end loop;
         declare
            function To_Wide_Char is new
               Ada.Unchecked_Conversion (Byte_Pair, Wide_Character);
            Txt : Wide_String (1 .. Len / 2);
            J : Natural;
         begin
            for I in Txt'Range loop
               J := (I - 1) * 2;
               Txt (I) := To_Wide_Char (Data.Data (J .. J + 1));
            end loop;
            Dmp := Global_Unlock (Mem);
            Dmp := Close_Clipboard;
            return Txt;
         end;
      end if;
      return "";
   end Get_Clipboard_Text_Unicode;

end GWindows.Clipboard;
