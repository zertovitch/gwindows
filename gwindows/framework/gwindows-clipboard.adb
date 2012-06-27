------------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                  G W I N D O W S . C L I P B O A R D                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
-- This is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. It is distributed in the hope that it will be useful,  but WITHOUT --
-- ANY WARRANTY;  without  even the  implied warranty of MERCHANTABILITY or --
-- FITNESS FOR A PARTICULAR PURPOSE.    See the GNU General  Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with this;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- More information about GWindows and the latest current release can       --
-- be located on the web at one of the following places:                    --
--   http://sf.net/projects/gnavi/                                          --
--   http://www.gnavi.org/gwindows                                          --
--   http://www.adapower.com/gwindows                                       --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

with GWindows.Base;
with GWindows.Types;

package body GWindows.Clipboard is

   subtype HGlobal is Interfaces.C.long;
   subtype LPVOID is GWindows.Types.Handle;

   CF_TEXT        : constant :=  1;
--   CF_OEMTEXT     : constant :=  7;
--   CF_UNICODETEXT : constant := 13;

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
      (Owner : in GWindows.Windows.Window_Type)
      return Boolean
   is
      function OpenClipboard
        (hWndNewOwner  : GWindows.Types.Handle :=
         GWindows.Base.Handle (GWindows.Base.Base_Window_Type (Owner)))
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

--  http://msdn.microsoft.com/library/default.asp?
---   url=/library/en-us/winui/winui/windowsuserinterface/dataexchange/
--    clipboard/usingtheclipboard.asp

--  Extra functions
   procedure Set_Clipboard_Text
      (Owner : in GWindows.Windows.Window_Type;
       Text  : in String)
   is
      function To_Byte is new
         Ada.Unchecked_Conversion (Character, Memory_Byte);

      Dmp  : Boolean;
      pragma Unreferenced (Dmp);
      Mem  : HGlobal;
      Data : Global_Alloc_Ptr;
      Idx  : Natural := 0;
   begin
      Dmp := Open_Clipboard (Owner);
      Mem := Global_Alloc (GMEM_MOVEABLE, Text'Length + 1);
      Data := Global_Lock (Mem);
      for I in Text'Range loop
         Data.Data (Idx) := To_Byte (Text (I));
         Idx := Idx + 1;
      end loop;
      Data.Data (Idx) := 0;
      Dmp := Global_Unlock (Mem);
      Dmp := Empty_Clipboard;
      Dmp := Set_Clipboard_Data (CF_TEXT, Data);
      Dmp := Close_Clipboard;
   end Set_Clipboard_Text;

   procedure Set_Clipboard_Text
      (Owner : in GWindows.Windows.Window_Type;
       Text  : in Unbounded_String)
   is
   begin
      Set_Clipboard_Text (Owner => Owner, Text => To_String (Text));
   end Set_Clipboard_Text;

   function Get_Clipboard_Text
      (Owner : in GWindows.Windows.Window_Type)
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

end GWindows.Clipboard;
