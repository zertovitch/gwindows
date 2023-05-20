------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada Framework for Windows Development             --
--                                                                          --
--                       G W I N D O W S . P I P E S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--              Copyright (C) 2010 - 2023 Gautier de Montmollin             --
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
--   https://github.com/zertovitch/gwindows                                 --
--                                                                          --
------------------------------------------------------------------------------
--
--  A test follows here in comment:
--
--  with Ada.Text_IO;
--  procedure Output_a_line (l : String) is
--  begin
--    Ada.Text_IO.Put_Line ('[' & l & ']');
--  end;
--
--  with Output_a_line, GWindows.Pipes;
--
--  procedure Win_Pipe_Test is
--    use Windows_Pipes;
--    p : Piped_Process;
--  begin
--    Start (p, "cmd.exe /c dir", ".", Output_a_line'Access);
--    while Is_Alive (p) loop
--      Check_Progress (p);
--    end loop;
--  end Win_Pipe_Test;

with Ada.Strings.Unbounded;
with Interfaces.C, System;

package GWindows.Pipes is

   --  Output a line to anywhere: a terminal (Text_IO), a message box,...
   type Output_Line is access procedure (l : String);

   Cannot_Create_Pipe : exception;
   Cannot_Start       : exception;
   Output_Is_Null     : exception;  --  output callback is null
   Still_Active       : exception;  --  raised if Last_Exit_Code is queried too early

   type Piped_Process is private;

   procedure Start
     (p             : in out Piped_Process;
      command, path :        String;
      text_output   :        Output_Line);

   procedure Stop (p : in out Piped_Process);

   procedure Check_Progress (p : in out Piped_Process);

   function Is_Alive (p : Piped_Process) return Boolean;

   function Last_Exit_Code (p : Piped_Process) return Integer;

private

   subtype PVOID   is System.Address;
   subtype LPVOID  is PVOID;
   type    BYTE       is new Interfaces.C.unsigned_char;
   type    PBYTE      is access all BYTE;
   subtype LPBYTE     is PBYTE;
   subtype CHAR       is Interfaces.C.char;
   type    PCHAR      is access all CHAR;
   subtype LPSTR      is PCHAR;
   type    PCCH       is access constant CHAR;
   subtype PCCHAR     is PCCH;
   subtype LPCCH      is PCCH;
   subtype LPCSTR     is PCCH;
   subtype USHORT     is Interfaces.C.unsigned_short;
   subtype WORD       is USHORT;
   subtype UINT       is Interfaces.C.unsigned;
   subtype LONG       is Interfaces.C.long;
   subtype LPARAM     is LONG;
   subtype LRESULT    is LONG;
   subtype ULONG       is Interfaces.C.unsigned_long;
   subtype DWORD       is ULONG;
   type    PULONG      is access all ULONG;
   subtype PDWORD      is PULONG;
   subtype LPDWORD     is PDWORD;
   subtype HANDLE is PVOID;
   subtype INT       is Interfaces.C.int;
   type    BOOL      is new INT;

   type STARTUPINFOA is
      record
         cb : DWORD;
         lpReserved : LPSTR;
         lpDesktop : LPSTR;
         lpTitle : LPSTR;
         dwX : DWORD;
         dwY : DWORD;
         dwXSize : DWORD;
         dwYSize : DWORD;
         dwXCountChars : DWORD;
         dwYCountChars : DWORD;
         dwFillAttribute : DWORD;
         dwFlags : DWORD;
         wShowWindow : WORD;
         cbReserved2 : WORD;
         lpReserved2 : LPBYTE;
         hStdInput  : HANDLE;
         hStdOutput : HANDLE;
         hStdError  : HANDLE;
      end record;
   subtype StartupInfo is STARTUPINFOA;

   type LPSTARTUPINFOA is access all STARTUPINFOA;

   type Process_Information is
      record
         hProcess : HANDLE;
         hThread  : HANDLE;
         dwProcessId : DWORD;
         dwThreadId  : DWORD;
      end record;

   type PPROCESS_INFORMATION is access all Process_Information;
   subtype LPPROCESS_INFORMATION is PPROCESS_INFORMATION;

   type Security_Attributes is
      record
         nLength : DWORD;
         lpSecurityDescriptor : LPVOID;
         bInheritHandle : BOOL;
      end record;

   type PSECURITY_ATTRIBUTES is access all Security_Attributes;
   subtype LPSECURITY_ATTRIBUTES is PSECURITY_ATTRIBUTES;

   type Piped_Process is record
     SI : aliased StartupInfo;
     PI : aliased Process_Information;
     SA : aliased Security_Attributes;
     PipeRead, PipeWrite : aliased HANDLE;
     ProcessObject : HANDLE := System.Null_Address; -- = null <=> inactive
     part_of_line : Ada.Strings.Unbounded.Unbounded_String;
     text_output : Output_Line;
     exit_code   : Integer;
   end record;

end GWindows.Pipes;
