------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada Framework for Windows Development             --
--                                                                          --
--                       G W I N D O W S . P I P E S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--              Copyright (C) 2010 - 2026 Gautier de Montmollin             --
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
--   https://github.com/zertovitch/gwindows                                 --
--                                                                          --
------------------------------------------------------------------------------

package body GWindows.Pipes is

   type PHANDLE is access all HANDLE;

   function Create_Pipe
     (hReadPipe        : PHANDLE;
      hWritePipe       : PHANDLE;
      lpPipeAttributes : LPSECURITY_ATTRIBUTES;
      nSize            : DWORD)
   return BOOL;

   pragma Import (Stdcall, Create_Pipe, "CreatePipe");

   function CreateProcessA
     (lpApplicationName    : LPCSTR;
      lpCommandLine        : LPSTR;
      lpProcessAttributes  : LPSECURITY_ATTRIBUTES;
      lpThreadAttributes   : LPSECURITY_ATTRIBUTES;
      bInheritHandles      : BOOL;
      dwCreationFlags      : DWORD;
      lpEnvironment        : LPVOID;
      lpCurrentDirectory   : LPCSTR;
      lpStartupInfo        : LPSTARTUPINFOA;
      lpProcessInformation : LPPROCESS_INFORMATION)
   return BOOL;

   pragma Import (Stdcall, CreateProcessA, "CreateProcessA");

   function Create_Process
     (lpApplicationName    : LPCSTR;
      lpCommandLine        : LPSTR;
      lpProcessAttributes  : LPSECURITY_ATTRIBUTES;
      lpThreadAttributes   : LPSECURITY_ATTRIBUTES;
      bInheritHandles      : BOOL;
      dwCreationFlags      : DWORD;
      lpEnvironment        : LPVOID;
      lpCurrentDirectory   : LPCSTR;
      lpStartupInfo        : LPSTARTUPINFOA;
      lpProcessInformation : LPPROCESS_INFORMATION)
   return BOOL
   renames CreateProcessA;

   function CloseHandle (hObject : HANDLE) return BOOL;

   pragma Import (Stdcall, CloseHandle, "CloseHandle");

   function Terminate_Process
     (hProcess  : HANDLE;
      uExitCode : UINT)
   return BOOL;

   pragma Import (Stdcall, Terminate_Process, "TerminateProcess");

   function Get_Exit_Code_Process
     (hProcess : HANDLE;
      lpExitCode : LPDWORD)
   return BOOL;

   pragma Import (Stdcall, Get_Exit_Code_Process, "GetExitCodeProcess");

   STATUS_PENDING : constant := 16#103#;
   STILL_ACTIVE_W : constant := STATUS_PENDING;

   function Peek_Named_Pipe
     (hNamedPipe             : HANDLE;
      lpBuffer               : LPVOID;
      nBufferSize            : DWORD;
      lpBytesRead            : LPDWORD;
      lpTotalBytesAvail      : LPDWORD;
      lpBytesLeftThisMessage : LPDWORD)
   return BOOL;

   pragma Import (Stdcall, Peek_Named_Pipe, "PeekNamedPipe");

   type OVERLAPPED is
      record
         Internal     : DWORD;
         InternalHigh : DWORD;
         Offset       : DWORD;
         OffsetHigh   : DWORD;
         hEvent       : HANDLE;
      end record;

   type LPOVERLAPPED is access all OVERLAPPED;

   function ReadFile
     (hFile                : HANDLE;
      lpBuffer             : LPVOID;
      nNumberOfBytesToRead : DWORD;
      lpNumberOfBytesRead  : LPDWORD;
      lpOverlappez         : LPOVERLAPPED)
   return BOOL;

   pragma Import (Stdcall, ReadFile, "ReadFile");

  -----------
  -- Start --
  -----------

  procedure Start
     (p             : in out Piped_Process;
      command, path :        String;
      text_output   :        Output_Line)
  is
    Created       : BOOL;
    IgnoreBool    : BOOL;
    pragma Unreferenced (IgnoreBool);
    IgnoreLResult : LRESULT;
    pragma Unmodified (IgnoreLResult);
    --
    Process_Buffer : array (command'First .. command'Last + 1) of aliased CHAR;
    Start_Path     : array (path'First .. path'Last + 1) of aliased CHAR;
    --
    Startf_UseShowWindow : constant :=   16#1#;
    Startf_UseStdHandles : constant :=   16#100#;
    Startf_flags : constant := Startf_UseShowWindow + Startf_UseStdHandles;
    SW_HIDE : constant := 0;
    use System;
  begin
    if text_output = null then
      raise Output_Is_Null;
    end if;
    p.text_output := text_output;
    p.SA.nLength := DWORD (p.SA'Size / 8);
    p.SA.lpSecurityDescriptor := System.Null_Address;
    p.SA.bInheritHandle := 1; -- BOOL(TRUE)

    Created :=
      Create_Pipe
        (p.PipeRead'Unrestricted_Access,
         p.PipeWrite'Unrestricted_Access,
         p.SA'Unrestricted_Access,
         1024);

    if Created = 0 then
      raise Cannot_Create_Pipe;
    end if;

    p.SI.cb          := DWORD (p.SI'Size / 8);
    p.SI.lpReserved  := null;
    p.SI.lpDesktop   := null;
    p.SI.lpTitle     := null;
    p.SI.dwFlags     := Startf_flags;
    p.SI.wShowWindow := SW_HIDE;
    p.SI.cbReserved2 := 0;
    p.SI.lpReserved2 := null;
    p.SI.hStdInput   := System.Null_Address;
    p.SI.hStdOutput  := p.PipeWrite;
    p.SI.hStdError   := p.PipeWrite;

    --  Copy command
    for i in command'Range loop
      Process_Buffer (i) := CHAR (command (i));
    end loop;
    Process_Buffer (Process_Buffer'Last) := CHAR'First;

    --  Copy path
    for i in path'Range loop
      Start_Path (i) := CHAR (path (i));
    end loop;
    Start_Path (Start_Path'Last) := CHAR'First;

    --
    --  http://msdn.microsoft.com/en-us/library/ms682425(VS.85).aspx
    --
    Created :=
      Create_Process
        (null,
         Process_Buffer (Process_Buffer'First)'Unchecked_Access,
         null,
         null,
         BOOL (1),  -- inherit handles
         DWORD (0), -- flags
         System.Null_Address,
         Start_Path (Start_Path'First)'Unrestricted_Access,
         p.SI'Unrestricted_Access,
         p.PI'Unrestricted_Access);

    if Created = 0 or p.PI.hProcess = System.Null_Address then
      raise Cannot_Start;
    end if;

    p.ProcessObject := p.PI.hProcess;
    IgnoreBool := CloseHandle (p.PipeWrite);
    p.part_of_line := Ada.Strings.Unbounded.Null_Unbounded_String;

  end Start;

  ----------
  -- Stop --
  ----------

  procedure Stop (p : in out Piped_Process) is
    IgnoreBool : BOOL;
    pragma Unreferenced (IgnoreBool);
    use System;
  begin
    if Is_Alive (p)  then
      IgnoreBool := Terminate_Process (p.ProcessObject, 0);
      p.ProcessObject := System.Null_Address;
    end if;
  end Stop;

  --  Internal
  --
  procedure Read_Pipe (p : in out Piped_Process) is
    num_read       : aliased DWORD;
    stuff_in_pipe  : BOOL;
    how_many_bytes : aliased DWORD;
    ignore_bool    : BOOL;
    pragma Unreferenced (ignore_bool);

    use Interfaces.C, Ada.Strings.Unbounded;

  begin
    stuff_in_pipe :=
      Peek_Named_Pipe
        (p.PipeRead,
         System.Null_Address,
         0,     --  don't actually read anything
         null,  --  don't care
         how_many_bytes'Unchecked_Access,
         null);

    if stuff_in_pipe = 0 or how_many_bytes = 0 then
      return;
    end if;

    declare
      buffer : array (1 .. Integer (how_many_bytes)) of aliased CHAR;
      mark : Integer := 1;

      procedure Memorize_Chunk (a, b : Integer) is
        s : String (a .. b);
      begin
        for i in s'Range loop
          s (i) := Character (buffer (i));
        end loop;
        p.part_of_line := p.part_of_line & To_Unbounded_String (s);
      end Memorize_Chunk;

      procedure Spit_a_Line (a, b : Integer) is
        s : String (a .. b);
      begin
        for i in s'Range loop
          s (i) := Character (buffer (i));
        end loop;
        p.text_output (To_String (p.part_of_line) & s);
        p.part_of_line := Ada.Strings.Unbounded.Null_Unbounded_String;
      end Spit_a_Line;

    begin
      ignore_bool :=
        ReadFile
          (p.PipeRead,
           buffer (1)'Address,
           how_many_bytes,
           num_read'Unrestricted_Access,
           null);

       for i in 1 .. Integer (num_read) loop
         case Character (buffer (i)) is
           when ASCII.CR =>
             Memorize_Chunk (mark, i - 1);
             mark := i + 1;
           when ASCII.LF =>
             Spit_a_Line (mark, i - 1);
             mark := i + 1;
           when others =>
             null;
         end case;
       end loop;

       --  We may have still an incomplete line:
       Memorize_Chunk (mark, Integer (num_read));
    end;

  end Read_Pipe;

  --------------------
  -- Check_Progress --
  --------------------

  procedure Check_Progress (p : in out Piped_Process) is
    func_exit_code_result : BOOL;
    DwExitCode : aliased DWORD;
    TempProcessObject : HANDLE;
    use System, Interfaces.C;
  begin
    if not Is_Alive (p) then
      return;
    end if;
    --  http://msdn.microsoft.com/en-us/library/ms683189(VS.85).aspx
    func_exit_code_result :=
      Get_Exit_Code_Process (p.ProcessObject, DwExitCode'Unchecked_Access);
    --
    if func_exit_code_result = 0 then  --  should never happen
      p.ProcessObject := System.Null_Address;
      p.exit_code := 666;
      return;
    end if;
    if DwExitCode /= STILL_ACTIVE_W then  --  died
      p.ProcessObject := System.Null_Address;
      p.exit_code := Integer (DwExitCode);
      return;
    end if;
    --
    --  don't want to have two running at same time somehow
    --  so we clear ProcessObject
    TempProcessObject := p.ProcessObject;
    p.ProcessObject   := System.Null_Address;
    Read_Pipe (p);
    --  restore value of ProcessObject
    p.ProcessObject := TempProcessObject;

  end Check_Progress;

  --------------
  -- Is_Alive --
  --------------

  function Is_Alive (p : Piped_Process) return Boolean is
    use System;
  begin
    if p.ProcessObject = System.Null_Address then
      return False;
    end if;
    return True;
  end Is_Alive;

  --------------------
  -- Last_Exit_Code --
  --------------------

  function Last_Exit_Code (p : Piped_Process) return Integer is
  begin
    if Is_Alive (p) then
      raise Still_Active;
    end if;
    return p.exit_code;
  end Last_Exit_Code;

end GWindows.Pipes;
