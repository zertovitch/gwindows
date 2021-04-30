package body Windows_pipes is

   type PHANDLE is access all HANDLE;
   function CreatePipe (hReadPipe : PHANDLE;
                        hWritePipe : PHANDLE;
                        lpPipeAttributes : LPSECURITY_ATTRIBUTES;
                        nSize : DWORD)
                       return BOOL;
   pragma Import (Stdcall, CreatePipe, "CreatePipe");

   function CreateProcessA (lpApplicationName : LPCSTR;
                            lpCommandLine : LPSTR;
                            lpProcessAttributes : LPSECURITY_ATTRIBUTES;
                            lpThreadAttributes : LPSECURITY_ATTRIBUTES;
                            bInheritHandles : BOOL;
                            dwCreationFlags : DWORD;
                            lpEnvironment : LPVOID;
                            lpCurrentDirectory : LPCSTR;
                            lpStartupInfo : LPSTARTUPINFOA;
                            lpProcessInformation : LPPROCESS_INFORMATION)
                           return BOOL;
   pragma Import (Stdcall, CreateProcessA, "CreateProcessA");
   function CreateProcess (lpApplicationName : LPCSTR;
                           lpCommandLine : LPSTR;
                           lpProcessAttributes : LPSECURITY_ATTRIBUTES;
                           lpThreadAttributes : LPSECURITY_ATTRIBUTES;
                           bInheritHandles : BOOL;
                           dwCreationFlags : DWORD;
                           lpEnvironment : LPVOID;
                           lpCurrentDirectory : LPCSTR;
                           lpStartupInfo : LPSTARTUPINFOA;
                           lpProcessInformation : LPPROCESS_INFORMATION)
                          return BOOL
     renames CreateProcessA;

   function CloseHandle (hObject : HANDLE) return BOOL;
   pragma Import (Stdcall, CloseHandle, "CloseHandle");

   function TerminateProcess (hProcess : HANDLE;
                              uExitCode : UINT)
                             return BOOL;
   pragma Import (Stdcall, TerminateProcess, "TerminateProcess");

   function GetExitCodeProcess (hProcess : HANDLE;
                                lpExitCode : LPDWORD)
                               return BOOL;
   pragma Import (Stdcall, GetExitCodeProcess, "GetExitCodeProcess");

   STATUS_PENDING : constant DWORD    :=  16#103#;
   STILL_ACTIVE_W : constant DWORD := STATUS_PENDING;

   function PeekNamedPipe (hNamedPipe : HANDLE;
                           lpBuffer : LPVOID;
                           nBufferSize : DWORD;
                           lpBytesRead : LPDWORD;
                           lpTotalBytesAvail : LPDWORD;
                           lpBytesLeftThisMessage : LPDWORD)
                          return BOOL;
   pragma Import (Stdcall, PeekNamedPipe, "PeekNamedPipe");

   type OVERLAPPED is
      record
         Internal : DWORD;
         InternalHigh : DWORD;
         Offset : DWORD;
         OffsetHigh : DWORD;
         hEvent : HANDLE;
      end record;

   type LPOVERLAPPED is access all OVERLAPPED;

   function ReadFile (hFile : HANDLE;
                      lpBuffer : LPVOID;
                      nNumberOfBytesToRead : DWORD;
                      lpNumberOfBytesRead : LPDWORD;
                      lpOverlappez : LPOVERLAPPED)
                     return BOOL;
   pragma Import (Stdcall, ReadFile, "ReadFile");

  -----------
  -- Start --
  -----------

  procedure Start(
    p            : in out Piped_process;
    command, path: String;
    text_output  : Output_Line
  )
   is
    Created       : BOOL;
    IgnoreBool    : BOOL;
    pragma Unreferenced (IgnoreBool);
    IgnoreLResult : LRESULT;
    pragma Unmodified (IgnoreLResult);
    --
    Process_Buffer : array (command'First..command'Last+1) of aliased CHAR;
    Start_Path     : array (path'First..path'Last+1) of aliased CHAR;
    --
    Startf_UseShowWindow : constant :=   16#1#;
    Startf_UseStdHandles : constant :=   16#100#;
    Startf_flags: constant:= Startf_UseShowWindow + Startf_UseStdHandles;
    SW_HIDE : constant := 0;
    use System;
  begin
    if text_output = null then
      raise Output_is_null;
    end if;
    p.text_output := text_output;
    p.SA.nLength := DWORD(p.SA'Size/8);
    p.SA.lpSecurityDescriptor := System.Null_Address;
    p.SA.bInheritHandle := 1; -- BOOL(TRUE)
    Created :=
      CreatePipe(
        p.PipeRead'Unrestricted_Access,
        p.PipeWrite'Unrestricted_Access,
        p.SA'Unrestricted_Access,
        1024
      );
    if Created = 0 then
      raise Cannot_create_pipe;
    end if;
    --
    p.SI.cb := DWORD(p.SI'Size/8);
    p.SI.lpReserved := null;
    p.SI.lpDesktop := null;
    p.SI.lpTitle := null;
    p.SI.dwFlags := Startf_flags;
    p.SI.wShowWindow := SW_HIDE;
    p.SI.cbReserved2 := 0;
    p.SI.lpReserved2 := null;
    p.SI.hStdInput  := System.Null_Address;
    p.SI.hStdOutput := p.PipeWrite;
    p.SI.hStdError  := p.PipeWrite;
    --  Copy command
    for i in command'Range loop
      Process_Buffer(i) := CHAR(command(i));
    end loop;
    Process_Buffer(Process_Buffer'Last) := CHAR'First;
    --  Copy path
    for i in path'Range loop
      Start_Path(i) := CHAR(path(i));
    end loop;
    Start_Path(Start_Path'Last) := CHAR'First;
    --
    --  http://msdn.microsoft.com/en-us/library/ms682425(VS.85).aspx
    --
    Created :=
      CreateProcess(
        null,
        Process_Buffer(Process_Buffer'First)'Unchecked_Access,
        null,
        null,
        BOOL(1),  -- inherit handles
        DWORD(0), -- flags
        System.Null_Address,
        Start_Path(Start_Path'First)'Unrestricted_Access,
        p.SI'Unrestricted_Access,
        p.PI'Unrestricted_Access
      );

    if Created = 0 or p.PI.hProcess = System.Null_Address then
      raise Cannot_start;
    end if;

    p.ProcessObject := p.PI.hProcess;
    IgnoreBool := CloseHandle(p.PipeWrite);
    p.part_of_line:= Ada.Strings.Unbounded.Null_Unbounded_String;
  end Start;

  ----------
  -- Stop --
  ----------

  procedure Stop (p: in out Piped_process) is
    IgnoreBool : BOOL;
    pragma Unreferenced (IgnoreBool);
    use System;
  begin
    if Alive(p)  then
      IgnoreBool := TerminateProcess(p.ProcessObject,0);
      p.ProcessObject := System.Null_Address;
    end if;
  end Stop;

  --  Internal
  --
  procedure Read_pipe (p: in out Piped_process) is
    NumRead : aliased DWORD;
    StuffInPipe   : BOOL;
    HowManyBytes  : aliased DWORD;
    IgnoreBool    : BOOL;
    pragma Unreferenced (IgnoreBool);
    use Interfaces.C, Ada.Strings.Unbounded;
  begin
    StuffInPipe :=
      PeekNamedPipe(
        p.PipeRead,
        System.Null_Address, 0, -- don't actually read anything
        null, -- don't care
        HowManyBytes'Unchecked_Access,
        null
      );
    if StuffInPipe = 0 or HowManyBytes = 0 then
      return;
    end if;
    declare
      Buffer : array(1..Integer(HowManyBytes)) of aliased CHAR;
      mark: Integer:= 1;
      --
      procedure Memorize_chunk(a,b: Integer) is
        s: String(a..b);
      begin
        for i in s'Range loop
          s(i):= Character(Buffer(i));
        end loop;
        p.part_of_line:= p.part_of_line & To_Unbounded_String(s);
      end Memorize_chunk;
      --
      procedure Spit_a_line(a,b: Integer) is
        s: String(a..b);
      begin
        for i in s'Range loop
          s(i):= Character(Buffer(i));
        end loop;
        p.text_output(To_String(p.part_of_line) & s);
        p.part_of_line:= Ada.Strings.Unbounded.Null_Unbounded_String;
      end Spit_a_line;
      --
    begin
      IgnoreBool:= ReadFile(
         p.PipeRead,
         Buffer(1)'Address,
         HowManyBytes,
         NumRead'Unrestricted_Access,
         null
       );
       for i in 1..Integer(NumRead) loop
         case Character(Buffer(i)) is
           when ASCII.CR =>
             Memorize_chunk(mark,i-1);
             mark:= i+1;
           when ASCII.LF =>
             Spit_a_line(mark,i-1);
             mark:= i+1;
           when others =>
             null;
         end case;
       end loop;
       --  We may have still an incomplete line:
       Memorize_chunk(mark, Integer(NumRead));
    end;

  end Read_pipe;

  --------------------
  -- Check_progress --
  --------------------

  procedure Check_progress (p: in out Piped_process) is
    func_exit_code_result : BOOL;
    DwExitCode : aliased DWORD;
    TempProcessObject: HANDLE;
    use System, Interfaces.C;
  begin
    if not Alive(p) then
      return;
    end if;
    --  http://msdn.microsoft.com/en-us/library/ms683189(VS.85).aspx
    func_exit_code_result :=
      GetExitCodeProcess(p.ProcessObject, DwExitCode'Unchecked_Access);
    --
    if func_exit_code_result = 0 then -- should never happen
      p.ProcessObject := System.Null_Address;
      p.exit_code:= 666;
      return;
    end if;
    if DwExitCode /= STILL_ACTIVE_W then -- died
      p.ProcessObject := System.Null_Address;
      p.exit_code:= Integer(DwExitCode);
      return;
    end if;
    --
    --  don't want to have two running at same time somehow
    --  so we clear ProcessObject
    TempProcessObject := p.ProcessObject;
    p.ProcessObject   := System.Null_Address;
    Read_pipe(p);
    --  restore value of ProcessObject
    p.ProcessObject := TempProcessObject;

  end Check_progress;

  -----------
  -- Alive --
  -----------

  function Alive(p: Piped_process) return Boolean is
    use System;
  begin
    if p.ProcessObject = System.Null_Address then
      return False;
    end if;
    return True;
  end Alive;

  function Last_exit_code(p: Piped_process) return Integer is
  begin
    if Alive(p) then
      raise Still_active;
    end if;
    return p.exit_code;
  end Last_exit_code;

end Windows_pipes;
