--  Windows_pipes
--  G. de Montmollin, 13-Feb-2010
--
--  Test procedure here in comment:
--
--  with Ada.Text_IO, Windows_pipes;
--  procedure Win_Pipe_test is
--    procedure Output_a_line(l: String) is
--    begin
--      Ada.Text_IO.Put_Line('[' & l & ']');
--    end;
--    package WP is new Windows_pipes(Output_a_line);
--    p: WP.Piped_process;
--  begin
--    WP.Start(p, "cmd.exe /c dir", ".");
--    while WP.Alive(p) loop
--      WP.Check_progress(p);
--    end loop;
--  end Win_Pipe_test;

with Ada.Strings.Unbounded;
with Interfaces.C, System;

generic
  -- Output a line to anywhere: a terminal (Text_IO), a message box,...
  with procedure Output_Line(l: String);

package Windows_pipes is

   Cannot_create_pipe: exception;
   Cannot_start: exception;

   type Piped_process is private;

   procedure Start(p: in out Piped_process; command, path: String);
   procedure Stop(p: in out Piped_process);
   procedure Check_progress(p: in out Piped_process);
   function Alive(p: Piped_process) return Boolean;

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
   subtype STARTUPINFO is STARTUPINFOA;

   type LPSTARTUPINFOA is access all STARTUPINFOA;

   type PROCESS_INFORMATION is
      record
         hProcess : HANDLE;
         hThread  : HANDLE;
         dwProcessId : DWORD;
         dwThreadId  : DWORD;
      end record;

   type PPROCESS_INFORMATION is access all PROCESS_INFORMATION;
   subtype LPPROCESS_INFORMATION is PPROCESS_INFORMATION;

   type SECURITY_ATTRIBUTES is
      record
         nLength : DWORD;
         lpSecurityDescriptor : LPVOID;
         bInheritHandle : BOOL;
      end record;

   type PSECURITY_ATTRIBUTES is access all SECURITY_ATTRIBUTES;
   subtype LPSECURITY_ATTRIBUTES is PSECURITY_ATTRIBUTES;

   type Piped_process is record
     SI : aliased StartupInfo;
     PI : aliased Process_Information;
     SA : aliased Security_Attributes;
     PipeRead, PipeWrite : aliased HANDLE;
     ProcessObject : HANDLE := System.Null_Address;
     part_of_line: Ada.Strings.Unbounded.Unbounded_String;
   end record;

end Windows_pipes;