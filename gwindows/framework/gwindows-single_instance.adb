------------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada Framework for Windows Development             --
--                                                                          --
--              G W I N D O W S . S I N G L E _ I N S T A N C E             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 2024 Nicolas Pinault                       --
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

with GWindows.Application,
     GWindows.Base,
     GWindows.Errors,
     GWindows.GStrings,
     GWindows.Types;

with Ada.Command_Line,
     Ada.Streams,
     Ada.Unchecked_Conversion;

with Interfaces.C;

with System.Storage_Elements;

package body GWindows.Single_Instance is

  IPC_BUFFER_SIZE : constant := 1024;

  WM_COPYDATA : constant := 74;

  IDI_APPLICATION : constant := 32512;

  type COPY_DATA_STRUCT is record
    dwData : Types.DWORD_PTR;  --  User defined value
    cbData : Types.DWORD;      --  Size of Data
    lpData : System.Address;   --  Data pointer
  end record;

  type COPY_DATA_STRUCT_PTR is access all COPY_DATA_STRUCT;

  COPY_DATA_CMD_LINE : constant := 1;

  type BOOL is new Interfaces.C.int;

  procedure Register_Class_Name (Application_Class_Name : GString);
  function Is_First_Instance (Application_Instance_Name : GString)
    return Boolean;
  function Find_Window (Class_Name : GString) return Types.Handle;
  function Is_Zoomed (hWnd : Types.Handle) return Boolean;
  function Is_Iconic (hWnd : Types.Handle) return Boolean;
  procedure Set_Foreground_Window (hWnd : Types.Handle);
  procedure Show_Window (hWnd : Types.Handle; Cmd : Integer);

  function Application_Custom_WndProc
    (hwnd    : Types.Handle;
     message : Interfaces.C.unsigned;
     wParam  : Types.Wparam;
     lParam  : Types.Lparam)
     return Types.Lresult;
  pragma Convention (Stdcall, Application_Custom_WndProc);

  ----------------------------------------------------------------------
  --  Streams writing/reading to/from a bounded memory block          --
  --                                                                  --
  --  A stream can't be used simultaneously for writing and reading.  --
  ----------------------------------------------------------------------
  --
  package Memory_Streams is

     use Ada.Streams;

     type Memory_Stream is new Ada.Streams.Root_Stream_Type with private;

     overriding procedure Read (Stream : in out Memory_Stream;
                                Item   :    out Stream_Element_Array;
                                Last   :    out Stream_Element_Offset);

     overriding procedure Write (Stream : in out Memory_Stream;
                                 Item   :        Stream_Element_Array);

     procedure Set_Address (Stream  : in out Memory_Stream;
                            Address :        System.Address;
                            Length  :        Natural);

  private

    type Memory_Stream is new Ada.Streams.Root_Stream_Type with record
      Address        : System.Address;
      Last_Offset    : Stream_Element_Offset := 0;
      Current_Offset : Stream_Element_Offset := 1;
    end record;

  end Memory_Streams;

  package body Memory_Streams is

    --  ********************************************************************
    overriding procedure Read (Stream : in out Memory_Stream;
                               Item   :    out Stream_Element_Array;
                               Last   :    out Stream_Element_Offset)
    is
      Source : Stream_Element_Array (1 .. Stream.Last_Offset)
        with Address => Stream.Address;
      pragma Import (Ada, Source);
      --  ^ This pragma prevents an attempt of initialization (possibly brought
      --    by the configuration pragma Initialize_Scalars (in file debug.pra)
      --    or a validity check in Debug mode) and solves following error:
      --        error: invalid address clause for initialized object "Source"
      --        error: reference to variable "Stream" not allowed (RM 13.1(22))

      Next_Offset : constant Stream_Element_Offset :=
        Stream.Current_Offset + Item'Length;
    begin
      Item := Source (Stream.Current_Offset .. Next_Offset - 1);
      Last := Item'Last;
      Stream.Current_Offset := Next_Offset;
    end Read;

    --  ********************************************************************
    overriding procedure Write (Stream : in out Memory_Stream;
                                Item   :        Stream_Element_Array)
    is
      Target : Stream_Element_Array (1 .. Stream.Last_Offset)
        with Address => Stream.Address;
      pragma Import (Ada, Target);
      Next_Offset : constant Stream_Element_Offset :=
        Stream.Current_Offset + Item'Length;
    begin
      Target (Stream.Current_Offset .. Next_Offset - 1) := Item;
      Stream.Current_Offset := Next_Offset;
    end Write;

    --  ********************************************************************
    procedure Set_Address (Stream  : in out Memory_Stream;
                           Address :        System.Address;
                           Length  :        Natural)
    is
    begin
      Stream.Address        := Address;
      Stream.Last_Offset    := Stream_Element_Offset (Length);
      Stream.Current_Offset := Stream_Element_Offset (1);
    end Set_Address;

  begin

    if Stream_Element_Array'Component_Size /= System.Storage_Unit then
      raise Program_Error;
    end if;

  end Memory_Streams;

  --  **********************************************************************
  procedure Manage_Single_Instance
    (Application_Class_Name    : in     GString;
     Application_Instance_Name : in     GString;
     Exit_Requested            :    out Boolean)
  is
    First_Instance : constant Boolean :=
      Is_First_Instance (Application_Instance_Name);

    procedure Search_and_Activate_Foreign_Instance is
      use type Types.Handle;
      Window_Handle : Types.Handle;

      procedure Activate_Foreign_Instance is
        SW_MAXIMIZE : constant := 3;
        SW_RESTORE  : constant := 9;
        sw : Integer := 0;
      begin
        --  Restore the window, bring it to front, etc,
        if Is_Zoomed (Window_Handle) then
          sw := SW_MAXIMIZE;
        elsif Is_Iconic (Window_Handle) then
          sw := SW_RESTORE;
        end if;
        if sw /= 0 then
          Show_Window (Window_Handle, sw);
        end if;
        Set_Foreground_Window (Window_Handle);

        --  Transfer the command line arguments to the first instance
        if Ada.Command_Line.Argument_Count > 0 then
          declare
            Copy_Data : COPY_DATA_STRUCT;

            procedure SendMessage (hwnd   : Types.Handle;
                                   uMsg   : Interfaces.C.int;
                                   wParam : Types.Wparam;
                                   lParam : access COPY_DATA_STRUCT);
            pragma Import
              (StdCall,
               SendMessage,
               "SendMessage" & Character_Mode_Identifier);
            Stream : aliased Memory_Streams.Memory_Stream;
            Buffer :
              System.Storage_Elements.Storage_Array (1 .. IPC_BUFFER_SIZE) :=
                (others => 0);

          begin
            Memory_Streams.Set_Address
              (Stream, Buffer (1)'Address, Buffer'Length);

            Natural'Output (Stream'Access, Ada.Command_Line.Argument_Count);
            for i in 1 .. Ada.Command_Line.Argument_Count loop
              String'Output (Stream'Access, Ada.Command_Line.Argument (i));
            end loop;

            Copy_Data.dwData := COPY_DATA_CMD_LINE;
            Copy_Data.cbData := Buffer'Length;
            Copy_Data.lpData := Buffer (1)'Address;

            SendMessage (Window_Handle,
                         WM_COPYDATA,
                         0,
                         Copy_Data'Unrestricted_Access);
          end;
        end if;

        Exit_Requested := True;
      end Activate_Foreign_Instance;

      Nb_Retry : Natural := 15;

    begin
      loop
        Window_Handle := Find_Window (Application_Class_Name);
        exit when Window_Handle /= Types.Null_Handle;
        Nb_Retry := Nb_Retry - 1;
        exit when Nb_Retry = 0;
        delay 0.1;
      end loop;

      if Window_Handle /= Types.Null_Handle then
        Activate_Foreign_Instance;
      end if;
    end Search_and_Activate_Foreign_Instance;

  begin
    Exit_Requested := False;

    Register_Class_Name (Application_Class_Name);

    if not First_Instance then
      Search_and_Activate_Foreign_Instance;
    end if;
  end Manage_Single_Instance;

  --  **********************************************************************
  procedure Register_Class_Name (Application_Class_Name : GString) is
      function LoadIcon
        (hInstance  : Types.Handle := Types.Null_Handle;
         lpIconName : Integer := IDI_APPLICATION)
        return Types.Handle;
      pragma Import
        (StdCall,
         LoadIcon,
         "LoadIcon" & Character_Mode_Identifier);

      Window_Class        : Base.WNDCLASS;
      Window_Class_Name_C : constant
         GString_C := GStrings.To_GString_C (Application_Class_Name);
  begin
      Window_Class.hInstance     := Application.hInstance;
      Window_Class.hIcon         := LoadIcon;
      Window_Class.lpszClassName :=
        Window_Class_Name_C (Window_Class_Name_C'First)'Unrestricted_Access;
      Window_Class.lpfnWndProc   := Application_Custom_WndProc'Address;
      Base.Register_Class (Window_Class);
  end Register_Class_Name;

  --  **********************************************************************
  --  Returns True if this application instance is the first to be running.
  function Is_First_Instance (Application_Instance_Name : GString)
  return Boolean
  is

    type BOOL is new Interfaces.C.int;

    function CreateMutex
      (MutexAttributes : System.Address := System.Null_Address;
       InitialOwner    : BOOL           := 0;  --  False
       Name            : GString_C)
    return Types.Handle;
    pragma Import
      (StdCall,
       CreateMutex,
       "CreateMutex" & Character_Mode_Identifier);

    Error        : Integer;
    Mutex_Handle : Types.Handle;
    Mutex_Name   : constant GString_C :=
       GStrings.To_GString_C (Application_Instance_Name);
    pragma Unreferenced (Mutex_Handle);

    ERROR_ALREADY_EXISTS : constant := 183;

  begin
    --  If the Mutex creation fails, then another instance of this
    --  application is already running.
    Mutex_Handle := CreateMutex (Name => Mutex_Name);
    Error := Errors.Get_Last_Error;
    return not (Error = ERROR_ALREADY_EXISTS);
  end Is_First_Instance;

  --  **********************************************************************
  --  Returns the handle of the first instance's window of
  --  the application, or NULL.
  function Find_Window (Class_Name : GString) return Types.Handle is

    function FindWindow (ClassName  : GString_C;
                         WindowName : System.Address := System.Null_Address)
                        return Types.Handle;
    pragma Import (StdCall, FindWindow,
                   "FindWindow" & Character_Mode_Identifier);

    ClassName  : constant GString_C :=
      GStrings.To_GString_C (Class_Name);
  begin
    return FindWindow (ClassName);
  end Find_Window;

  --  **********************************************************************
  function Is_Zoomed (hWnd : Types.Handle) return Boolean is
    function IsZoomed (hWnd : Types.Handle) return BOOL;
    pragma Import (StdCall, IsZoomed, "IsZoomed");
  begin
    return IsZoomed (hWnd) /= 0;
  end Is_Zoomed;

  --  **********************************************************************
  function Is_Iconic (hWnd : Types.Handle) return Boolean is
    function IsIconic (hWnd : Types.Handle) return BOOL;
    pragma Import (StdCall, IsIconic, "IsIconic");
  begin
    return IsIconic (hWnd) /= 0;
  end Is_Iconic;

  --  **********************************************************************
  procedure Set_Foreground_Window (hWnd : Types.Handle) is
    procedure SetForegroundWindow (hWnd : Types.Handle);
    pragma Import (StdCall, SetForegroundWindow, "SetForegroundWindow");
  begin
    SetForegroundWindow (hWnd);
  end Set_Foreground_Window;

  --  **********************************************************************
  procedure Show_Window (hWnd : Types.Handle;
                         Cmd  : Integer)
  is
    procedure ShowWindow (hwnd     : Types.Handle;
                          nCmdShow : Interfaces.C.long);
    pragma Import (StdCall, ShowWindow, "ShowWindow");
  begin
    ShowWindow (hWnd, Interfaces.C.long (Cmd));
  end Show_Window;

  --  ************************************************************************
  function Application_Custom_WndProc
    (hwnd    : Types.Handle;
     message : Interfaces.C.unsigned;
     wParam  : Types.Wparam;
     lParam  : Types.Lparam)
     return Types.Lresult
  is

    function Process_Arguments_From_Newer_Instance return Types.Lresult is

      function To_COPY_DATA is
        new Ada.Unchecked_Conversion (Types.Lparam, COPY_DATA_STRUCT_PTR);

      Copy_Data : constant COPY_DATA_STRUCT_PTR := To_COPY_DATA (lParam);
    begin
      case Copy_Data.dwData is
        when COPY_DATA_CMD_LINE =>
          declare
            Stream : aliased Memory_Streams.Memory_Stream;
            Nb_Params : Integer;
          begin
            Memory_Streams.Set_Address
              (Stream, Copy_Data.lpData, IPC_BUFFER_SIZE);
            Nb_Params := Natural'Input (Stream'Access);
            for i in 1 .. Nb_Params loop
              Process_Argument (i, Nb_Params, String'Input (Stream'Access));
            end loop;
          end;
          return 1;

        when others =>
          return 0;
      end case;
    end Process_Arguments_From_Newer_Instance;

  begin
    case message is
      when WM_COPYDATA =>
        return Process_Arguments_From_Newer_Instance;
      when others =>
        return Base.WndProc (hwnd    => hwnd,
                             message => message,
                             wParam  => wParam,
                             lParam  => lParam);
    end case;
  end Application_Custom_WndProc;

end GWindows.Single_Instance;
