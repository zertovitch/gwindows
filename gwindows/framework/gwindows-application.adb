-----------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada Framework for Windows Development             --
--                                                                          --
--                 G W I N D O W S . A P P L I C A T I O N                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2024 David Botton                   --
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
--   https://sourceforge.net/projects/gnavi/                                --
--   https://github.com/zertovitch/gwindows                                 --
--                                                                          --
------------------------------------------------------------------------------

with GWindows.GStrings;
with GWindows.Internal;
with GWindows.Windows;
with GWindows.Constants;
with GWindows.Utilities;

with Ada.Strings.Wide_Fixed;
with Ada.Text_IO;
with GNAT.OS_Lib;
with System;
with Win32_Types;
with Ada.Unchecked_Conversion;

package body GWindows.Application is

   -------------------------------------------------------------------------
   --  Local Specs
   -------------------------------------------------------------------------

   --  Package-level callbacks for Win32 enumeration APIs.
   --  These must NOT be nested functions because GNAT generates
   --  stack trampolines for nested function access values, and
   --  Wine cannot execute code on the stack (NX protection).
   --  Captured state is passed through the LPARAM/dwData parameter.

   function Monitor_Enum_Callback
      (hMonitor :        GWindows.Types.Handle;
       hdc      :        GWindows.Types.Handle;
       lpRect   : access GWindows.Types.Rectangle_Type;
       dwData   :        GWindows.Types.Lparam)
      return Interfaces.C.int;
   pragma Convention (Stdcall, Monitor_Enum_Callback);
   pragma Machine_Attribute (Monitor_Enum_Callback, "ms_abi");

   type Enum_Child_Data is record
      Path : access GString_Unbounded;
   end record;

   function Enum_Child_Callback
      (child : GWindows.Types.Handle;
       lp    : GWindows.Types.Lparam)
      return Interfaces.C.int;
   pragma Convention (StdCall, Enum_Child_Callback);
   pragma Machine_Attribute (Enum_Child_Callback, "ms_abi");

   type POINTL is
      record
         x : Win32_Types.Long;
         y : Win32_Types.Long;
      end record;
   pragma Convention (C_PASS_BY_COPY, POINTL);
   --  Part of MSG

   type MSG is
      record
         hwnd    : GWindows.Types.Handle;
         message : Interfaces.C.int;
         wParam  : GWindows.Types.Wparam;
         lParam  : GWindows.Types.Lparam;
         time    : Win32_Types.Unsigned_Long;
         pt      : POINTL;
      end record;
   pragma Convention (C_PASS_BY_COPY, MSG);
   type Pointer_To_MSG is access all MSG;
   --  Win32 Message Loop Objet

   procedure Process_Message
     (Message :    Pointer_To_MSG;
      Window  : in GWindows.Base.Base_Window_Type'Class);
   --  Process a message for a message loop

   -------------------------------------------------------------------------
   --  Operating System Imports
   -------------------------------------------------------------------------

   function GetCurrentThreadId
     return Win32_Types.Unsigned_Long;
   pragma Import (StdCall, GetCurrentThreadId, "GetCurrentThreadId");
   pragma Machine_Attribute (GetCurrentThreadId, "ms_abi");

   WM_QUIT      : constant := 18;

   procedure PostThreadMessage
     (idThread : Win32_Types.Unsigned_Long;
      thr_msg  : Interfaces.C.unsigned;
      wParam   : GWindows.Types.Wparam := 0;
      lParam   : GWindows.Types.Lparam := 0);
   pragma Import (StdCall, PostThreadMessage,
                    "PostThreadMessage" & Character_Mode_Identifier);
   pragma Machine_Attribute (PostThreadMessage, "ms_abi");

   -------------------------------------------------------------------------
   --  Package-level callback implementations
   -------------------------------------------------------------------------

   function Monitor_Enum_Callback
      (hMonitor :        GWindows.Types.Handle;
       hdc      :        GWindows.Types.Handle;
       lpRect   : access GWindows.Types.Rectangle_Type;
       dwData   :        GWindows.Types.Lparam)
      return Interfaces.C.int
   is
      pragma Unreferenced (hMonitor, hdc);
      function To_Monitor_Dimensions is new Ada.Unchecked_Conversion
         (GWindows.Types.Lparam, Monitor_Dimensions);
      M : constant Monitor_Dimensions := To_Monitor_Dimensions (dwData);
   begin
      M (lpRect.all);
      return 1;  --  Continue enumeration (TRUE)
   end Monitor_Enum_Callback;

   -------------------------------------------------------------------------
   --  Package Body
   -------------------------------------------------------------------------

   ---------------
   -- hInstance --
   ---------------

   function hInstance return GWindows.Types.Handle is
   begin
      return GWindows.Internal.Current_hInstance;
   end hInstance;

   -------------------
   -- Set_hInstance --
   -------------------

   procedure Set_hInstance (Process_hInstance : GWindows.Types.Handle) is
   begin
      GWindows.Internal.Current_hInstance := Process_hInstance;
   end Set_hInstance;

   -----------------
   -- Load_String --
   -----------------

   function Load_String
     (ID       : in Interfaces.C.unsigned;
      Max_Size : in Integer := 256)
      return GString
   is
      C_Text : GString_C (0 .. Interfaces.C.size_t (Max_Size));

      function LoadString
        (hInst : in     GWindows.Types.Handle   := hInstance;
         uID   : in     Interfaces.C.unsigned   := ID;
         Buf   : access GChar_C                 := C_Text (0)'Access;
         Max   : in     Integer                 := Max_Size)
        return Integer;
      pragma Import (StdCall, LoadString,
                       "LoadString" & Character_Mode_Identifier);
      pragma Machine_Attribute (LoadString, "ms_abi");

      Result : constant Integer := LoadString;
   begin
      if Result = 0 then
         return "";
      else
         return GWindows.GStrings.To_GString_From_C (C_Text);
      end if;
   end Load_String;

   ------------------
   -- Message_Loop --
   ------------------

   procedure Message_Loop is
      Null_Window : GWindows.Base.Base_Window_Type;
   begin
      Modal_Loop (Null_Window);
   end Message_Loop;

   -------------------
   -- Message_Check --
   -------------------

   procedure Message_Check
   is
      use type Win32_Types.Long;

      PM_REMOVE   : constant := 1;

      function PeekMessage
        (lpMsg         : Pointer_To_MSG;
         hwnd          : GWindows.Types.Handle;
         wMsgFilterMin : Interfaces.C.unsigned;
         wMsgFilterMax : Interfaces.C.unsigned;
         RemoveMsg     : Interfaces.C.unsigned := PM_REMOVE)
        return Win32_Types.Long;
      pragma Import (StdCall, PeekMessage,
                       "PeekMessage" & Character_Mode_Identifier);
      pragma Machine_Attribute (PeekMessage, "ms_abi");

      tMSG        : aliased MSG;
      Null_Window : GWindows.Base.Base_Window_Type;
   begin
      if PeekMessage (tMSG'Unchecked_Access, GWindows.Types.Null_Handle,
                      0, 0) /= 0
      then
         Process_Message (tMSG'Unchecked_Access, Null_Window);
      end if;
   end Message_Check;

   ----------------
   -- Modal_Loop --
   ----------------

   procedure Modal_Loop (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use type Win32_Types.Long;

      function GetMessage
        (lpMsg         : Pointer_To_MSG;
         hwnd          : GWindows.Types.Handle;
         wMsgFilterMin : Interfaces.C.unsigned;
         wMsgFilterMax : Interfaces.C.unsigned)
        return Win32_Types.Long;
      pragma Import (StdCall, GetMessage,
                       "GetMessage" & Character_Mode_Identifier);
      pragma Machine_Attribute (GetMessage, "ms_abi");

      tMSG : aliased MSG;
   begin
      while GetMessage (tMSG'Unchecked_Access, GWindows.Types.Null_Handle,
                        0, 0) /= 0
      loop
         Process_Message (tMSG'Unchecked_Access, Window);
      end loop;
   end Modal_Loop;

   --------------
   -- End_Loop --
   --------------

   procedure End_Loop is
   begin
      PostThreadMessage (GetCurrentThreadId, WM_QUIT);
   end End_Loop;

   ---------------------
   -- End_Application --
   ---------------------

   procedure End_Application is
   begin
      PostThreadMessage (GWindows.Internal.Main_Thread_ID, WM_QUIT);
   end End_Application;

   -------------------
   -- Desktop_Width --
   -------------------

   function Desktop_Width return Natural is
   begin
      return GWindows.Internal.Desktop_Width;
   end Desktop_Width;

   --------------------
   -- Desktop_Height --
   --------------------

   function Desktop_Height return Natural is
   begin
      return GWindows.Internal.Desktop_Height;
   end Desktop_Height;

   --------------------------------
   -- Enumerate_Display_Monitors --
   --------------------------------

   procedure Enumerate_Display_Monitors (M : Monitor_Dimensions) is
      use GWindows.Types;

      type MONITORENUMPROC is access
         function
            (HMONITOR :        Handle;
             HDC      :        Handle;
             LPRECT   : access Rectangle_Type;
             mLPARAM  :        Lparam)
         return Interfaces.C.int;
      pragma Convention (Stdcall, MONITORENUMPROC);
      pragma Machine_Attribute (MONITORENUMPROC, "ms_abi");

      procedure EnumDisplayMonitors
         (hdc      : Handle          := Null_Handle;
          lprcClip : Wparam          := 0;
          lpfnEnum : MONITORENUMPROC := Monitor_Enum_Callback'Access;
          dwData   : Lparam          := 0);
      pragma Import (StdCall, EnumDisplayMonitors, "EnumDisplayMonitors");
      pragma Machine_Attribute (EnumDisplayMonitors, "ms_abi");

      function To_Lparam is new Ada.Unchecked_Conversion
         (Monitor_Dimensions, Lparam);
   begin
      EnumDisplayMonitors (dwData => To_Lparam (M));
   end Enumerate_Display_Monitors;

   -----------------------
   -- Screen_Visibility --
   -----------------------

   function Screen_Visibility
     (Left_Top_Corner : Types.Point_Type;
      Minimum_Width   : Positive := 200;
      Minimum_Height  : Positive := 50)
      return Screen_Visibility_Type
   is
      Result : Screen_Visibility_Type := Poor;

      procedure Check_Point_In_Monitor (Rectangle : Types.Rectangle_Type) is
         Is_In_Rectangle, Is_In_Restricted_Rectangle : Boolean;
      begin
         Is_In_Rectangle :=
            Left_Top_Corner.X in            Rectangle.Left .. Rectangle.Right
              and then Left_Top_Corner.Y in Rectangle.Top  .. Rectangle.Bottom;
         --
         Is_In_Restricted_Rectangle :=
            Is_In_Rectangle
              and then Left_Top_Corner.X <= Rectangle.Right - Minimum_Width
              and then Left_Top_Corner.Y <= Rectangle.Bottom - Minimum_Height;
         --
         --  Promotion of the visibility if the tested
         --  point is visible on the tested monitor.
         --
         if Is_In_Rectangle and Result = Poor then
            Result := Fair;
         end if;
         if Is_In_Restricted_Rectangle then
            Result := Good;
         end if;
      end Check_Point_In_Monitor;

   begin
      Enumerate_Display_Monitors (Check_Point_In_Monitor'Unrestricted_Access);
      return Result;
   end Screen_Visibility;

   -----------------------------
   -- Add_To_Recent_Documents --
   -----------------------------

   procedure Add_To_Recent_Documents (File_Name : GString) is

      procedure SHAddToRecentDocs
        (uFlags : Interfaces.C.int;
         pv     : Types.Handle);
      pragma Import (StdCall, SHAddToRecentDocs, "SHAddToRecentDocs");
   pragma Machine_Attribute (SHAddToRecentDocs, "ms_abi");

      SHARD_Value : constant GWindows.Utilities.ANSI_Unicode_Choice :=
        (ANSI => 2, Unicode => 3);
      File_Name_C : GString_C := GStrings.To_GString_C (File_Name);

   begin
      SHAddToRecentDocs
         (SHARD_Value (Character_Mode), Types.Handle (File_Name_C'Address));
   end Add_To_Recent_Documents;

   -------------------------
   -- Detach_From_Console --
   -------------------------

   procedure Detach_From_Console
   is
      procedure FreeConsole;
      pragma Import (StdCall, FreeConsole, "FreeConsole");
   pragma Machine_Attribute (FreeConsole, "ms_abi");
   begin
      FreeConsole;
   end Detach_From_Console;

   ---------------------------------------------------
   -- Get_Window_At_Location, and similar functions --
   ---------------------------------------------------

   type Static_Point_Type is
      record
         X, Y : Integer;
      end record;
   pragma Convention (C_Pass_By_Copy, Static_Point_Type);

   --  Internal
   function WindowFromPoint (Point : Static_Point_Type)
      return GWindows.Types.Handle;
   pragma Import (StdCall, WindowFromPoint, "WindowFromPoint");
   pragma Machine_Attribute (WindowFromPoint, "ms_abi");

   function Get_Window_At_Location
     (X, Y : Integer)
     return GWindows.Base.Pointer_To_Base_Window_Class
   is
      Win_Ptr : constant GWindows.Base.Pointer_To_Base_Window_Class :=
        GWindows.Base.Window_From_Handle (WindowFromPoint ((X, Y)));
   begin
      return Win_Ptr;
   end Get_Window_At_Location;

   --  Internal
   function Get_Window_Text (WH : GWindows.Types.Handle) return GString
   is
      function GetWindowTextLength (hwnd : GWindows.Types.Handle)
         return Integer;
      pragma Import (StdCall, GetWindowTextLength,
                       "GetWindowTextLength" & Character_Mode_Identifier);
      pragma Machine_Attribute (GetWindowTextLength, "ms_abi");
      procedure GetWindowText
        (hwnd : in     GWindows.Types.Handle;
         Text : access GChar_C;
         Max  : in     Interfaces.C.size_t);
      pragma Import (StdCall, GetWindowText,
                       "GetWindowText" & Character_Mode_Identifier);
      pragma Machine_Attribute (GetWindowText, "ms_abi");
      use GWindows.Types;
   begin
      if WH = Null_Handle then
         return "";
      else
         declare
            Buf : GString_C (1 .. Interfaces.C.size_t
                             (GetWindowTextLength (WH) + 1));
         begin
            GetWindowText (WH, Buf (Buf'First)'Access, Buf'Last);
            return GWindows.GStrings.To_GString_From_C (Buf);
         end;
      end if;
   end Get_Window_Text;

   function Get_Window_Text_At_Location (X, Y : Integer) return GString
   is
   begin
      return Get_Window_Text (WindowFromPoint ((X, Y)));
   end Get_Window_Text_At_Location;

   --  Internal
   function Get_Window_Class_Name (WH : GWindows.Types.Handle) return GString
   is
      procedure GetClassName
        (hwnd : in     GWindows.Types.Handle;
         Text : access GChar_C;
         Max  : in     Interfaces.C.size_t);
      pragma Import (StdCall, GetClassName,
                       "GetClassName" & Character_Mode_Identifier);
      pragma Machine_Attribute (GetClassName, "ms_abi");
      use GWindows.Types;
   begin
      if WH = Null_Handle then
         return "";
      else
         declare
            Buf : GString_C (1 .. 1024);
         begin
            GetClassName (WH, Buf (Buf'First)'Access, Buf'Last);
            return GWindows.GStrings.To_GString_From_C (Buf);
         end;
      end if;
   end Get_Window_Class_Name;

   function Get_Window_Class_Name_At_Location (X, Y : Integer)
      return GString
   is
   begin
      return Get_Window_Class_Name (WindowFromPoint ((X, Y)));
   end Get_Window_Class_Name_At_Location;

   --  GetAncestor is internal, just for obtaining the root window handle.
   function GetAncestor
      (hwnd    : GWindows.Types.Handle;
       gaFlags : Interfaces.C.unsigned)
   return GWindows.Types.Handle;
   pragma Import (StdCall, GetAncestor, "GetAncestor");
   pragma Machine_Attribute (GetAncestor, "ms_abi");
   GA_ROOT : constant := 2;

   function Get_Window_Root_Class_Name_At_Location (X, Y : Integer)
      return GString
   is
   begin
      return Get_Window_Class_Name
         (GetAncestor (WindowFromPoint ((X, Y)), GA_ROOT));
   end Get_Window_Root_Class_Name_At_Location;

   function Is_Desktop_At_Location (X, Y : Integer) return Boolean is
   begin
     return
       Get_Window_Root_Class_Name_At_Location (X, Y) = "Progman" and then
       Get_Window_Class_Name_At_Location (X, Y) = "SysListView32";
   end Is_Desktop_At_Location;

   function Enum_Child_Callback
      (child : GWindows.Types.Handle;
       lp    : GWindows.Types.Lparam)
      return Interfaces.C.int
   is
      use GWindows.GStrings;
      type Enum_Child_Data_Ptr is access all Enum_Child_Data;
      function To_Data_Ptr is new Ada.Unchecked_Conversion
         (GWindows.Types.Lparam, Enum_Child_Data_Ptr);
      Data : constant Enum_Child_Data_Ptr := To_Data_Ptr (lp);
      Child_Class_Name : constant GString := Get_Window_Class_Name (child);
      CT  : constant GString := Get_Window_Text (child);
      WCT : constant Wide_String := To_Wide_String (CT);
      is_candidate : Boolean := False;
      start_index : Integer;
      use Ada.Strings.Wide_Fixed;
   begin
      if Child_Class_Name = "ToolbarWindow32" then
         start_index := Index (WCT, ": ");
         is_candidate := start_index > 0;
         start_index := start_index + 2;
      elsif Child_Class_Name = "ShellTabWindowClass" then
         start_index := WCT'First;
         is_candidate := True;
      end if;
      if is_candidate then
         if Index (WCT, ":\") = 0 and then Index (WCT, "\\") = 0 then
            null;
         else
            Data.Path.all :=
               To_GString_Unbounded (CT (start_index .. CT'Last));
            return 0;  --  Found, stop enumeration (FALSE)
         end if;
      end if;
      return 1;  --  Continue enumeration (TRUE)
   end Enum_Child_Callback;

   function Explorer_Path_At_Location (X, Y : Integer) return GString is
      trace_mode : constant Boolean := False;
      --
      procedure EnumChildWindows (hwnd   : GWindows.Types.Handle;
                                  Proc   : System.Address;
                                  lp     : GWindows.Types.Lparam);
      pragma Import (StdCall, EnumChildWindows, "EnumChildWindows");
      pragma Machine_Attribute (EnumChildWindows, "ms_abi");
      --  NB: EnumChildWindows is recursive:
      --  "If a child window has created child windows of its own,
      --   EnumChildWindows enumerates those windows as well."
      Path : aliased GString_Unbounded;
      Data : aliased Enum_Child_Data := (Path => Path'Unchecked_Access);
      use GWindows.GStrings;
      --
      function To_Lparam is new Ada.Unchecked_Conversion
         (System.Address, GWindows.Types.Lparam);
      --
      RWH : constant GWindows.Types.Handle :=
                        GetAncestor (WindowFromPoint ((X, Y)), GA_ROOT);
      RCN : constant GString := Get_Window_Class_Name (RWH);
      use GNAT.OS_Lib;
      Env_Var : String_Access;
   begin
      if Is_Desktop_At_Location (X, Y) then
         --  NB: a probably cleaner way (regarding Unicode names)
         --  could be to use SHGetFolderPath with CSIDL_DESKTOPDIRECTORY
         Env_Var := Getenv ("USERPROFILE");
         if Env_Var = null or else Env_Var.all = "" then
            return "";
         else
            return To_GString_From_String (Env_Var.all & "\Desktop");
         end if;
      end if;
      if RCN = "CabinetWClass" or else RCN = "ExploreWClass" then
         --  This trick needs to have the Explorer option
         --  "Display the full path in the title bar" enabled,
         --  at least on Windows 11.
         if trace_mode then
           Ada.Text_IO.Put_Line (To_String (RCN) & " -> ENUM Explorer");
         end if;
         EnumChildWindows
            (RWH, Enum_Child_Callback'Address,
             To_Lparam (Data'Address));
         return To_GString_From_Unbounded (Path);
      end if;
      return "";
   end Explorer_Path_At_Location;

   -----------------------
   -- Get_Active_Window --
   -----------------------

   function Get_Active_Window
     return GWindows.Base.Pointer_To_Base_Window_Class
   is
      function GetActiveWindow return GWindows.Types.Handle;
      pragma Import (StdCall, GetActiveWindow, "GetActiveWindow");
   pragma Machine_Attribute (GetActiveWindow, "ms_abi");

      Win_Ptr : constant GWindows.Base.Pointer_To_Base_Window_Class :=
        GWindows.Base.Window_From_Handle (GetActiveWindow);
   begin
      return Win_Ptr;
   end Get_Active_Window;

   --------------------
   -- Display_Dialog --
   --------------------

   function Display_Dialog (Parent : in GWindows.Base.Base_Window_Type'Class;
                            Name   : in GString;
                            Center : in Boolean := True)
                           return Integer
   is
      use GWindows.Windows;

      Win_Ptr : constant GWindows.Base.Pointer_To_Base_Window_Class :=
        GWindows.Base.Window_From_Handle (GWindows.Base.Handle (Parent));

      Dialog : GWindows.Windows.Window_Type;
   begin
      Create_Dialog (Dialog, Win_Ptr.all, Name);
      if Center then
         GWindows.Windows.Center (Dialog, Parent);
      end if;
      Show_Dialog (Dialog, Win_Ptr.all);
      return Modal_Result (Dialog);
   end Display_Dialog;

   function Display_Dialog (Name   : in GString;
                            Center : in Boolean := True)
                           return Integer
   is
      use GWindows.Windows;

      Dialog : Window_Type;
   begin
      Create_Dialog (Dialog, Name);
      if Center then
         GWindows.Windows.Center (Dialog);
      end if;
      Show_Dialog (Dialog);
      return Modal_Result (Dialog);
   end Display_Dialog;

   procedure Display_Dialog
     (Parent : in out GWindows.Base.Base_Window_Type'Class;
      Name   : in     GString;
      Center : in     Boolean                              := True)
   is
      Dialog : constant Integer := Display_Dialog (Parent, Name, Center);
      pragma Warnings (Off, Dialog);
   begin
      null;
   end Display_Dialog;

   procedure Display_Dialog (Name   : in GString;
                             Center : in Boolean := True)
   is
      Dialog : constant Integer := Display_Dialog (Name, Center);
      pragma Warnings (Off, Dialog);
   begin
      null;
   end Display_Dialog;

   ----------------
   -- Show_Modal --
   ----------------

   procedure Show_Modal (Window : in out GWindows.Base.Base_Window_Type'Class;
                         Parent : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.Base;
      Real_Parent : Pointer_To_Base_Window_Class :=
        Controlling_Parent (Parent);
   begin
      if Real_Parent = null then
         Real_Parent := Parent'Unchecked_Access;
      end if;

      if GWindows.Base.Enabled (Real_Parent.all) then
         Is_Modal (Window, True, Real_Parent);
      else
         Is_Modal (Window, True, null);
      end if;

      Disable (Real_Parent.all);

      Visible (Window);

      Modal_Loop (Window);

      Focus (Parent);

      Is_Modal (Window, False, null);
   end Show_Modal;

   ----------------
   -- Show_Modal --
   ----------------

   procedure Show_Modal (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.Base;

      Null_Parent : Base_Window_Type;
   begin
      Show_Modal (Window, Null_Parent);
   end Show_Modal;

   -----------------
   -- Show_Dialog --
   -----------------

   procedure Show_Dialog (Window : in out GWindows.Base.Base_Window_Type'Class;
                          Parent : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.Base;
   begin
      Is_Dialog (Window, True);
      Modal_Result (Window, GWindows.Constants.IDCANCEL);

      Show_Modal (Window, Parent);

      Is_Dialog (Window, False);
   end Show_Dialog;

   -----------------
   -- Show_Dialog --
   -----------------

   procedure Show_Dialog
     (Window : in out GWindows.Base.Base_Window_Type'Class)
   is
      use GWindows.Base;

      Null_Parent : Base_Window_Type;
   begin
      Show_Dialog (Window, Null_Parent);
   end Show_Dialog;

   -----------------
   -- Show_Dialog --
   -----------------

   function Show_Dialog (Window : in GWindows.Base.Base_Window_Type'Class;
                         Parent : in GWindows.Base.Base_Window_Type'Class)
                        return Integer
   is
      use GWindows.Base;

      Win_Ptr    : constant Pointer_To_Base_Window_Class :=
        Window_From_Handle (Handle (Window));

      Parent_Ptr : constant Pointer_To_Base_Window_Class :=
        Window_From_Handle (Handle (Parent));
   begin
      Show_Dialog (Win_Ptr.all, Parent_Ptr.all);
      return Modal_Result (Window);
   end Show_Dialog;

   -----------------
   -- Show_Dialog --
   -----------------

   function Show_Dialog (Window : in GWindows.Base.Base_Window_Type'Class)
                        return Integer
   is
      use GWindows.Base;

      Win_Ptr : constant Pointer_To_Base_Window_Class :=
        Window_From_Handle (Handle (Window));
   begin
      Show_Dialog (Win_Ptr.all);
      return Modal_Result (Window);
   end Show_Dialog;

   -------------------------------------------------------------------------
   --  Local Body
   -------------------------------------------------------------------------

   ---------------------
   -- Process_Message --
   ---------------------

   procedure Process_Message
     (Message :    Pointer_To_MSG;
      Window  : in GWindows.Base.Base_Window_Type'Class)
   is
      use type GWindows.Types.Handle;
      use type GWindows.Internal.Pointer_To_Keyboard_Control;
      use type GWindows.Base.Base_Window_Access;

      function IsDialogMessage
        (hwnd  : GWindows.Types.Handle;
         lpMsg : Pointer_To_MSG)
        return Integer;
      pragma Import (StdCall, IsDialogMessage,
                       "IsDialogMessage" & Character_Mode_Identifier);
      pragma Machine_Attribute (IsDialogMessage, "ms_abi");

      function TranslateMDISysAccel
        (hwnd   : GWindows.Types.Handle;
         lpMsg  : Pointer_To_MSG)
        return Integer;
      pragma Import (StdCall, TranslateMDISysAccel, "TranslateMDISysAccel");
   pragma Machine_Attribute (TranslateMDISysAccel, "ms_abi");

      function TranslateAccelerator
        (hwnd   : GWindows.Types.Handle;
         haccel : GWindows.Types.Handle;
         lpMsg  : Pointer_To_MSG)
        return Integer;
      pragma Import (StdCall, TranslateAccelerator,
                       "TranslateAccelerator" & Character_Mode_Identifier);
      pragma Machine_Attribute (TranslateAccelerator, "ms_abi");

      procedure TranslateMessage
        (lpMsg : Pointer_To_MSG);
      pragma Import (StdCall, TranslateMessage, "TranslateMessage");
   pragma Machine_Attribute (TranslateMessage, "ms_abi");

      procedure DispatchMessage
        (lpMsg : Pointer_To_MSG);
      pragma Import (StdCall, DispatchMessage,
                       "DispatchMessage" & Character_Mode_Identifier);
      pragma Machine_Attribute (DispatchMessage, "ms_abi");

      Current_Keyboard_Control : GWindows.Internal.Pointer_To_Keyboard_Control;
      Processed                : Integer;
   begin
      Processed := 0;

      if GWindows.Base.Handle (Window) /= GWindows.Types.Null_Handle then
         Processed := IsDialogMessage
           (GWindows.Base.Handle (Window),
            Message);
      end if;

      if Processed = 0 then
         Current_Keyboard_Control := GWindows.Internal.Top_Keyboard_Control;
         while Current_Keyboard_Control /= null loop

            Processed := 0;

            if
              GWindows.Base.MDI_Client_Window
              (Current_Keyboard_Control.Window.all) /= null
            then
               Processed := TranslateMDISysAccel
                 (GWindows.Base.Handle
                  (GWindows.Base.MDI_Client_Window
                   (Current_Keyboard_Control.Window.all).all),
                  Message);

               if Processed /= 0 then
                  exit;
               end if;

            end if;

            if
              GWindows.Base.Accelerator_Handle
                (Current_Keyboard_Control.Window.all) /=
                GWindows.Types.Null_Handle
            then
               Processed := TranslateAccelerator
                 (GWindows.Base.Handle (Current_Keyboard_Control.Window.all),
                  GWindows.Base.Accelerator_Handle
                  (Current_Keyboard_Control.Window.all),
                  Message);

               if Processed /= 0 then
                  exit;
               end if;

            end if;

            if
              GWindows.Base.Keyboard_Support
              (Current_Keyboard_Control.Window.all)
            then
               Processed := IsDialogMessage
                 (GWindows.Base.Handle (Current_Keyboard_Control.Window.all),
                  Message);

               if Processed /= 0 then
                  exit;
               end if;
            end if;

            Current_Keyboard_Control := Current_Keyboard_Control.Next;
         end loop;
      end if;

      if Processed = 0 then
         TranslateMessage (Message);
         DispatchMessage (Message);
      end if;
   end Process_Message;

end GWindows.Application;
