-----------------------------------------------------------------------------
--                                                                          --
--            GWINDOWS - Ada 95 Framework for Windows Development           --
--                                                                          --
--                 G W I N D O W S . A P P L I C A T I O N                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2024 David Botton                   --
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

package body GWindows.Application is

   -------------------------------------------------------------------------
   --  Local Specs
   -------------------------------------------------------------------------

   type POINTL is
      record
         x : Interfaces.C.long;
         y : Interfaces.C.long;
      end record;
   pragma Convention (C_PASS_BY_COPY, POINTL);
   --  Part of MSG

   type MSG is
      record
         hwnd    : GWindows.Types.Handle;
         message : Interfaces.C.int;
         wParam  : GWindows.Types.Wparam;
         lParam  : GWindows.Types.Lparam;
         time    : Interfaces.C.unsigned_long;
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
     return Interfaces.C.unsigned_long;
   pragma Import (StdCall, GetCurrentThreadId, "GetCurrentThreadId");

   WM_QUIT      : constant := 18;

   procedure PostThreadMessage
     (idThread : Interfaces.C.unsigned_long;
      thr_msg  : Interfaces.C.unsigned;
      wParam   : GWindows.Types.Wparam := 0;
      lParam   : GWindows.Types.Lparam := 0);
   pragma Import (StdCall, PostThreadMessage,
                    "PostThreadMessage" & Character_Mode_Identifier);

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
      use type Interfaces.C.long;

      PM_REMOVE   : constant := 1;

      function PeekMessage
        (lpMsg         : Pointer_To_MSG;
         hwnd          : GWindows.Types.Handle;
         wMsgFilterMin : Interfaces.C.unsigned;
         wMsgFilterMax : Interfaces.C.unsigned;
         RemoveMsg     : Interfaces.C.unsigned := PM_REMOVE)
        return Interfaces.C.long;
      pragma Import (StdCall, PeekMessage,
                       "PeekMessage" & Character_Mode_Identifier);

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
      use type Interfaces.C.long;

      function GetMessage
        (lpMsg         : Pointer_To_MSG;
         hwnd          : GWindows.Types.Handle;
         wMsgFilterMin : Interfaces.C.unsigned;
         wMsgFilterMax : Interfaces.C.unsigned)
        return Interfaces.C.long;
      pragma Import (StdCall, GetMessage,
                       "GetMessage" & Character_Mode_Identifier);

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

      function Output_Monitor_Dims (
           HMONITOR :        Handle;
           HDC      :        Handle;
           LPRECT   : access Rectangle_Type;
           mLPARAM  :        Lparam
         ) return Boolean;
      pragma Convention (Stdcall, Output_Monitor_Dims);

      function Output_Monitor_Dims (
           HMONITOR :        Handle;
           HDC      :        Handle;
           LPRECT   : access Rectangle_Type;
           mLPARAM  :        Lparam
         ) return Boolean
      is
         pragma Unreferenced (HMONITOR, HDC, mLPARAM);
      begin
         M (LPRECT.all);
         return True;  --  Continue enumeration
      end Output_Monitor_Dims;

      type MONITORENUMPROC is access
         function (
           HMONITOR :        Handle;
           HDC      :        Handle;
           LPRECT   : access Rectangle_Type;
           mLPARAM  :        Lparam
         ) return Boolean;
      pragma Convention (Stdcall, MONITORENUMPROC);

      procedure EnumDisplayMonitors
         (hdc      : Handle          := Null_Handle;
          lprcClip : Wparam          := 0;
          lpfnEnum : MONITORENUMPROC := Output_Monitor_Dims'Access;
          dwData   : Lparam          := 0);
      pragma Import (StdCall, EnumDisplayMonitors, "EnumDisplayMonitors");
   begin
      EnumDisplayMonitors;
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
      procedure GetWindowText
        (hwnd : in     GWindows.Types.Handle;
         Text : access GChar_C;
         Max  : in     Interfaces.C.size_t);
      pragma Import (StdCall, GetWindowText,
                       "GetWindowText" & Character_Mode_Identifier);
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
   function GetAncestor (
     hwnd    : GWindows.Types.Handle;
     gaFlags : Interfaces.C.unsigned)
   return GWindows.Types.Handle;
   pragma Import (StdCall, GetAncestor, "GetAncestor");
   GA_ROOT : constant := 2;

   function Get_Window_Root_Class_Name_At_Location (X, Y : Integer)
      return GString
   is
   begin
      return Get_Window_Class_Name (
         GetAncestor (WindowFromPoint ((X, Y)), GA_ROOT)
      );
   end Get_Window_Root_Class_Name_At_Location;

   function Is_Desktop_At_Location (X, Y : Integer) return Boolean is
   begin
     return
       Get_Window_Root_Class_Name_At_Location (X, Y) = "Progman" and then
       Get_Window_Class_Name_At_Location (X, Y) = "SysListView32";
   end Is_Desktop_At_Location;

   function Explorer_Path_At_Location (X, Y : Integer) return GString is
      trace_mode : constant Boolean := False;
      --
      procedure EnumChildWindows (hwnd   : GWindows.Types.Handle;
                                  Proc   : System.Address;
                                  lp     : GWindows.Types.Lparam);
      pragma Import (StdCall, EnumChildWindows, "EnumChildWindows");
      --  NB: EnumChildWindows is recursive:
      --  "If a child window has created child windows of its own,
      --   EnumChildWindows enumerates those windows as well."
      Path : GString_Unbounded;
      use GWindows.GStrings;
      --
      function Capture_Edit_Box (child  : GWindows.Types.Handle;
                                 lp     : GWindows.Types.Lparam)
                         return Boolean;
      pragma Convention (StdCall, Capture_Edit_Box);
      function Capture_Edit_Box (child  : GWindows.Types.Handle;
                                 lp     : GWindows.Types.Lparam)
                         return Boolean
      is
      pragma Unreferenced (lp);
         Child_Class_Name : constant GString := Get_Window_Class_Name (child);
         CT  : constant GString := Get_Window_Text (child);
         --  Force to Unicode (for the Index function)
         WCT : constant Wide_String := To_Wide_String (CT);
         is_candidate : Boolean := False;
         start_index : Integer;
         use Ada.Strings.Wide_Fixed;
      begin
         --  List everything:
         if trace_mode then
           Ada.Text_IO.Put_Line (To_String ("    " & Child_Class_Name & ": " & CT));
         end if;
         if Child_Class_Name = "ToolbarWindow32" then
            --  Windows 95 to Windows 10
            ----------------------------
            --  There are many children with the class name
            --  ToolbarWindow32! Only one may contain a path.
            --  Examples of such strings (Windows 10 in French, accents removed):
            --    "Boutons de navigation"
            --    "Barre d'outils du bandeau superieur"
            --    "Adresse: C:\Ada\gnavi\gwindows\samples"
            start_index := Index (WCT, ": ");
            is_candidate := start_index > 0;
            start_index := start_index + 2;
         elsif Child_Class_Name = "ShellTabWindowClass" then
            --  Windows 11.
            start_index := WCT'First;
            is_candidate := True;
         end if;
         if is_candidate then
            if Index (WCT, ":\") = 0 and then Index (WCT, "\\") = 0 then
               --  It is a bogus directory, like "My Computer"
               --  or "Documents" (in various languages...).
               null;
            else
               Path := To_GString_Unbounded (CT (start_index .. CT'Last));
               if trace_mode then
                  Ada.Text_IO.Put_Line
                    ('[' & To_String (To_GString_From_Unbounded (Path)) & ']');
               end if;
               return False;  --  Found, then stop enumeration
            end if;
         end if;
         --  Recurse on children (not needed: EnumChildWindows is recursive):
         --  EnumChildWindows (child, Capture_Edit_Box'Address, 0);
         return True;  --  Continue enumeration
      end Capture_Edit_Box;
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
         EnumChildWindows (RWH, Capture_Edit_Box'Address, 0);
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
         GWindows.Windows.Center (Dialog);
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

      function TranslateMDISysAccel
        (hwnd   : GWindows.Types.Handle;
         lpMsg  : Pointer_To_MSG)
        return Integer;
      pragma Import (StdCall, TranslateMDISysAccel, "TranslateMDISysAccel");

      function TranslateAccelerator
        (hwnd   : GWindows.Types.Handle;
         haccel : GWindows.Types.Handle;
         lpMsg  : Pointer_To_MSG)
        return Integer;
      pragma Import (StdCall, TranslateAccelerator,
                       "TranslateAccelerator" & Character_Mode_Identifier);

      procedure TranslateMessage
        (lpMsg : Pointer_To_MSG);
      pragma Import (StdCall, TranslateMessage, "TranslateMessage");

      procedure DispatchMessage
        (lpMsg : Pointer_To_MSG);
      pragma Import (StdCall, DispatchMessage,
                       "DispatchMessage" & Character_Mode_Identifier);

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
