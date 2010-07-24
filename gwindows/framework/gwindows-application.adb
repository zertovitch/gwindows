-----------------------------------------------------------------------------
--                                                                          --
--             GWINDOWS - Ada 95 Framework for Win32 Development            --
--                                                                          --
--                 G W I N D O W S . A P P L I C A T I O N                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                 Copyright (C) 1999 - 2005 David Botton                   --
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
-- be located on the web at http://www.gnavi.org/gwindows                   --
--                                                                          --
------------------------------------------------------------------------------

with GWindows.GStrings;
with GWindows.Internal;
with GWindows.Windows;
with GWindows.Constants;

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
     (Message :        Pointer_To_MSG;
      Window  : in out GWindows.Base.Base_Window_Type'Class);
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
      msg      : Interfaces.C.unsigned;
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

   procedure Set_hInstance (hInstance : GWindows.Types.Handle) is
   begin
      GWindows.Internal.Current_hInstance := hInstance;
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

   ----------------------------
   -- Get_Window_At_Location --
   ----------------------------

   function Get_Window_At_Location
     (X, Y : Integer)
     return GWindows.Base.Pointer_To_Base_Window_Class
   is
      type Static_Point_Type is
         record
            X, Y : Integer;
         end record;
      pragma Convention (C_Pass_By_Copy, Static_Point_Type);

      function WindowFromPoint
        (Point : Static_Point_Type := (X, Y))
        return GWindows.Types.Handle;
      pragma Import (StdCall, WindowFromPoint, "WindowFromPoint");

      Win_Ptr : constant GWindows.Base.Pointer_To_Base_Window_Class :=
        GWindows.Base.Window_From_Handle (WindowFromPoint);
   begin
      return Win_Ptr;
   end Get_Window_At_Location;

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
     (Message :        Pointer_To_MSG;
      Window  : in out GWindows.Base.Base_Window_Type'Class)
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
