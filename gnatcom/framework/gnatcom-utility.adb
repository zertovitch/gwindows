------------------------------------------------------------------------------
--                                                                          --
--      GNATCOM - Ada 95 COM/DCOM/COM+ Development Framework and Tools      --
--                                                                          --
--                     G N A T C O M . U T I L I T Y                        --
--                                                                          --
--                                B o d y                                   --
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
-- More information about GNATCOM and the most current version can          --
-- be located on the web at http://www.gnavi.org/gnatcom                    --
--                                                                          --
------------------------------------------------------------------------------

with System;

package body GNATCOM.Utility is

   WM_QUIT : constant := 18;

   type POINTL is
      record
         x : Interfaces.C.long;
         y : Interfaces.C.long;
      end record;
   pragma Convention (C_PASS_BY_COPY, POINTL);

   type MSG is
      record
         hwnd     : System.Address;
         message  : Interfaces.C.int;
         wParam   : Interfaces.C.int;
         lParam   : Interfaces.C.long;
         time     : Interfaces.C.unsigned_long;
         pt       : POINTL;
         lPrivate : Interfaces.C.unsigned_long;
      end record;
   pragma Convention (C_PASS_BY_COPY, MSG);
   type Pointer_To_MSG is access all MSG;

   function GetMessage
     (lpMsg         : Pointer_To_MSG;
      hwnd          : System.Address;
      wMsgFilterMin : Interfaces.C.unsigned;
      wMsgFilterMax : Interfaces.C.unsigned)
     return Interfaces.C.long;
   pragma Import (StdCall, GetMessage, "GetMessageA");

   procedure DispatchMessage
     (lpMsg : Pointer_To_MSG);
   pragma Import (StdCall, DispatchMessage, "DispatchMessageA");

   function GetCurrentThreadId
     return Interfaces.C.unsigned_long;
   pragma Import (StdCall, GetCurrentThreadId, "GetCurrentThreadId");

   procedure PostThreadMessage
     (idThread : Interfaces.C.unsigned_long;
      MSG_Kind : Interfaces.C.unsigned;
      wParam   : Interfaces.C.unsigned      := 0;
      lParam   : Interfaces.C.long          := 0);
   pragma Import (StdCall, PostThreadMessage, "PostThreadMessageA");

   procedure MessageBox
     (hwnd    : in System.Address := System.Null_Address;
      Message : in Interfaces.C.char_array;
      Title   : in Interfaces.C.char_array;
      uType   : in Interfaces.C.unsigned   := 0);
   pragma Import (StdCall, MessageBox, "MessageBoxA");

   ---------------------------
   -- Get_Current_Thread_ID --
   ---------------------------

   function Get_Current_Thread_ID return Interfaces.C.unsigned_long
   is
   begin
      return GetCurrentThreadId;
   end Get_Current_Thread_ID;

   ------------------
   -- Message_Loop --
   ------------------

   procedure Message_Loop is
      use type Interfaces.C.long;

      tMSG    : aliased MSG;
   begin
      while GetMessage (tMSG'Unchecked_Access,
                        System.Null_Address, 0, 0) /= 0 loop
         DispatchMessage (tMSG'Unchecked_Access);
      end loop;
   end Message_Loop;

   ---------------
   -- Post_Quit --
   ---------------

   procedure Post_Quit (Thread_ID : Interfaces.C.unsigned_long)
   is
   begin
      PostThreadMessage (Thread_ID, WM_QUIT);
   end Post_Quit;

   ---------------
   -- Post_Quit --
   ---------------

   procedure Post_Quit is
   begin
      Post_Quit (GetCurrentThreadId);
   end Post_Quit;

   -----------------
   -- Message_Box --
   -----------------

   procedure Message_Box (Title, Message : String) is
      BoxTitle    : constant Interfaces.C.char_array :=
        Interfaces.C.To_C (Title);
      BoxMessage  : constant Interfaces.C.char_array :=
        Interfaces.C.To_C (Message);
   begin
      MessageBox (Message => BoxMessage,
                  Title   => BoxTitle);
   end Message_Box;

end GNATCOM.Utility;
