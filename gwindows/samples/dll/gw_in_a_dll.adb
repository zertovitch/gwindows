-- Demonstrates using GWindows in a DLL

with Interfaces.C;

with GWindows.Application;
with GWindows.Windows;

package body GW_In_A_DLL is

   DLL_PROCESS_DETACH : constant := 0;
   DLL_PROCESS_ATTACH : constant := 1;

   procedure Adainit;
   pragma Import (C, Adainit);

   procedure Adafinal;
   pragma Import (C, Adafinal);

   function DllMain
     (hinstDLL    : Interfaces.C.long;
      fdwReason   : Interfaces.C.unsigned_short;
      lpvReserved : Integer)
     return Interfaces.C.int;
   pragma Export (StdCall, DllMain, "DllMain");

   -------------
   -- DllMain --
   -------------

   function DllMain
     (hinstDLL    : Interfaces.C.long;
      fdwReason   : Interfaces.C.unsigned_short;
      lpvReserved : Integer)
     return Interfaces.C.int
   is
      pragma Warnings (Off, lpvReserved);
   begin
      case fdwReason is
         when DLL_PROCESS_ATTACH =>
            GWindows.Application.Set_hInstance (HinstDLL);
            return 1;
         when DLL_PROCESS_DETACH =>
            Adafinal;
            return 1;
         when others =>
            return 1;
      end case;
   end DllMain;

   ------------------
   -- Pop_A_Window --
   ------------------

   procedure Pop_A_Window is
      use GWindows.Windows;

      Window : Window_Type;
   begin
      Adainit;
      --  Adainit is called outside of DllMain to avoid
      --  a Win32 "feature", that if a thread is created in
      --  the dllmain, it must exit before dllmain can return.
      --
      --  In our small example this does not matter, but if
      --  there were any tasks that elaborate at start up
      --  we would be in trouble.

      Create (Window, "Hello Form a DLL", Width => 100, Height => 100);
      Visible (Window);

      GWindows.Application.Show_Modal (Window);
   end Pop_A_Window;

end GW_In_A_DLL;
