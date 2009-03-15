-- Demonstrates using GWindows in a DLL

package GW_In_A_DLL is

   procedure Pop_A_Window;
   pragma Export (C, Pop_A_Window, "pop_a_window");

end GW_In_A_DLL;
