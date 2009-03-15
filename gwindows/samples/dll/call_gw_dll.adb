procedure Call_GW_DLL is
   pragma Linker_Options ("-lgw_in_a_dll");

   procedure Pop_A_Window;
   pragma Import (C, Pop_A_Window, "pop_a_window");

begin
   Pop_A_Window;
end Call_GW_DLL;
