with Spin_Browse;

procedure Spin is
   pragma Linker_Options ("-mwindows");
   pragma Linker_Options ("spin.coff");

begin
   Spin_Browse.Go;
end Spin;
